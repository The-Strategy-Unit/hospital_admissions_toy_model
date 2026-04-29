#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(bslib)
library(readxl)
library(ggplot2)
library(StrategyUnitTheme)
library(plotly)

options(scipen=999)

# Data-------------------------------------------------------------------------

data<-read_excel("data/Collating the data.xlsx")
from_93<-data[7:38,]
  
# Functions--------------------------------------------------------------------

## Solve for admissions
solve_for_admissions<-function(admissions,LoS,occupancy,beds){
  return(abs(beds - qpois(occupancy, admissions*LoS/365)))
}


## Generate model outputs FUTURE ADMISSIONS
future_admissions_model<-function(from_93, beds_yearly_percentage_change, los_yearly_percentage_change, occupancy_fixed_level){
  
  years<-c(from_93$Year,2026,2027,2028,2029,2030,2031,2032,2033,2034,2035)
  admissions<-from_93$`All admissions`
  beddays<-from_93$`All beddays`
  beds<-from_93$Beds
  los<-from_93$avgLoS
  occupancy<-from_93$`occupancy`
  
  for(i in 1:(length(years)-length(from_93$Year))){
    ## adds one entry to each array during each loop
    los <- c(los , los[length(los)] + los[length(los)]*los_yearly_percentage_change/100 )
    beddays <- c(beddays , admissions[length(admissions)]*los[length(los)])
    ## Using MMinf as model to work out # beds to reach set performance
    beds <- c(beds, beds[length(beds)] + beds[length(beds)]*beds_yearly_percentage_change/100)
    occupancy<- c(occupancy, (occupancy_fixed_level)/100)
    # now solve for admissions
    admissions <- c(admissions,optim(par = c(18742360),fn = solve_for_admissions  , LoS = los[length(los)], occupancy = occupancy[length(occupancy)], beds = beds[length(beds)], method="Brent",,lower=15000000,upper=26000000)$par)
  }
  
  plot_data<-data.frame(
    year = as.numeric(unlist(years)), 
    admissions = (unlist(admissions))/1000000,
    los = unlist(los), 
    beds = unlist(beds),
    occupancy = (unlist(occupancy))*100
  )
  
  return(plot_data)
  
}


## Generate model outputs FUTURE BEDS

future_beds_model<-function(from_93, admissions_yearly_percentage_change, los_yearly_percentage_change, occupancy_fixed_level){
  
  years<-c(from_93$Year,2026,2027,2028,2029,2030,2031,2032,2033,2034,2035)
  admissions<-from_93$`All admissions`
  beddays<-from_93$`All beddays`
  beds<-from_93$Beds
  los<-from_93$avgLoS
  occupancy<-from_93$`occupancy`
  
  for(i in 1:(length(years)-length(from_93$Year))){
    admissions <- c(admissions,admissions[length(admissions)] +  admissions[length(admissions)] *admissions_yearly_percentage_change/100)
    los <- c(los , los[length(los)] + los[length(los)]*los_yearly_percentage_change/100 )
    beddays <- c(beddays , admissions[length(admissions)]*los[length(los)])
    beds <- c(beds, qpois((occupancy_fixed_level/100), admissions[length(admissions)]*los[length(los)]/365))
    occupancy<- c(occupancy, (occupancy_fixed_level/100))
  }
  
  plot_data<-data.frame(
    year = as.numeric(unlist(years)), 
    admissions = (unlist(admissions))/1000000,
    los = unlist(los), 
    beds = unlist(beds),
    occupancy = (unlist(occupancy))*100
  )
  
  return(plot_data)
  
}

## Generate plots
plotting_function<-function(plot_data, output_type, y_axis_label){
  
  
  p<-ggplot(data=plot_data, aes(x=year, y=.data[[output_type]], text = paste0("Year: ", year, "<br>", y_axis_label, ": ", round(.data[[output_type]],1) )))+
    geom_line(data=scenario_1_values, aes(x=year, y=.data[[output_type]], text = paste0("Scenario 1<br>","Year: ", year, "<br>", y_axis_label, ": ", round((.data[[output_type]]),1 ))), colour= "#b2b7b9" , linewidth=0.8, , linetype="dashed", group=1)+
    geom_line(data=scenario_2_values, aes(x=year, y=.data[[output_type]], text = paste0("Scenario 2<br>","Year: ", year, "<br>", y_axis_label, ": ", round((.data[[output_type]]),1 ))), colour= "#b2b7b9" , linewidth=0.8, , linetype="dashed",group=1)+
    geom_line(colour=ifelse(as.numeric(plot_data$year) <= 2026,  "black", "#ec6555"), linewidth=0.8, linetype=ifelse(as.numeric(plot_data$year)<= 2026,  "solid", "dashed"), group=1)+
    geom_vline(xintercept=2026, colour="#ec6555", linetype="dotted")+
    su_theme()+
    labs(y=y_axis_label,
         x="Year")+
    scale_y_continuous(limits=c(0,NA))
  
  ggplotly(p, tooltip = "text")
  
  
}


baseline_beds<-1000

#UI ----------------------------------------------------------------------------
# Define UI for application that draws a histogram
ui <- page_navbar(
  title = "Hospital Admission Model",
  id = "nav",
  bg = "#f9bf07",
  theme = bs_theme(
    bootswatch = "united",
    dark = "black",
    primary = "#686f73",
    secondary = "#f9bf07" 
  ),

#Adding a logo to top right ----------------------------------------------------
  tags$style(HTML("
    .top-panel {
      position: fixed;
      top: 6px;
      right: 15px;
      z-index: 9999;
    }

    .logo img {
      height: 50px;
    }
  ")),
  
  tags$div(
    class = "top-panel",
    tags$div(
      class = "logo",
      tags$img(src = "tsu_logo_black.png")
    )
  ),

## Explainer ---------------------------------------------------------  
nav_panel(
  "Explainer",
  div(
    style = "max-width: 1280px; margin: 0 auto;",
    
    div(
      style = "
        background-color: #f9bf07;
        border: 2px solid #2c2825;
        padding: 32px;
        margin: 16px 0 20px 0;
      ",
      h2("Hospital Admissions Analysis Tool"),
      p("This interactive tool helps you explore NHS hospital admissions data from 1994-2025 and project future scenarios through 2035 based on customisable assumptions about admissions growth, length of stay, and bed occupancy rates.")
    ),
    
### Cards -----------------------------------------------------------------------
    
    card(
      card_header("What This Tool Does"),
      div(
        style = "display: flex; gap: 16px; padding: 16px;",
        tags$span(
          style = "font-size: 24px; color: #5881c1;",
          HTML("&#9432;")
        ),
        div(
          p("This tool analyses historical NHS hospital admissions data and projects future scenarios based on your assumptions. It helps you understand:"),
          tags$ul(
            tags$li("How admissions have changed over the past 30+ years (1994-2025)"),
            tags$li("How different growth rates affect future bed requirements"),
            tags$li("The relationship between admissions, length of stay, and bed capacity"),
            tags$li("What occupancy rates are sustainable given different scenarios")
          )
        )
      )
    ),
    
    card(
      card_header("Toy Models"),
      div(
        style = "display: flex; gap: 16px; padding: 16px;",
        tags$span(
          style = "font-size: 24px; color: #5881c1;",
          HTML("&#9432;")
        ),
        div(
          p("This is one of a series of toy models developed by the Strategy Unit to support decision-making, strategic planning and also a teaching aid for testing and applying complex theories. Other toy models in the series included or being developed are:"),
          tags$ul(
            tags$li("How might risk stratification save money?"),
            tags$li("Bed pooling and occupancy"),
            tags$li("Bottlenecks and patient flow"),
            tags$li("'Sharing nicely'"),
            tags$li("Waiting list dynamics")
          )
        )
      )
    ),
    
    card(
      card_header("Understanding the Data"),
      div(
        style = "padding: 16px;",
        layout_columns(
          div(
            style = "
              border: 2px solid #5881c1;
              padding: 16px;
              background-color: #ffffff;
            ",
            h4("Historical Data (1994-2025)"),
            p(HTML("Shown as <b>solid lines</b> on all charts. This is real NHS data showing:")),
            tags$ul(
              tags$li(HTML("<b>Admissions:</b> Annual hospital admissions")),
              tags$li(HTML("<b>Length of Stay (LoS):</b> Average days patients spend in hospital")),
              tags$li(HTML("<b>Beddays:</b> Total days of care provided")),
              tags$li(HTML("<b>Beds:</b> Number of available hospital beds")),
              tags$li(HTML("<b>Occupancy:</b> Percentage of beds in use"))
            )
          ),
          
          div(
            style = "
              border: 2px solid #ec6555;
              padding: 16px;
              background-color: #ffffff;
            ",
            h4("Projected Data (2026-2035)"),
            p(HTML("Shown as <b>dashed lines</b> on all charts. These projections are calculated based on the assumptions you set in the sidebar:")),
            tags$ul(
              tags$li(HTML("<b>Admissions growth:</b> Annual % increase in admissions")),
              tags$li(HTML("<b>LoS change:</b> Annual % change in length of stay")),
              tags$li(HTML("<b>Target occupancy:</b> Desired bed occupancy rate"))
            ),
            p(em("A vertical dashed line marks the transition from historical to projected data."))
          ),
          
          col_widths = c(6, 6)
        )
      )
    ),
    
    card(
      card_header("How to Use This Tool"),
      div(
        style = "padding: 16px;",
        
        div(
          style = "display: flex; gap: 16px; margin-bottom: 16px;",
          tags$span(
            style = "font-size: 24px; color: #f9bf07;",
            HTML("&#9881;")
          ),
          div(
            style = "width: 100%;",
            h4("Step 1: Adjust Assumptions (Sidebar)"),
            p("Use the sliders in the sidebar to set your assumptions:"),
            
            div(
              style = "
                background-color: #f4f4f4;
                border: 1px solid #686f73;
                padding: 10px;
                margin-bottom: 10px;
              ",
              p(HTML("<b>Admissions growth:</b> -2% to +5% annually")),
              p(style = "color: #686f73; margin-bottom: 0;", "Example: +2% means admissions increase by 2% each year from 2026-2035")
            ),
            
            div(
              style = "
                background-color: #f4f4f4;
                border: 1px solid #686f73;
                padding: 10px;
                margin-bottom: 10px;
              ",
              p(HTML("<b>LoS change:</b> -3% to +2% annually")),
              p(style = "color: #686f73; margin-bottom: 0;", "Example: -1% means average length of stay decreases by 1% each year")
            ),
            
            div(
              style = "
                background-color: #f4f4f4;
                border: 1px solid #686f73;
                padding: 10px;
              ",
              p(HTML("<b>Target occupancy:</b> 75% to 95%")),
              p(style = "color: #686f73; margin-bottom: 0;", "The bed occupancy rate you want to maintain (85% is often considered optimal)")
            )
          )
        ),
        
        hr(),
        
        div(
          style = "display: flex; gap: 16px; margin-bottom: 16px;",
          tags$span(
            style = "font-size: 24px; color: #5881c1;",
            HTML("&#8599;")
          ),
          div(
            h4("Step 2: Explore the Future Beds Calculator Tab"),
            p("View charts showing historical trends and your projected scenarios. The charts update in real-time as you adjust the sliders. Pay attention to:"),
            tags$ul(
              tags$li("How your assumptions affect the trajectory of each metric"),
              tags$li(HTML('The "Key Metrics (2035 Projected)" section showing end-state values')),
              tags$li("Whether projected occupancy rates remain sustainable")
            )
          )
        ),
        
        hr(),
        
        div(
          style = "display: flex; gap: 16px;",
          tags$span(
            style = "font-size: 24px; color: #ec6555;",
            HTML("&#128425;")
          ),
          div(
            h4("Step 3: Use the Fixed Bed Solver (Optional)"),
            p('The "Fixed Bed Solver" tab lets you work backwards: specify a fixed number of beds and see what combinations of admissions growth and LoS changes would maintain your target occupancy rate.'),
            p(em("This is useful for planning scenarios where bed capacity is constrained."))
          )
        )
      )
    ),
    
    card(
      card_header("Data Source & Methodology"),
      div(
        style = "padding: 16px;",
        p("Historical data (1994-2025) is sourced from NHS England. Projections are calculated by applying your chosen growth rates to the last historical year (2025) and extending through 2035."),
        p(HTML('For detailed information about data sources, calculation methods, and assumptions, visit the <b>"Assumptions and Method"</b> tab.'))
      )
    ),
    
    div(
      style = "
        border: 2px solid #5881c1;
        background-color: #eef4fb;
        padding: 18px;
        margin: 20px 0;
      ",
      h4("Ready to Get Started?"),
      p(HTML('Click on the <b>"Future beds calculator"</b> tab to begin exploring the data. Use the sidebar sliders to adjust your assumptions and watch how the projections change in real-time.'))
    )
  )
),

##Future beds nav panel ---------------------------------------------------------  
nav_panel(
  "Future beds calculator",
  class = "panel-one",
  
  layout_sidebar(
    sidebar = sidebar(
      title = "Future Assumptions",
      id = "sidebar_future_beds",
      width = "250px",
      
      p(
        "Adjust how you think each input might change over the next 10 years",
        style = "font-size: 0.85rem; line-height: 1.2;"
      ),
      
      hr(style = "margin: 10px 0;"),
      
      h5("ADMISSIONS", style = "font-size: 1rem; margin-bottom: 8px;"),
      
      layout_columns(
        actionButton("scenario_1", "Scenario 1", style = "font-size: 0.8rem; padding: 6px;"),
        actionButton("scenario_2", "Scenario 2", style = "font-size: 0.8rem; padding: 6px;"),
        actionButton("custom_scenario", "Custom", style = "font-size: 0.8rem; padding: 6px;"),
        col_widths = c(4, 4, 4)
      ),
      
      div(
        style = "display:flex; justify-content:space-between; margin-top: 10px;",
        strong("Annual Change"),
        span("0%")
      ),
      
      sliderInput(
        "admissions_change",
        label = NULL,
        min = -10,
        max = 10,
        value = 0,
        step = 0.1
      ),
      
      div(
        style = "display:flex; justify-content:space-between; font-size:0.8rem;",
        span("-10%"),
        span("0%"),
        span("+10%")
      ),
      
      div(
        style = "display:flex; justify-content:space-between; font-size:0.75rem; margin-top:8px;",
        span("2026: 16967214"),
        span("2035: 16967214")
      ),
      
      hr(style = "margin: 12px 0;"),
      
      h5("LENGTH OF STAY", style = "font-size: 1rem; margin-bottom: 8px;"),
      
      div(
        style = "display:flex; justify-content:space-between;",
        strong("Annual Change"),
        span("0%")
      ),
      
      sliderInput(
        "los_change",
        label = NULL,
        min = -10,
        max = 10,
        value = 0,
        step = 0.1
      ),
      
      div(
        style = "display:flex; justify-content:space-between; font-size:0.8rem;",
        span("-10%"),
        span("0%"),
        span("+10%")
      ),
      
      div(
        style = "display:flex; justify-content:space-between; font-size:0.75rem; margin-top:8px;",
        span("2026: 3.12 days"),
        span("2035: 3.12 days")
      ),
      
      hr(style = "margin: 12px 0;"),
      
      h5("TARGET OCCUPANCY", style = "font-size: 1rem; margin-bottom: 8px;"),
      
      div(
        style = "display:flex; justify-content:space-between;",
        strong("Fixed Value"),
        span("80%")
      ),
      
      sliderInput(
        "target_occupancy",
        label = NULL,
        min = 0,
        max = 100,
        value = 80,
        step = 0.1
      ),
      
      div(
        style = "display:flex; justify-content:space-between; font-size:0.8rem;",
        span("0%"),
        span("100%")
      ),
      
      actionButton(
        "reset_scenario", 
        "↻ Reset to Baseline", 
        class = "btn-primary w-100",
        style = "margin-top: 10px;"
      ),
      
      div(
        style = "
          border: 1px solid #f9bf07;
          background-color: #fff8e1;
          padding: 8px;
          margin-top: 10px;
          font-size: 0.75rem;
          line-height: 1.2;
        ",
        "Use the sliders to model how admissions, LoS, and occupancy might change annually."
      )
    ),
    
    div(
      style = "padding-left: 10px;",
      
      h3("Admissions Analysis", style = "margin-bottom: 4px;"),
      hr(style = "margin: 4px 0 10px 0;"),
      
      div(
        style = "
          border: 2px solid #f9bf07;
          background-color: #fff8e1;
          padding: 8px 10px;
          margin-bottom: 8px;
          font-size: 0.85rem;
          line-height: 1.25;
        ",
        div(HTML("<b>Historical data (1994-2025)</b> shown in solid lines. <b>Predicted data (2026-2035)</b> shown in dashed lines, based on assumptions in the sidebar.")),
        div(HTML("<b>What this is:</b> A headline view of admissions, LoS, beddays, beds required and occupancy over time.")),
        div(HTML("<b>Why it is useful:</b> Shows how changing assumptions affects future pressure on beds.")),
        div(HTML("<b>How to use it:</b> Adjust the sidebar inputs and compare the projected direction and 2035 values."))
      ),
      
      layout_columns(
        card(
          card_header(HTML("Admissions &nbsp; &#9432;")),
          div(style = "height: 170px;")
        ),
        
        card(
          card_header(HTML("Length of Stay &nbsp; &#9432;")),
          div(style = "height: 170px;")
        ),
        
        card(
          card_header(HTML("Beds Required &nbsp; &#9432;")),
          div(style = "height: 170px;")
        ),
        
        card(
          card_header(HTML("Occupancy Rate &nbsp; &#9432;")),
          div(style = "height: 170px;")
        ),
        
        col_widths = c(6, 6, 6, 6)
      ),
      
      card(
        card_header("Key Metrics (2035 Projected)"),
        
        div(
          style = "
            border: 2px solid #f9bf07;
            background-color: #fff8e1;
            padding: 8px 10px;
            margin: 10px;
            font-size: 0.8rem;
            line-height: 1.25;
          ",
          "These are the modelled 2035 values based on the assumptions set in the sidebar. They start from the 2025 baseline and apply the changes each year from 2026 to 2035, so small differences compound over time."
        ),
        
        layout_columns(
          card(
            style = "padding: 8px;",
            div("Admissions", style = "font-size: 0.8rem;"),
            strong("16.97 million")
          ),
          
          card(
            style = "padding: 8px;",
            div("LoS", style = "font-size: 0.8rem;"),
            strong("3.12 days")
          ),
          
          card(
            style = "padding: 8px;",
            div("Beds", style = "font-size: 0.8rem;"),
            strong("181.4 thousand")
          ),
          
          card(
            style = "padding: 8px;",
            div("Occupancy", style = "font-size: 0.8rem;"),
            strong("80.0 %")
          ),
          
          col_widths = c(3, 3, 3, 3)
        )
      )
    )
  )
),

##Future Admissions Calculator nav panel ---------------------------------------
nav_panel(
  "Future Admissions Calculator",
  
  layout_sidebar(
    sidebar = sidebar(
      title = "Future Admissions Calculator",
      id = "sidebar_fixed_beds",
      width = "250px",
      
      p(
        "Set a fixed bed supply and test what admissions and LoS combinations would be sustainable.",
        style = "font-size: 0.85rem; line-height: 1.2;"
      ),
      
      hr(style = "margin: 10px 0;"),
      
      h5("BED SUPPLY", style = "font-size: 1rem; margin-bottom: 8px;"),
      
      p("Baseline (2025):", baseline_beds, style = "font-size: 0.85rem;"),
      
      div(
        style = "display:flex; justify-content:space-between;",
        strong("Fixed beds"),
        span("1000")
      ),
      
      sliderInput(
        "fixed_beds",
        label = NULL,
        min = 0,
        max = 250000,
        value = baseline_beds,
        step = 100
      ),
      
      div(
        style = "display:flex; justify-content:space-between; font-size:0.8rem;",
        span("0"),
        span("250,000")
      ),
      
      hr(style = "margin: 12px 0;"),
      
      h5("LENGTH OF STAY", style = "font-size: 1rem; margin-bottom: 8px;"),
      
      div(
        style = "display:flex; justify-content:space-between;",
        strong("Annual Change"),
        span("0%")
      ),
      
      sliderInput(
        "fixed_los_change",
        label = NULL,
        min = -10,
        max = 10,
        value = 0,
        step = 0.1
      ),
      
      div(
        style = "display:flex; justify-content:space-between; font-size:0.8rem;",
        span("-10%"),
        span("0%"),
        span("+10%")
      ),
      
      hr(style = "margin: 12px 0;"),
      
      h5("TARGET OCCUPANCY", style = "font-size: 1rem; margin-bottom: 8px;"),
      
      div(
        style = "display:flex; justify-content:space-between;",
        strong("Fixed Value"),
        span("85%")
      ),
      
      sliderInput(
        "fixed_target_occupancy",
        label = NULL,
        min = 0,
        max = 100,
        value = 85,
        step = 0.5
      ),
      
      div(
        style = "display:flex; justify-content:space-between; font-size:0.8rem;",
        span("0%"),
        span("100%")
      ),
      
      actionButton(
        "reset_baseline", 
        "↻ Reset to Baseline", 
        class = "btn-primary w-100",
        style = "margin-top: 10px;"
      ),
      
      div(
        style = "
          border: 1px solid #f9bf07;
          background-color: #fff8e1;
          padding: 8px;
          margin-top: 10px;
          font-size: 0.75rem;
          line-height: 1.2;
        ",
        "Use this panel to work backwards from a fixed bed supply. Admissions capacity is calculated from the selected beds, LoS and target occupancy assumptions."
      )
    ),
    
    div(
      style = "padding-left: 10px;",
      
      h3("Future Admissions Calculator", style = "margin-bottom: 4px;"),
      hr(style = "margin: 4px 0 10px 0;"),
      
      div(
        style = "
          border: 2px solid #f9bf07;
          background-color: #fff8e1;
          padding: 8px 10px;
          margin-bottom: 8px;
          font-size: 0.85rem;
          line-height: 1.25;
        ",
        div(HTML("<b>What this is:</b> A backwards-looking planning view that starts with a fixed number of beds and estimates what level of admissions could be supported.")),
        div(HTML("<b>Why it is useful:</b> Helps test capacity-constrained scenarios where bed numbers are fixed or cannot grow enough to match demand.")),
        div(HTML("<b>How to use it:</b> Adjust fixed beds, LoS and target occupancy in the sidebar, then compare the resulting projected admissions capacity and bed pressure over time."))
      ),
      
      layout_columns(
        card(
          card_header(HTML("Supported Admissions &nbsp; &#9432;")),
          div(style = "height: 170px;")
        ),
        
        card(
          card_header(HTML("Length of Stay &nbsp; &#9432;")),
          div(style = "height: 170px;")
        ),
        
        card(
          card_header(HTML("Fixed Beds &nbsp; &#9432;")),
          div(style = "height: 170px;")
        ),
        
        card(
          card_header(HTML("Occupancy Rate &nbsp; &#9432;")),
          div(style = "height: 170px;")
        ),
        
        col_widths = c(6, 6, 6, 6)
      ),
      
      card(
        card_header("Key Metrics (2035 Projected)"),
        
        div(
          style = "
            border: 2px solid #f9bf07;
            background-color: #fff8e1;
            padding: 8px 10px;
            margin: 10px;
            font-size: 0.8rem;
            line-height: 1.25;
          ",
          "These are the modelled 2035 values based on the fixed bed assumptions set in the sidebar. They show the admissions capacity that could be supported under the selected LoS and occupancy assumptions."
        ),
        
        layout_columns(
          card(
            style = "padding: 8px;",
            div("Supported admissions", style = "font-size: 0.8rem;"),
            strong("Add value")
          ),
          
          card(
            style = "padding: 8px;",
            div("LoS", style = "font-size: 0.8rem;"),
            strong("Add value")
          ),
          
          card(
            style = "padding: 8px;",
            div("Fixed beds", style = "font-size: 0.8rem;"),
            strong("Add value")
          ),
          
          card(
            style = "padding: 8px;",
            div("Occupancy", style = "font-size: 0.8rem;"),
            strong("Add value")
          ),
          
          col_widths = c(3, 3, 3, 3)
        )
      )
    )
  )
),


## Assumptions and Methods -----------------------------------------------------
nav_panel(
  "Assumptions and Method",
  
  div(
    style = "max-width: 1280px; margin: 0 auto;",
    
    card(
      card_header("Model Overview"),
      div(
        style = "padding: 16px;",
        p("This tool models the relationship between bed capacity, patient admissions, length of stay (LoS), and occupancy rates."),
        
        div(
          style = "
            border: 2px solid #5881c1;
            background-color: #f4f4f4;
            padding: 12px;
            margin: 10px 0 16px 0;
            font-family: monospace;
          ",
          "Add in correct calculation"
        ),
        
        p("Parameters:"),
        tags$ul(
          tags$li("Admissions: Patients per week"),
          tags$li("Length of Stay: Average days in hospital"),
          tags$li("Beds: Total capacity"),
          tags$li("Occupancy: % of beds occupied")
        )
      )
    ),
    
    layout_columns(
      card(
        card_header("Data and Definitions"),
        div(
          style = "padding: 16px;",
          div(
            style = "
              border: 1px solid #686f73;
              padding: 12px;
              margin-bottom: 10px;
            ",
            p("Default Values"),
            p("• Beds Baseline (2025): 145687 beds"),
            p("• Admissions: 150/week"),
            p("• LoS: 5.5 days"),
            p("• Target: 85%")
          ),
          
          div(
            style = "
              border: 1px solid #686f73;
              padding: 12px;
            ",
            p("Time Period"),
            p("Weekly admissions")
          )
        )
      ),
      
      card(
        card_header("Assumptions and Limitations"),
        div(
          style = "padding: 16px;",
          div(
            style = "
              border: 1px solid #686f73;
              min-height: 18px;
              margin-bottom: 10px;
            "
          ),
          p(em("For strategic planning only, not operational decisions."))
        )
      ),
      
      col_widths = c(6, 6)
    ),
    
    card(
      card_header("References"),
      div(
        style = "padding: 16px;",
        p("• NHS England: Bed Occupancy Guidance"),
        p("• Strategy Unit: Capacity and Demand Modelling"),
        p("• The Nuffield Trust: Hospital Bed Numbers")
      )
    )
  )
)

)

#Server interface --------------------------------------------------------------

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  observeEvent(input$preset, {
    if (input$preset == "Scenario 1") {
      updateSliderInput(session, "admissions_change", value = 1)
      updateSliderInput(session, "los_change", value = -2)
      updateSliderInput(session, "target_occupancy", value = 85)
    } else if (input$preset == "Scenario 2") {
      updateSliderInput(session, "admissions_change", value = 5)
      updateSliderInput(session, "los_change", value = -5)
      updateSliderInput(session, "target_occupancy", value = 90)
    }
  })
  
  
  observeEvent(input$reset_scenario, {
    if (input$preset == "Scenario 1") {
      updateSliderInput(session, "admissions_change", value = 1)
      updateSliderInput(session, "los_change", value = -2)
      updateSliderInput(session, "target_occupancy", value = 85)
    } else if (input$preset == "Scenario 2") {
      updateSliderInput(session, "admissions_change", value = 5)
      updateSliderInput(session, "los_change", value = -5)
      updateSliderInput(session, "target_occupancy", value = 90)
    }
  })
  
  observeEvent(input$reset_baseline, {
    updateSliderInput(session, "bedday_growth", value = 0) 
    updateSliderInput(session, "los_days", value = 5.2) 
    updateSliderInput(session, "bed_occupancy", value = 85) 
  })
  
  output$TEST <- renderText(
    paste("TEST:", input$target_occupancy)
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
