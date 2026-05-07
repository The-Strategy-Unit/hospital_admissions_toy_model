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
  
  if(occupancy_fixed_level==100){
    occupancy_fixed<-99.999999999999
  }else{ 
    occupancy_fixed<-occupancy_fixed_level}
  
  for(i in 1:(length(years)-length(from_93$Year))){
    ## adds one entry to each array during each loop
    los <- c(los , los[length(los)] + los[length(los)]*los_yearly_percentage_change/100 )
    beddays <- c(beddays , admissions[length(admissions)]*los[length(los)])
    ## Using MMinf as model to work out # beds to reach set performance
    beds <- c(beds, beds[length(beds)] + beds[length(beds)]*beds_yearly_percentage_change/100)
    occupancy<- c(occupancy, (occupancy_fixed/100))
    # now solve for admissions
    admissions <- c(admissions,optim(par = c(18742360),fn = solve_for_admissions  , LoS = los[length(los)], occupancy = occupancy[length(occupancy)], beds = beds[length(beds)], method="Brent",lower=15000000,upper=26000000)$par)
  }
  
  plot_data<-data.frame(
    year = as.numeric(unlist(years)), 
    admissions = (unlist(admissions))/1000000,
    los = unlist(los), 
    beds = round(unlist(beds),0),
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
  
if(occupancy_fixed_level==100){
  occupancy_fixed<-99.999999999999
}else{ 
  occupancy_fixed<-occupancy_fixed_level}
  
  
  for(i in 1:(length(years)-length(from_93$Year))){
    admissions <- c(admissions,admissions[length(admissions)] +  admissions[length(admissions)] *admissions_yearly_percentage_change/100)
    los <- c(los , los[length(los)] + los[length(los)]*los_yearly_percentage_change/100 )
    beddays <- c(beddays , admissions[length(admissions)]*los[length(los)])
    beds <- c(beds, qpois((occupancy_fixed/100), admissions[length(admissions)]*los[length(los)]/365))
    occupancy<- c(occupancy, (occupancy_fixed/100))
  }
  
  plot_data<-data.frame(
    year = as.numeric(unlist(years)), 
    admissions = (unlist(admissions))/1000000,
    los = unlist(los), 
    beds = round(unlist(beds),0),
    occupancy = (unlist(occupancy))*100
  )
  
  return(plot_data)
  
}

## Plotting function
plotting_function <- function(plot_data, scenario_1_values, scenario_2_values, scenario_3_values, output_type, y_axis_label, y_axis_max = NULL) {
  
  if(is.null(y_axis_max)){
    y_axis_max <- max(plot_data[[output_type]], na.rm = TRUE) * 1.05
  }
  
  # Initial plot
  p <- ggplot(data = plot_data, aes(x = year, y = .data[[output_type]], 
                                    text = paste0("Year: ", year, "<br>", y_axis_label, ": ", round(.data[[output_type]], 1)))) +
    geom_vline(xintercept = 2025, colour = "#5881c1" , linetype = "dashed") +
    su_theme() +
    labs(title=NULL, subtitle=NULL, y = y_axis_label, x = "Year") +
    scale_y_continuous(limits = c(0, y_axis_max))
  
  # Add Scenario plots if needed
  if (!is.null(scenario_1_values)) {
    p <- p + geom_line(data = scenario_1_values, 
                       aes(x = year, y = .data[[output_type]], 
                           text = paste0("Do nothing<br>", "Year: ", year, "<br>", y_axis_label, ": ", round((.data[[output_type]]), 1))), 
                       colour = "#b2b7b9", linewidth = 0.5, linetype = "dotted", group = 1)+ 
      geom_line(data = scenario_2_values, 
                aes(x = year, y = .data[[output_type]], 
                    text = paste0("Planned<br>", "Year: ", year, "<br>", y_axis_label, ": ", round((.data[[output_type]]), 1))), 
                colour = "#b2b7b9", linewidth = 0.5, linetype = "dotted", group = 1)+ 
      geom_line(data = scenario_3_values, 
                aes(x = year, y = .data[[output_type]], 
                    text = paste0("Ambitious<br>", "Year: ", year, "<br>", y_axis_label, ": ", round((.data[[output_type]]), 1))), 
                colour = "#b2b7b9", linewidth = 0.5, linetype = "dotted", group = 1)
  }
  
  # Add the main line last so it sits on top
  p <- p +     geom_line(data = subset(plot_data, as.numeric(year) >= 2025),
                         aes(x = year, y = .data[[output_type]]),
                         colour = "#ec6555", 
                         linewidth = 0.5, 
                         linetype = "dotted", 
                         group = 1)+
    geom_line(data = subset(plot_data, as.numeric(year) <= 2025),
              aes(x = year, y = .data[[output_type]]),
              colour = "black", 
              linewidth = 0.5, 
              linetype = "solid", 
              group = 1)
  
  
  ggplotly(p, tooltip = "text")|>
    layout(
      margin = list(t = 20, b = 10, l = 55, r =10), # Top, Bottom, Left, Right
      pad = 0
    )
}

baseline_beds<-1000


#UI ----------------------------------------------------------------------------
##UI General
ui <- page_navbar(
  title = "Hospital Admission Tool",
  position = "fixed-top", 
  id = "nav",
  bg = "#2c2825",
  theme = bs_theme(
    bootswatch = "united",
    primary = "#686f73",
    secondary = "#f9bf07"
  ),
  
  # Styling --------------------------------------------------------------------
  tags$style(HTML("
      /* navbar */
    .navbar {
      min-height: 62px;
      padding-top: 6px;
      padding-bottom: 6px;
    }
    

    /* Alignment of title + tabs */
    .navbar-brand,
    .navbar-nav > li > a {
      padding-top: 10px !important;
      padding-bottom: 10px !important;
      line-height: 1.2;
    }

    /* Top right buttons + logo */
    .top-panel {
      position: fixed;
      top: 8px;
      right: 15px;
      z-index: 9999;
      display: flex;
      align-items: center;
      gap: 10px;
    }

    /* Buttons */
    .top-panel-button {
      border: 1px solid #ffffff;
      color: #ffffff;
      background-color: #2c2825;
      padding: 5px 10px;
      font-size: 0.78rem;
      font-weight: 600;
      text-decoration: none;
      display: inline-flex;
      align-items: center;
      gap: 6px;
      border-radius: 4px;
    }

    .top-panel-button:hover {
      background-color: #3a3632;
      color: #ffffff;
      text-decoration: none;
    }

    /* Logo */
    .logo img {
      height: 52px;
    }
    
    .toy-model-links ul li a {
    color: #0d6efd;
    text-decoration: underline;
    }

    .toy-model-links ul li a:hover {
    color: #0a58ca;
    }
  ")),
  
  tags$head(
    tags$style(HTML("
    body { 
      padding-top: 60px !important; 
    }
  "))
  ),
  
  # Top-right buttons + logo ---------------------------------------------------
  tags$div(
    class = "top-panel",
    
  #  tags$a(
   #   href = "#",
   #   class = "top-panel-button",
 #     HTML("&#9432; About")
  #  ),
    
    tags$a(
      href = "mailto:strategy.unit@nhs.net?subject=Hospital Admission Model query",
      class = "top-panel-button",
      HTML("&#9993; Contact")
    ),
    
    tags$div(
      class = "logo",
      tags$img(src = "tsu_logo_yellow_screen_transparent.png")
    )
  ),

## Panel 1: Explainer ---------------------------------------------------------
  nav_panel(
    "Explainer",
    
    div(
      style = "max-width: 1400px; margin: 0 auto;",
      
      # Top banner 
      div(
        style = "
        background-color: #f9bf07;
        border: 2px solid #2c2825;
        padding: 16px 22px;
        margin: 10px 0 10px 0;
      ",
        h2("Hospital Admissions Analysis Tool", style = "margin: 0 0 6px 0;"),
        p(
          "This interactive tool helps you explore future NHS hospital admissions or bed requirements based on customisable assumptions about changes in admissions/available beds, length of stay, and bed occupancy.",
          style = "margin: 0;"
        )
      ),
      
      ### Side-by-side cards 
      layout_columns(
        
        card(
          style = "margin-bottom: 10px; height: 100%;",
          card_header("What This Tool Does"),
          card_body(
            style = "padding: 10px 12px 8px 12px;",
            div(
              style = "display: flex; gap: 10px; align-items: flex-start;",
              tags$span(
                style = "font-size: 20px; color: #5881c1; line-height: 1.1;"
              ),
              div(
                p(
                  "This tool uses historic NHS inpatient data and projects future scenarios based on your assumptions. It helps you understand:",
                  style = "margin: 0 0 6px 0;"
                ),
                tags$ul(
                  style = "margin: 0; padding-left: 20px;",
                  tags$li("How admissions, available beds, average length of stay and bed occupancy have changed over the past 30+ years (1994–2025)."),
                  tags$li("How changes in admissions could affect future bed requirements (Future Beds Calculator)."),
                  tags$li("How many admissions could be supported by future growth in bed capacity (Future Admissions Calculator)."),
                  tags$li("Relationship between admissions, bed capacity, length of stay and bed occupancy.")
                  
                )
              )
            )
          )
        ),
        
        card(
          class = "toy-model-links",
          style = "margin-bottom: 10px; height: 100%;",
          card_header("Toy Models"),
          card_body(
            style = "padding: 10px 12px 8px 12px;",
            div(
              style = "display: flex; gap: 10px; align-items: flex-start;",
              tags$span(
                style = "font-size: 20px; color: #5881c1; line-height: 1.1;"),
              div(
                p(
                  "This is one of a series of toy models developed by the Strategy Unit to support decision-making, strategic planning and as a teaching aid.",
                  style = "margin: 0 0 6px 0;"),
                
                tags$ul(
                  style = "margin: 0; padding-left: 20px;",
                  
                  tags$li(
                    tags$a(
                      "How might risk stratification save money?",
                      href = "https://connect.strategyunitwm.nhs.uk/risk_stratification_tool/",
                      target = "_blank"
                    )),
                  
                  tags$li(
                    tags$a(
                      "Bed pooling and occupancy",
                      href = "https://connect.strategyunitwm.nhs.uk/bed_pool_tool/",
                      target = "_blank"
                    )),
                  
                  tags$li(
                    tags$a(
                      "Bottlenecks and patient flow",
                      href = "https://exchange.iseesystems.com/public/sally-thompson/flows-and-bottlenecks-toy-model/index.html",
                      target = "_blank"
                    )),
                  
                  tags$li("'Sharing nicely'"),
                  tags$li("Waiting list dynamics")
                ))))),
        
        col_widths = c(8, 4)
      ),
    
      ### How to use 
      card(
        style = "margin-bottom: 10px;",
        card_header("How to Use This Tool"),
        card_body(
          style = "padding: 10px 12px;",
          
          div(
            style = "
        display: grid;
        grid-template-columns: 1fr 1fr;
      ",
            
            div(
              style = "display: flex; gap: 2px; align-items: flex-start; padding: 0 16px 0 0;",
              tags$span(style = "font-size: 20px; color: #5881c1; line-height: 1.1;"),
              div(
                h5("STEP 1: Adjust assumptions", style = "margin: 0 0 4px 0;"),
                p("Use the sliders on the sidebar to set you input assumptions:"),
                tags$ul(
                  style = "margin: 0; padding-left: 20px;",
                  tags$li("Annual % change in admissions/bed capacity over the next 10 years (2026-2035)."),
                  tags$li("The Future Bed Calculator gives 3 preset scenarios for admission growth, or the sliders can be adjusted to input your own value."),
                  tags$li("Annual % change in length of stay over the next 10 years (2026-2035)."),
                  tags$li("Target bed occupancy (%) you want to maintain.")
                )
              )
            ),
            
            div(
              style = "
          display: flex;
          gap: 2px;
          align-items: flex-start;
          padding: 0 16px;
          border-left: 1px solid #d9d9d9;
        ",
              tags$span(style = "font-size: 20px; color: #5881c1; line-height: 1.1;"),
              div(
                h5("Step 2: Explore Results", style = "margin: 0 0 4px 0;"),
                tags$ul(
                  style = "margin: 0; padding-left: 20px;",
                  tags$li("Four trend charts are displayed: Number of admissions, Average length of stay, Number of beds and Target bed occupancy."),
                  tags$li("On each chart a solid black lines gives the historic trends and a red dotted line indicates the projected trends from 2026-2035 (based on the selected assumptions)."),
                  tags$li("In the Future Beds Calculator the admissions and beds charts show 3 dotted grey lines that give the projected values for the 3 preset scenarios."),
                  tags$li("Adjust the sliders to watch trends change in real time."),
                  tags$li("Hover over the chart lines to read off the values. Actual values for 2025 and projected values for 2035 are given on the sidebar.")
                )
              )
            )
            

          )
        )
      ),
      
      ### Data source 
   #   card(
    #    style = "margin-bottom: 10px;",
    #    card_header("Data Source & Methodology"),
    #    card_body(
     #     style = "padding: 10px 12px;",
     #     p("Historical data (1994–2025) from NHS England. Projections apply user-defined growth assumptions.", style = "margin: 0 0 6px 0;"),
     #     p(HTML('See <b>"Assumptions and Method"</b> tab for detail.'), style = "margin: 0;")
     #   )
     # ),
      
      ### Call to action 
      div(
        style = "
        border: 2px solid #5881c1;
        background-color: #eef4fb;
        padding: 10px 12px;
        margin: 10px 0;
      ",
        h4("Ready to Get Started?", style = "margin: 0 0 6px 0;"),
        p(HTML('Go to <b>"Future Beds Calculator"</b> and adjust assumptions to predict the number of beds required to meet future changes in admissions.'), style = "margin: 0;"),
        p(HTML('Go to <b>"Future Admissions Calculator"</b> and adjust assumptions to predict the number of admissions that could be supported by future growth in bed capacity.'), style = "margin: 0;")
      )
    )
  ),
  
## Panel 2: Future beds nav panel ---------------------------------------------------------  
nav_panel(
  "Future Beds Calculator",
  class = "panel-one",
  
  layout_sidebar(
    sidebar = sidebar(
      title = NULL,
      id = "sidebar_future_beds",
      width = "300px",
      
      div(
        style = "margin-top: -18px;",
        
        h3("Future Assumptions",
          style = "font-size: 1.2rem; margin: 0 0 6px 0;"),
        
        hr(style = "margin: 6px 0;"),
        
        p("Adjust inputs for the next 10 years.",
          style = "font-size: 0.85rem; line-height: 1.15; margin: 0 0 8px 0;"),
        
        hr(style = "margin: 6px 0;"),
        
        h5("ADMISSIONS", style = "font-size: 0.95rem; font-weight: 500; margin: 6px 0;"),
      
      tooltip(
        selectInput(
          "preset",
          label = span(
            "Select Scenario or adjust slider:",
            style = "font-size: 0.85rem;"
          ), 
                    choices = c("Do nothing", "Planned", "Ambitious")), # don't think we need custom as can overwrite a preset
        "Choose a preset scenario or adjust sliders to customise assumptions",
        placement = "right"),
      
      div(style = "display:flex; justify-content:space-between; margin-top: 10px;",
        strong("Annual Change",style = "font-size: 0.8rem; font-weight: 600;")
        ),
      
      sliderInput(
        "admissions_change",
        label = NULL, min = -5, max = 5, value = 0, step = 0.1),
      
      div(style = "display:flex; justify-content:space-between; font-size:0.75rem; margin-top:8px;",
        span("2026: 16967214"),
        span("2035: 16967214") ),
      
  hr(style = "margin: 8px 0;"),
        
        h5("LENGTH OF STAY", style = "font-size: 0.95rem; font-weight: 500; margin: 6px 0;"),
      
      div(
        style = "display:flex; justify-content:space-between;",
        strong("Annual Change",style = "font-size: 0.8rem; font-weight: 600;")
      ),
      
      sliderInput(
        "los_change",
        label = NULL,
        min = -5,
        max = 5,
        value = 0,
        step = 0.1
      ),
      
      div(
        style = "display:flex; justify-content:space-between; font-size:0.75rem; margin-top:8px;",
        span("2026: 3.12 days"),
        span("2035: 3.12 days")
      ),
      
        hr(style = "margin: 8px 0;"),
        
        h5("TARGET BED OCCUPANCY", style = "font-size: 0.95rem; font-weight: 500; margin: 6px 0;"),
      
      div(
        style = "display:flex; justify-content:space-between;",
        strong("Fixed Value",style = "font-size: 0.8rem; font-weight: 600;")
      ),
      
      sliderInput(
        "target_occupancy",
        label = NULL,
        min = 75,
        max = 100,
        value = 80,
        step = 0.1
      ),
      
           div(
          style = "display:flex; justify-content:space-between; font-size:0.75rem; margin-top:-6px;",
          span("0%"),
          span("100%")
        ),
         
      actionButton(
        "reset_scenario", 
        "Reset Selected Scenario", 
        class = "btn-primary w-100",
        style = "margin-top: 10px;"
      ),
      
    
        div(
          style = "
            border: 1px solid #f9bf07;
            background-color: #fff8e1;
            padding: 8px;
            margin-top: 8px;
            font-size: 0.75rem;
            line-height: 1.15;
          ",
          "Beds required are calculated automatically from the assumptions."
        )
      )
    ),
    
    div(
      style = "padding-left: 10px;",
      
      h3("Admissions Analysis", style = "margin-bottom: 4px;"),
      hr(style = "margin: 4px 0 8px 0;"),
      
      div(
        style = "
          border: 2px solid #f9bf07;
          background-color: #fff8e1;
          padding: 6px 10px;
          margin-bottom: 8px;
          font-size: 0.8rem;
          line-height: 1.2;
        ",
        div(HTML("<b>Historical data (1994-2025)</b> shown in solid lines. <b>Predicted data (2026-2035)</b> shown in dashed lines, based on assumptions in the sidebar.")),
        div(HTML("<b>What this is:</b> A headline view of admissions, LoS, beddays, beds required and occupancy over time.")),
        div(HTML("<b>Why it is useful:</b> Shows how changing assumptions affects future pressure on beds.")),
        div(HTML("<b>How to use it:</b> Adjust the sidebar inputs and compare the projected direction and 2035 values."))
      ),
      
      layout_columns(
        card(
          card_header(HTML("Number of Admissions")),
          card_body(
            (plotlyOutput("admissions")),
            padding = 10,
            style = "height: 33vh; overflow-y: auto;"
          )
         
         
        ),
        
        card(
          card_header(HTML("Average Length of Stay (days)")),
          card_body(
            (plotlyOutput("los")),
            padding = 10,
            style = "height: 33vh; overflow-y: auto;"
          )
        ),
        
        card(
          card_header(HTML("Beds Required")),
          card_body(
            (plotlyOutput("beds")),
            padding = 10,
            style = "height: 33vh; overflow-y: auto;"
          )
        ),
        
        card(
          card_header(HTML("Bed Occupancy Rate")),
          card_body(
            (plotlyOutput("occupancy")),
            padding = 10,
            style = "height: 33vh; overflow-y: auto;"
          )
        ),
        
        col_widths = c(6, 6, 6, 6)
      )

    )
  )
),

## Panel 3: Future Admissions Calculator nav panel ---------------------------------------
nav_panel(
  "Future Admissions Calculator",
  
  layout_sidebar(
    sidebar = sidebar(
      title = NULL,
      id = "sidebar_fixed_beds",
      width = "300px",
      
      div(
        style = "margin-top: -18px;",
        
        h3(
          "Future Admissions Calculator",
          style = "font-size: 1.2rem; margin: 0 0 6px 0;"
        ),
        
        hr(style = "margin: 6px 0;"),
        
        p(
          "Set a fixed bed supply and test what admissions and LoS combinations would be sustainable.",
          style = "font-size: 0.85rem; line-height: 1.15; margin: 0 0 8px 0;"
        ),
        
        hr(style = "margin: 6px 0;"),
        
        h5("BED SUPPLY", style = "font-size: 0.95rem; margin: 6px 0;"),
        
        p(
          paste("Baseline (2025):", baseline_beds),
          style = "font-size: 0.8rem; margin-bottom: 4px;"
        ),
        
        div(
          style = "display:flex; justify-content:space-between; font-size:0.8rem;",
          strong("Fixed beds"),
          span("1000")
        ),
        
        sliderInput("bedday_growth", 
                    label="Annual Growth (%):", 
                    min = 0, 
                    max =5,  
                    value=0, 
                    step=0.1),
        
        
        hr(style = "margin: 8px 0;"),
        
        h5("LENGTH OF STAY", style = "font-size: 0.95rem; margin: 6px 0;"),
        
        div(
          style = "display:flex; justify-content:space-between; font-size:0.8rem;",
          strong("Annual Change"),
          span("0%")
        ),
        
        sliderInput(
          "los_change2",
          NULL,
          min = -5,
          max = 5,
          value = 0,
          step = 0.1,
          width = "100%"
        ),
        
        hr(style = "margin: 8px 0;"),
        
        h5("TARGET BED OCCUPANCY", style = "font-size: 0.95rem; margin: 6px 0;"),
        
        div(
          style = "display:flex; justify-content:space-between; font-size:0.8rem;",
          strong("Fixed Value"),
          span("85%")
        ),
        
        sliderInput(
          "bed_occupancy",
          NULL,
          min = 75,
          max = 100,
          value = 85,
          step = 0.5,
          width = "100%"
        ),

        
        actionButton(
          "reset_baseline", 
          "↻ Reset to Baseline", 
          class = "btn-primary w-100",
          style = "margin-top: 8px; font-size: 0.8rem; padding: 6px;"
        ),
        
        div(
          style = "
            border: 1px solid #f9bf07;
            background-color: #fff8e1;
            padding: 8px;
            margin-top: 8px;
            font-size: 0.75rem;
            line-height: 1.15;
          ",
          "Admissions capacity is calculated from the selected beds, LoS and target bed occupancy assumptions."
        )
      )
    ),
    
    div(
      style = "padding-left: 10px;",
      
      h3("Future Admissions Calculator", style = "margin-bottom: 4px;"),
      hr(style = "margin: 4px 0 8px 0;"),
      
      div(
        style = "
          border: 2px solid #f9bf07;
          background-color: #fff8e1;
          padding: 6px 10px;
          margin-bottom: 8px;
          font-size: 0.8rem;
          line-height: 1.2;
        ",
        div(HTML("<b>What this is:</b> A backwards-looking planning view that starts with a fixed number of beds and estimates what level of admissions could be supported.")),
        div(HTML("<b>Why it is useful:</b> Helps test capacity-constrained scenarios where bed numbers are fixed or cannot grow enough to match demand.")),
        div(HTML("<b>How to use it:</b> Adjust fixed beds, LoS and target bed occupancy in the sidebar, then compare the resulting projected admissions capacity and bed pressure over time."))
      ),
      
      layout_columns(
        card(
          card_header(HTML("Supported Admissions")),
          card_body(
            (plotlyOutput("admissions2")),
            padding = 10,
            style = "height: 33vh; overflow-y: auto;"
          )
        ),
        
        card(
          card_header(HTML("Average Length of Stay")),
          card_body(
            (plotlyOutput("los2")),
            padding = 10,
            style = "height: 33vh; overflow-y: auto;"
          )
        ),
        
        card(
          card_header(HTML("Number of Beds")),
          card_body(
            (plotlyOutput("beds2")),
            padding = 10,
            style = "height: 33vh; overflow-y: auto;"
          )
        ),
        
        card(
          card_header(HTML("Bed Occupancy Rate")),
          card_body(
            (plotlyOutput("occupancy2")),
            padding = 10,
            style = "height: 33vh; overflow-y: auto;"
          )
        ),
        
        col_widths = c(6, 6, 6, 6)
      ),
      
    #  card(
    #    card_header("Key Metrics (2035 Projected)"),
    #    
    #    div(
    #      style = "
    #        border: 2px solid #f9bf07;
    #        background-color: #fff8e1;
    #        padding: 6px 10px;
    #        margin: 8px;
    #        font-size: 0.75rem;
    #        line-height: 1.15;
    #      ",
    #      "These are the modelled 2035 values based on the fixed bed assumptions set in the sidebar."
    #    ),
    #    
    #    layout_columns(
     #     card(
     #       style = "padding: 6px;",
     #       div("Supported admissions", style = "font-size: 0.75rem;"),
     #       strong("Add value")
     #     ),
          
     #     card(
     #       style = "padding: 6px;",
     #       div("LoS", style = "font-size: 0.75rem;"),
     #       strong("Add value")
     #     ),
          
     #     card(
     #       style = "padding: 6px;",
     #       div("Fixed beds", style = "font-size: 0.75rem;"),
     #       strong("Add value")
     #     ),
          
     #     card(
      #      style = "padding: 6px;",
     #       div("Occupancy", style = "font-size: 0.75rem;"),
     #       strong("Add value")
     #     ),
          
     #     col_widths = c(3, 3, 3, 3)
     #   )
      #)
    )
  )
),

## Panel 4: Methodology -----------------------------------------------------
nav_panel(
  "Methodology",
  
  div(
    style = "width: 88vw; max-width: none; margin: 0 auto;",
    
    card(
      card_header("Model Overview"),
      div(
        style = "padding: 4px;",
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
          tags$li("Admissions: Number of hospital admissions per year"),
          tags$li("Length of stay: Average number of days in hospital for each admission"),
          tags$li("Beds: Number of available beds (ANNUAL AVERAGE?)"),
          tags$li("Target bed occupancy: % of available beds that are occupied at a given time")
        ),
        tags$ul(
          tags$li("Change in number of admissions: Predicted % annual change in the number of hospital admissions"),
          tags$li("Change in average length of stay: Predicted % annual change in length of stay"),
          tags$li("Change in number of beds: Predicted % of annual growth in the number of available beds")
        )
      )
      
    ),
    
    #layout_columns(
      card(
        card_header("Data"),
        div(
          style = "padding: 4px;",
          div(
        #    style = "
        #     border: 1px solid #686f73;
        #      padding: 12px;
        #      margin-bottom: 10px;
        #    ",
            p("The historic annual number of admissions and average length of stays between 1994 and 2025 were derived from the Hospital Episode Statistics dataset"),
            p("The historic bed occupancy was taken from the NHS England Bed Availability and Occupancy (KH03) Collection?????."),
            p("The scenario assumptions (Do nothing, Planned and Ambitious) use potential changes in admissions based on  NHP work???. It is assumed length of stay doesn't change and the target bed occupancy is set at 85%, which is generally considered the optimal bed occupancy.")
          ),
          
       #   div(
       #     style = "
      #        border: 1px solid #686f73;
       #       padding: 12px;
       #     ",
       #     p("Time Period"),
        #    p("Weekly admissions")
       #   )
        )
      ),
      
      card(
        card_header("Assumptions and Limitations"),
        div(
          style = "padding: 4px;",
         # div(
         #   style = "
         #     border: 1px solid #686f73;
         #     min-height: 18px;
         #     margin-bottom: 10px;
         #   "
        #  ),
          p(strong("This tool is designed for strategic planning only, and not for operational decisions."))
        )
      #),
      
      #col_widths = c(6, 6)
    ),
    
    card(
      class = "toy-model-links",
      style = "margin-bottom: 10px; height: 15%;",
      card_header("References"),
      card_body(
        style = "padding: 10px 12px 8px 12px;",
        div(
          style = "display: flex; gap: 10px; align-items: flex-start;",
          tags$span(
            style = "font-size: 20px; color: #5881c1; line-height: 1.1;"),
          div(
            
            tags$ul(
              style = "margin: 0; padding-left: 20px;",
              
              tags$li(
                tags$a(
                  "NHS England: Bed Availability and Occupancy",
                  href = "https://www.england.nhs.uk/statistics/statistical-work-areas/bed-availability-and-occupancy/",
                  target = "_blank"
                )),
              
              tags$li(
                tags$a(
                  "Strategy Unit: New Hospitals Programme Capacity and Demand Modelling",
                  href = "https://www.strategyunitwm.nhs.uk/news/transforming-hospital-planning-open-source-demand-and-capacity-model",
                  target = "_blank"
                )),
              
              tags$li(
                tags$a(
                  "The Nuffield Trust: Hospital Bed Occupancy",
                  href = "https://www.nuffieldtrust.org.uk/resource/hospital-bed-occupancy",
                  target = "_blank"
                ))
            ))))),
    
  )
)

)




#Server interface --------------------------------------------------------------

server <- function(input, output, session) {
  
  observeEvent(input$preset, {
    if (input$preset == "Do nothing") {
      updateSliderInput(session, "admissions_change", value = 2.8)
      updateSliderInput(session, "los_change", value = 0)
      updateSliderInput(session, "target_occupancy", value = 85)
    } else if (input$preset == "Planned") {
      updateSliderInput(session, "admissions_change", value = 1.9)
      updateSliderInput(session, "los_change", value = 0)
      updateSliderInput(session, "target_occupancy", value = 85)
    } else if (input$preset == "Ambitious") {
      updateSliderInput(session, "admissions_change", value = 0.9)
      updateSliderInput(session, "los_change", value = 0)
      updateSliderInput(session, "target_occupancy", value = 85)
    }
  })
  
  
  observeEvent(input$reset_scenario, {
    if (input$preset == "Do nothing") {
      updateSliderInput(session, "admissions_change", value = 2.8)
      updateSliderInput(session, "los_change", value = 0)
      updateSliderInput(session, "target_occupancy", value = 85)
    } else if (input$preset == "Planned") {
      updateSliderInput(session, "admissions_change", value = 1.9)
      updateSliderInput(session, "los_change", value = 0)
      updateSliderInput(session, "target_occupancy", value = 85)
    } else if (input$preset == "Ambitious") {
      updateSliderInput(session, "admissions_change", value = 0.9)
      updateSliderInput(session, "los_change", value = 0)
      updateSliderInput(session, "target_occupancy", value = 85)
    }
  })
  
  observeEvent(input$reset_baseline, {
    updateSliderInput(session, "bedday_growth", value = 0) 
    updateSliderInput(session, "los_change2", value = 0) 
    updateSliderInput(session, "bed_occupancy", value = 85) 
  })
  
  
  # Chart outputs-----------------------------------------------------------------
  
  
  output$admissions<-renderPlotly({
    
    plot_data<-future_beds_model(from_93, input$admissions_change, input$los_change, input$target_occupancy)
    do_nothing<-future_beds_model(from_93, 2.8 , 0, 85)
    planned<-future_beds_model(from_93, 1.9, 0, 85)
    ambitious<-future_beds_model(from_93, 0.9, 0, 85)
    
    axis_max_data<-future_beds_model(from_93, 5, input$los_change, input$target_occupancy)
    y_axis_max<-max(axis_max_data$admissions, na.rm = TRUE) * 1.05
    
    
    plotting_function(plot_data,
                      do_nothing,
                      planned,
                      ambitious,
                      "admissions", 
                      "Admissions (millions)",
                      y_axis_max)
    
  })
  
  output$los<-renderPlotly({
    
    plot_data<-future_beds_model(from_93, input$admissions_change, input$los_change, input$target_occupancy)
    do_nothing<-future_beds_model(from_93, 2.8 , 0, 85)
    planned<-future_beds_model(from_93, 1.9, 0, 85)
    ambitious<-future_beds_model(from_93, 0.9, 0, 85)
    
    axis_max_data<-future_beds_model(from_93, input$admissions_change, 5, input$target_occupancy)
    y_axis_max<-max(axis_max_data$los, na.rm = TRUE) * 1.05
    
    plotting_function(plot_data,
                      do_nothing,
                      planned,
                      ambitious,
                      "los", 
                      "Length of Stay (days)",
                      y_axis_max)
    
  })
  
  output$beds<-renderPlotly({
    
    plot_data<-future_beds_model(from_93, input$admissions_change, input$los_change, input$target_occupancy)
    do_nothing<-future_beds_model(from_93, 2.8 , 0, 85)
    planned<-future_beds_model(from_93, 1.9, 0, 85)
    ambitious<-future_beds_model(from_93, 0.9, 0, 85)
    
    axis_max_data<-future_beds_model(from_93, 5, 5, 75)
    y_axis_max<-max(axis_max_data$beds, na.rm = TRUE) * 1.05
    
    plotting_function(plot_data,
                      do_nothing,
                      planned,
                      ambitious,
                      "beds",
                      "Beds",
                      y_axis_max)
    
  })
  
  
  output$occupancy<-renderPlotly({
    
    plot_data<-future_beds_model(from_93, input$admissions_change, input$los_change, input$target_occupancy)
    do_nothing<-future_beds_model(from_93, 2.8 , 0, 85)
    planned<-future_beds_model(from_93, 1.9, 0, 85)
    ambitious<-future_beds_model(from_93, 0.9, 0, 85)
    
    y_axis_max<-100
    
    plotting_function(plot_data,
                      do_nothing,
                      planned,
                      ambitious,
                      "occupancy", 
                      "Bed Occupancy (%)",
                      y_axis_max)
    
  })
  
  
  
  output$admissions2<-renderPlotly({
    
    plot_data<-future_admissions_model(from_93, input$bedday_growth, input$los_change2, input$bed_occupancy)
    
    axis_max_data<-future_admissions_model(from_93, 5, -5, 100)
    y_axis_max<-max(axis_max_data$admissions, na.rm = TRUE) * 1.05
    
    plotting_function(plot_data,
                      NULL,
                      NULL,
                      NULL,
                      "admissions", 
                      "Admissions (millions)",
                      y_axis_max)
    
  })
  
  output$los2<-renderPlotly({
    
    plot_data<-future_admissions_model(from_93, input$bedday_growth, input$los_change2, input$bed_occupancy)
    
    axis_max_data<-future_admissions_model(from_93, input$bedday_growth, 5, input$bed_occupancy)
    y_axis_max<-max(axis_max_data$los, na.rm = TRUE) * 1.05
    
    plotting_function(plot_data,
                      NULL,
                      NULL,
                      NULL,
                      "los", 
                      "Length of Stay (days)",
                      y_axis_max)
    
  })
  
  output$beds2<-renderPlotly({
    
    plot_data<-future_admissions_model(from_93, input$bedday_growth, input$los_change2, input$bed_occupancy)
    
    axis_max_data<-future_admissions_model(from_93, 5, input$los_change2, input$bed_occupancy)
    y_axis_max<-max(axis_max_data$beds, na.rm = TRUE) * 1.05
    
    plotting_function(plot_data,
                      NULL,
                      NULL,
                      NULL,
                      "beds", 
                      "Beds",
                      y_axis_max)
    
  })
  
  
  output$occupancy2<-renderPlotly({
    
    plot_data<-future_admissions_model(from_93, input$bedday_growth, input$los_change2, input$bed_occupancy)
    
    y_axis_max<-100
    
    plotting_function(plot_data,
                      NULL,
                      NULL,
                      NULL,
                      "occupancy", 
                      "Bed Occupancy (%)",
                      y_axis_max)
    
  })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
