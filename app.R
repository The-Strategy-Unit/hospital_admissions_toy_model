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

##Explainer ---------------------------------------------------------  
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
      p(HTML('Click on the <b>"Overview"</b> tab to begin exploring the data. Use the sidebar sliders to adjust your assumptions and watch how the projections change in real-time.'))
    )
  )
),

##Future beds nav panel ---------------------------------------------------------  
  nav_panel(
    "Future beds calculator",
    class = "panel-one",

  
  tags$head(
    tags$style(HTML("
      .sidebar-title { padding-bottom: 0 !important; margin-bottom: 0 !important; }
    "))
  ),
  
  
#Sidebar layout
  layout_sidebar(
  sidebar = sidebar(
    title ="Input assumptions",
    id = "sidebar",
    width = "320px",
    
    p("Adjust how you think each input might change over the next 10 years.",  style = "font-size: 0.9rem", class = "pt-0"),
    
    hr(class = "my-0"), # This creates the horizontal line
    
    h5("Admissions"),
    
    tooltip(
      selectInput("preset", "Select a Scenario:", 
                  choices = c("Scenario 1", "Scenario 2")), # don't think we need custom as can overwrite a preset
      "Choose a preset scenario or adjust sliders to customise assumptions",
      placement = "right"                      
    ),
      
    sliderInput("admissions_change", "Annual % change:", min = -10, max = 10, value = 0, step = 0.1),
    
    hr(class = "my-0"), # This creates the horizontal line
    
    h5("Length of Stay"),

    sliderInput("los_change", "Annual % change:", min = -10, max =+10,  value=0, step=0.1),
    
    hr(class = "my-0"), # This creates the horizontal line
    
    h5("Bed Occupancy"),
    
    sliderInput("target_occupancy", "Fixed % value:", min = 0, max =100, value = 80, step=0.1),
  
    actionButton("reset_scenario", 
      "Reset to Selected Scenario", 
      class = "btn-secondary w-100" )
  ),
  card(
    card_header("Admissions Analysis",
                class = "bg-dark"),
    p(textOutput("TEST"))
  )
  )  
  ),
  
##Future admissions nav panel --------------------------------------------------
  nav_panel(
    "Future Admissions calculator",

  
  layout_sidebar(
    sidebar = sidebar(
    title ="Input assumptions",
    id = "sidebar",
    width = "320px",
    
    p("Adjust each input to model the impact on admissions over the next 10 years.",  style = "font-size: 0.9rem", class = "pt-0"),
    
    hr(class = "my-0"), # This creates the horizontal line
    
    h5("Beds"),
    
    p("Baseline (2025):",baseline_beds),
    
    sliderInput("bedday_growth", label="Predicted growth (%):", min = 0, max =50,  value=0, step=0.1),
    
    hr(class = "my-0"), # This creates the horizontal line
    
    h5("Length of Stay (in days)"),
    
    sliderInput("los_days", label=NULL, min = 0, max =20,  value=5.2, step=0.1),
    
    hr(class = "my-0"), # This creates the horizontal line
    
    h5("Bed Occupancy (%)"),
    
    sliderInput("bed_occupancy",  label=NULL, min = 0, max =100, value = 85, step=0.5),
    
    actionButton("reset_baseline", 
                 "Reset to baseline", 
                 class = "btn-secondary w-100" )
  ),
  
  card(
    card_header("xxxxxxxxxxx2",
                class = "bg-dark"),
    card_body( p("xxxxxxxxxxxxxxxxx")
  
  ))
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
