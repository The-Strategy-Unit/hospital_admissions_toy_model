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

# Data-------------------------------------------------------------------------

library(readxl)
library(ggplot2)
library(StrategyUnitTheme)
library(plotly)

options(scipen=999)

data<-read_excel("data/Collating the data.xlsx")
from_93<-data[7:38,]
--------------------------------------------------------------------------------
  
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
    occupancy<- c(occupancy, occpuancy_fixed_level)
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
    beds <- c(beds, qpois(occupancy_fixed_level, admissions[length(admissions)]*los[length(los)]/365))
    occupancy<- c(occupancy, occupancy_fixed_level)
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


--------------------------------------------------------------------------------
baseline_beds<-1000

# Define UI for application that draws a histogram
ui <- page_navbar(
  title = "Title",
  id = "nav",
  bg = "#f9bf07",
  theme = bs_theme(
    bootswatch = "united",
    dark = "black",
    primary = "#686f73",
    secondary= "#f9bf07" 
    ),
  
  nav_panel(
    "Future beds calculator",
    class = "panel-one",

  
  tags$head(
    tags$style(HTML("
      .sidebar-title { padding-bottom: 0 !important; margin-bottom: 0 !important; }
    "))
  ),

  layout_sidebar(
  sidebar = sidebar(
    title ="Input assumptions",
    id = "sidebar",
    width = "320px",
    
    p("Adjust each input to model the impact on beds over the next 10 years.",  style = "font-size: 0.9rem", class = "pt-0"),
    
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
    card_header("xxxxxxxxxxx",
                class = "bg-dark"),
    p(textOutput("TEST"))
  )
  )  
  ),
  
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


# Define server logic required to draw a histogram
server <- function(input, output, session) {

  observeEvent(input$preset, {
    if (input$preset == "Scenario 1") {
      updateSliderInput(session, "admissions_val", value = 1)
      updateSliderInput(session, "los_val", value = -2)
      updateSliderInput(session, "val", value = 85)
    } else if (input$preset == "Scenario 2") {
      updateSliderInput(session, "admissions_val", value = 5)
      updateSliderInput(session, "los_val", value = -5)
      updateSliderInput(session, "val", value = 90)
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
