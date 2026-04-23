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
