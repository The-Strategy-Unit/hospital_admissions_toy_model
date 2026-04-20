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
    "Introduction",
    class = "panel-one",
    card(
      card_header("xxxxxxxxxxx",
                  class = "bg-dark"),
      p("xxxxxxxxxxxxxxxxx")
      )
    ),
  
  tags$head(
    tags$style(HTML("
      .sidebar-title { padding-bottom: 0 !important; margin-bottom: 0 !important; }
    "))
  ),


  sidebar = sidebar(
    title ="Setting assumptions",
    id = "sidebar",
    width = "320px",
    
    p("Use the sliders to model how admissions, LoS, and occupancy might change annually over the next 10 yrs. 
      The beds required will be calculated automatically.",  style = "font-size: 0.9rem", class = "pt-0"),
    
    hr(class = "my-0"), # This creates the horizontal line
    
    h5("Admissions"),
    
    tooltip(
      selectInput("preset", "Select a Scenario:", 
                  choices = c("Scenario 1", "Scenario 2")), # don't think we need custom as can overwrite a preset
      "Choose a preset scenario or adjust sliders to customise assumptions",
      placement = "right"                      
    ),
      
    sliderInput("admissions_val", "Annual change:", min = -10, max = 10, value = 0, step = 0.5),
    
    hr(class = "my-0"), # This creates the horizontal line
    
    h5("Length of Stay"),

    sliderInput("los_val", "Annual change:", min = -10, max =+10,  value=0, step=0.5),
    
    hr(class = "my-0"), # This creates the horizontal line
    
    h5("Target Occupancy"),
    
    sliderInput("val", "Fixed value:", min = 0, max =100, value = 80, step=0.5),
  
    actionButton("reset", 
      "Reset to Selected Scenario", 
      class = "btn-secondary w-100" )
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
  
  
  observeEvent(input$reset, {
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
  
  
  output$display_val <- renderText({
    paste("The current value is:", input$admissions_val)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
