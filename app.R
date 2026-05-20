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
library(dplyr)

options(scipen=999)

# Data-------------------------------------------------------------------------

data<-read_excel("data/Collating the data_final.xlsx")
from_93<-data[7:38,]

# Formatting data----------------------------------------------------------------
historic_trends<-from_93|>
  rename(admissions=`All admissions`)|>
  rename(los=`avgLoS`)|>
  rename(beds=Beds)|>
  select(admissions, los, beds, occupancy)|>
  summarise(across(everything(),  ~ ((last(.) / first(.))^(1 / n()) - 1) * 100))|>
  mutate(across(where(is.numeric), round, digits = 2))

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
    beds = round(unlist(beds),0)/1000,
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
    beds <- c(beds, qpois((occupancy_fixed/100), (admissions[length(admissions)]/365)*los[length(los)]))
    occupancy<- c(occupancy, (occupancy_fixed/100))
  }
  
  plot_data<-data.frame(
    year = as.numeric(unlist(years)), 
    admissions = (unlist(admissions))/1000000,
    los = unlist(los), 
    beds = round(unlist(beds),0)/1000,
    occupancy = (unlist(occupancy))*100
  )
  
  return(plot_data)
  
}

## Plotting function
plotting_function <- function(
    plot_data,
    scenario_1_values,
    scenario_2_values,
    scenario_3_values,
    output_type,
    y_axis_label,
    y_axis_max = NULL,
    show_trend_labels = FALSE
) {
  # Heather adding historic and future average annual % change chart labels
  
  annual_percentage_change <- function(data, output_type, start_year, end_year) {
    
    start_value <- data[[output_type]][data$year == start_year]
    end_value <- data[[output_type]][data$year == end_year]
    number_of_years <- end_year - start_year
    
    if (
      length(start_value) == 0 ||
      length(end_value) == 0 ||
      is.na(start_value) ||
      is.na(end_value) ||
      start_value <= 0 ||
      end_value <= 0
    ) {
      return(NA)
    }
    
    ((end_value / start_value)^(1 / number_of_years) - 1) * 100
  }
  
  historical_avg_annual_change <- annual_percentage_change(
    plot_data,
    output_type,
    min(plot_data$year[plot_data$year <= 2025], na.rm = TRUE),
    2025
  )
  
  future_avg_annual_change <- annual_percentage_change(
    plot_data,
    output_type,
    2025,
    2035
  )
  
  # Heather adding historic trend continuation line
  
  historic_trend_data <- data.frame(
    year = 2025:2035
  )
  
  historic_2025_value <- plot_data[[output_type]][plot_data$year == 2025]
  
  historic_trend_data[[output_type]] <- historic_2025_value *
    (1 + historical_avg_annual_change / 100)^(historic_trend_data$year - 2025)
  
  y_axis_min <- min(
    plot_data[[output_type]],
    historic_trend_data[[output_type]],
    na.rm = TRUE
  )
  
  y_axis_max <- max(
    plot_data[[output_type]],
    historic_trend_data[[output_type]],
    na.rm = TRUE
  )
  
  y_axis_range <- y_axis_max - y_axis_min
  
  if(y_axis_range == 0){
    y_axis_range <- y_axis_max * 0.1
  }
  
  y_axis_min <- y_axis_min - (y_axis_range * 0.08)
  y_axis_max <- y_axis_max + (y_axis_range * 0.25)
  
  y_label_position <- y_axis_min + ((y_axis_max - y_axis_min) * 0.95)
  
  historical_label <- paste0(
    "Historic avg %<br>change: ",
    round(historical_avg_annual_change, 1),
    "%"
  )
  
  future_label <- paste0(
    "Future avg %<br>change: ",
    round(future_avg_annual_change, 1),
    "%"
  )
  
  # Initial plot
  p <- ggplot(data = plot_data, aes(x = year, y = .data[[output_type]], 
                                    text = paste0("Year: ", year, "<br>", y_axis_label, ": ", round(.data[[output_type]], 1)))) +
    #geom_vline(xintercept = 2025, colour = "#5881c1" , linetype = "dashed") +
    geom_vline(xintercept = 2025, colour = "#f9bf07" , linetype = "dashed") +
    su_theme() +
    labs(title=NULL, subtitle=NULL, y = NULL, x = "Year") +
    scale_y_continuous(limits = c(y_axis_min, y_axis_max))
  
  # Add Scenario plots if needed
  #  if (!is.null(scenario_1_values)) {
  #    p <- p + geom_line(data = scenario_1_values, 
  #                       aes(x = year, y = .data[[output_type]], 
  #                           text = paste0("Do nothing<br>", "Year: ", year, "<br>", y_axis_label, ": ", round((.data[[output_type]]), 1))), 
  #                       colour = "#b2b7b9", linewidth = 0.5, linetype = "dotted", group = 1)+ 
  #      geom_line(data = scenario_2_values, 
  #                aes(x = year, y = .data[[output_type]], 
  #                    text = paste0("Planned<br>", "Year: ", year, "<br>", y_axis_label, ": ", round((.data[[output_type]]), 1))), 
  #                colour = "#b2b7b9", linewidth = 0.5, linetype = "dotted", group = 1)+ 
  #      geom_line(data = scenario_3_values, 
  #                aes(x = year, y = .data[[output_type]], 
  #                    text = paste0("Ambitious<br>", "Year: ", year, "<br>", y_axis_label, ": ", round((.data[[output_type]]), 1))), 
  #                colour = "#b2b7b9", linewidth = 0.5, linetype = "dotted", group = 1)
  # }
  
  # Add the main line last so it sits on top
  p <- p +     
    geom_line(
      data = historic_trend_data,
      aes(
        x = year,
        y = .data[[output_type]],
        text = paste0(
          "Historic trend continued<br>",
          "Year: ", year,
          "<br>", y_axis_label, ": ", round(.data[[output_type]], 1)
        )
      ),
      colour = "#5881c1",
      linewidth = 0.6,
      linetype = "dotted",
      group = 1
    ) +
    geom_line(data = subset(plot_data, as.numeric(year) >= 2025),
              aes(x = year, y = .data[[output_type]]),
              colour = "#5881c1", 
              linewidth = 0.6, 
              linetype = "solid", 
              group = 1)+
    geom_line(data = subset(plot_data, as.numeric(year) <= 2025),
              aes(x = year, y = .data[[output_type]]),
              colour = "black", 
              linewidth = 0.5, 
              linetype = "solid", 
              group = 1)
  
  
  #ggplotly(p, tooltip = "text")|>
  #  layout(
  #    margin = list(t = 20, b = 10, l = 55, r =10, pad = 0), # Top, Bottom, Left, Right
  #  )
  
  ggplotly(p, tooltip = "text")|>
    layout(
      margin = list(t = 20, b = 10, l = 30, r = 8, pad = 0),
      
      yaxis = list(
        title = "",
        automargin = FALSE,
        fixedrange = FALSE
      ),
      annotations = if(show_trend_labels) {
        list(
          list(
            x = 2024.4,
            y = y_label_position,
            text = historical_label,
            showarrow = FALSE,
            xanchor = "right",
            yanchor = "middle",
            font = list(size = 14, color = "black")
          ),
          list(
            x = 2025.6,
            y = y_label_position,
            text = future_label,
            showarrow = FALSE,
            xanchor = "left",
            yanchor = "middle",
            font = list(size = 14, color = "black")
          )
        )
      } else {
        list()
      }
    )
}



#UI ----------------------------------------------------------------------------
##UI General
ui <- page_navbar(

  title = "Why do we need more beds?",
  position = "fixed-top", 
  id = "nav",
  bg = "#2c2825",
  theme = bs_theme(
    bootswatch = "united",
    primary = "#686f73",
    secondary = "#f9bf07"
  ),
  
  header = tagList(
    
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
    
    .infographic-wrap {
  position: relative;
  width: 330px;
  height: 245px;
  margin: 0 auto;
}

.info-node {
  position: absolute;
  width: 92px;
  height: 92px;
  border-radius: 50%;
  text-align: center;
  padding-top: 18px;
  z-index: 3;
}

.info-value {
  font-size: 1.35rem;
  font-weight: 800;
  line-height: 1;
}

.info-label {
  font-size: 0.68rem;
  line-height: 1.05;
  margin-top: 6px;
}

.info-sublabel {
  font-size: 0.62rem;
  line-height: 1.05;
}

.info-node-blue {
  top: 0;
  left: 119px;
  border: 1.5px solid #5881c1;
  background: #eef4fb;
}

.info-node-grey {
  top: 145px;
  left: 0;
  border: 1.5px solid #686f73;
  background: #f4f4f4;
}

.info-node-yellow {
  top: 145px;
  right: 0;
  border: 1.5px solid #f9bf07;
  background: #fff8e1;
}

.info-arrow {
  position: absolute;
  height: 3px;
  background: #9fa3a6;
  transform-origin: left center;
  z-index: 1;
}

.info-arrow::after {
  content: '';
  position: absolute;
  right: -1px;
  top: -5px;
  width: 0;
  height: 0;
  border-left: 12px solid #9fa3a6;
  border-top: 6px solid transparent;
  border-bottom: 6px solid transparent;
}

.info-arrow-left {
  width: 105px;
  top: 93px;
  left: 105px;
  transform: rotate(125deg);
}

.info-arrow-right {
  width: 105px;
  top: 93px;
  left: 220px;
  transform: rotate(55deg);
}

.info-arrow-bottom {
  width: 118px;
  top: 191px;
  left: 108px;
}

.info-output {
  position: absolute;
  top: 92px;
  left: 105px;
  width: 120px;
  text-align: center;
  z-index: 2;
}

.bed-icon {
  margin-bottom: 2px;
}

.bed-icon i {
  font-size: 2.4rem;
  color: #4a4a4a;
  line-height: 1;
}

.output-label {
  font-size: 0.72rem;
  font-weight: 800;
  letter-spacing: 0.03em;
  margin-top: 4px;
}

.output-value {
  font-size: 2rem;
  font-weight: 900;
  color: #d99a00;
  line-height: 1;
}

/* PANEL 3 — Supported Admissions infographic */

.panel3-smart-wrap {
  position: relative;
  width: 100%;
  max-width: 390px;
  height: 255px;
  margin: 8px auto 6px auto;
  overflow: hidden;
}

.panel3-smart-inputs {
  position: absolute;
  left: 8px;
  top: 18px;
  width: 130px;
  z-index: 3;
  display: flex;
  flex-direction: column;
  gap: 8px;
}

.panel3-smart-input {
  border-radius: 7px;
  padding: 7px 5px;
  text-align: center;
  min-height: 61px;
}

.panel3-smart-blue {
  border: 1.5px solid #5881c1;
  background: #eef4fb;
}

.panel3-smart-grey {
  border: 1.5px solid #686f73;
  background: #f4f4f4;
}

.panel3-smart-yellow {
  border: 1.5px solid #f9bf07;
  background: #fff8e1;
}

.panel3-smart-value {
  font-size: 1.1rem;
  font-weight: 900;
  line-height: 1;
}

.panel3-smart-label {
  font-size: 0.7rem;
  line-height: 1.05;
  margin-top: 4px;
}

.panel3-smart-sublabel {
  font-size: 0.62rem;
  line-height: 1.05;
}

.panel3-smart-flow {
  position: absolute;
  left: 122px;
  top: 30px;
  width: 150px;
  height: 175px;
  z-index: 1;
}

.panel3-smart-output {
  position: absolute;
  right: 4px;
  top: 52px;
  width: 128px;
  min-height: 128px;
  border: 1.5px solid #f9bf07;
  background: #fff8e1;
  border-radius: 8px;
  text-align: center;
  padding: 10px 6px;
  z-index: 4;
}

.panel3-smart-output .bed-icon i {
  font-size: 2.4rem;
  color: #4a4a4a;
}

.panel3-smart-output-label {
  font-size: 0.72rem;
  font-weight: 900;
  line-height: 1.1;
  margin-top: 6px;
  color: #2c2825;
}

.panel3-smart-output-value {
  font-size: 1.75rem;
  font-weight: 900;
  line-height: 1.05;
  margin-top: 8px;
}

.panel3-smart-note {
  font-size: 0.8rem;
  margin: 0;
  line-height: 1.2;
}

")
               ),
    
    tags$head(
      tags$style(HTML("
    body { 
      padding-top: 60px !important; 
    }
  "))
    ),
    
    tags$style(HTML("
      .irs-min, .irs-max, .irs-grid-pol.small { 
        display: none !important; 
      }
      
      .irs-bar, .irs-bar-edge, .irs-single {
        background: #5881c1 !important;
        border-color: #5881c1 !important;
      }
      
      .irs-handle > i:first-child {
        background-color: #5881c1 !important;
      }
    ")
  ),
  
  tags$script(HTML("
        $(document).on('shiny:connected', function() {
          var slider = $('#target_occupancy').data('ionRangeSlider');
          if (slider) {
            slider.update({ grid_num: 5 });
          }
        });
      ")),
  
  tags$script(HTML("
        $(document).on('shiny:connected', function() {
          var slider = $('#bed_occupancy').data('ionRangeSlider');
          if (slider) {
            slider.update({ grid_num: 5 });
          }
        });
      ")),
    
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
    )
  ),
  
  ## Panel 1: Landing page ---------------------------------------------------------
  nav_panel(
    "The Question",
    
    div(
      style = "max-width: 1400px; margin: 0 auto;",
      
      div(
        style = "
        background-color: #f9bf07;
        border: 2px solid #2c2825;
        padding: 34px 36px;
        margin: 30px 0 18px 0;
      ",
        
        h1(
          "Why do we need more beds?",
          style = "
          margin: 0;
          font-size: 2.2rem;
          line-height: 1.15;
        "
        )
      ),
      
      card(
        style = "margin-bottom: 10px;",
        card_header("A changing picture of hospital bed requirements"),
        card_body(
          style = "padding: 10px 12px 8px 12px;",
          div(
            style = "display: flex; gap: 10px; align-items: flex-start;",
            tags$span(
              style = "font-size: 20px; color: #5881c1; line-height: 1.1;"
            ),
            div(
              p(
                "Historically reduction in LoS has allowed for reduction in bed capacity despite growing admissions. However, there is a limit to the amount LoS can reduce, and if admissions continue to grow bed capacity must also to maintain system performance.",
                style = "margin: 0 0 6px 0;"
              ),
              p(
                "This app allows the user to build intuition by:",
                style = "margin: 0 0 6px 0;"
              ),
              tags$ul(
                style = "margin: 0; padding-left: 20px;",
                tags$li("Viewing how admissions, available beds, average length of stay and bed occupancy have changed over the past 30+ years (1994–2025)."),
                tags$li("How changes in admissions could affect future bed requirements (Future Beds Calculator)."),
                tags$li("How many admissions could be supported by future growth in bed capacity (Future Admissions Calculator).")
              )
            )
          )
        )
      )
    )
  ),
  
  ## Panel 2: Beds nav panel ---------------------------------------------------------  
  nav_panel(
    "Beds Calculator",
    class = "panel-one",
    
    layout_sidebar(
      sidebar = sidebar(
        title = NULL,
        id = "sidebar_future_beds",
        width = "300px",
        open = "always",
        
        div(
          style = "margin-top: 18px;",
          
          h3("Future Assumptions",
             style = "font-size: 1.2rem; margin: 0 0 6px 0;"),
          
          hr(style = "margin: 18px 0;"),
          
          p("Set assumptions for admissions, length of stay and bed occupancy over the next 10 years, and the number of beds required is calculated.",
            style = "font-size: 0.85rem; line-height: 1.15; margin: 0 0 8px 0;"),
          
          hr(style = "margin: 18px 0;"),
          
          h5("ADMISSIONS", style = "font-size: 0.95rem; font-weight: 500; margin: 6px 0;"),
          
          #  tooltip(
          #    selectInput(
          #     "preset",
          #      label = span(
          #        "Select Scenario or adjust slider:",
          #        style = "font-size: 0.85rem;"
          #      ), 
          #      choices = c("Do nothing", "Planned", "Ambitious")), # don't think we need custom as can overwrite a preset
          #    "Choose a preset scenario or adjust sliders to customise assumptions",
          #   placement = "right"),
          
          div(style = "display:flex; justify-content:space-between; margin-top: 10px;",
              strong("Annual Change (%):",style = "font-size: 0.8rem; font-weight: 600;")
          ),
          
          sliderInput(
            "admissions_change",
            label = NULL, min = -5, max = 5, value = historic_trends$admissions, step = 0.1),
          
          # div(style = "display:flex; justify-content:space-between; font-size:0.75rem; margin-top:8px;",
          #     span("2026: 16967214"),
          #     span("2035: 16967214") ),
          

          # div(
          #   style = "display:flex; justify-content:space-between; font-size:0.75rem; margin-top:8px;",
          #   #span(paste0("2025: ", round(from_93$`All admissions`[from_93$Year == 2025], 0))),
          #   #Add commas: 
          #   span(
          #     paste0(
          #       "2025: ",
          #       format(
          #         round(from_93$`All admissions`[from_93$Year == 2025], 0),
          #         big.mark = ",",
          #         scientific = FALSE
          #       )
          #     )
          #   ),
          #   span(textOutput("future_beds_admissions_2035", inline = TRUE))
          # ),
          
          hr(style = "margin: 18px 0;"),
          
          h5("LENGTH OF STAY", style = "font-size: 0.95rem; font-weight: 500; margin: 6px 0;"),
          
          div(
            style = "display:flex; justify-content:space-between;",
            strong("Annual Change (%):",style = "font-size: 0.8rem; font-weight: 600;")
          ),
          
          sliderInput(
            "los_change",
            label = NULL,
            min = -5,
            max = 5,
            value = historic_trends$los,
            step = 0.1
          ),
          
          #div(
          #  style = "display:flex; justify-content:space-between; font-size:0.75rem; margin-top:8px;",
          #  span("2026: 3.12 days"),
          #  span("2035: 3.12 days")
          # ),
          

          # div(
          #   style = "display:flex; justify-content:space-between; font-size:0.75rem; margin-top:8px;",
          #   span(paste0("2025: ", round(from_93$avgLoS[from_93$Year == 2025], 2), " days")),
          #   span(textOutput("future_beds_los_2035", inline = TRUE))
          # ),

          
          hr(style = "margin: 18px 0;"),
          
          h5("TARGET BED OCCUPANCY", style = "font-size: 0.95rem; font-weight: 500; margin: 6px 0;"),
          
          div(
            style = "display:flex; justify-content:space-between;",
            strong("Fixed Value:",style = "font-size: 0.8rem; font-weight: 600;")
          ),
          
          sliderInput(
            "target_occupancy",
            label = NULL,
            min = 75,
            max = 100,
            value = 88.9,
            step = 0.1
          ),
          
          #div(
          #  style = "display:flex; justify-content:space-between; font-size:0.75rem; margin-top:-6px;",
          #  span("0%"),
          #  span("100%")
          #),
          

          # div(
          #   style = "display:flex; justify-content:space-between; font-size:0.75rem; margin-top:8px;",
          #   span(paste0("2025: ", round(from_93$occupancy[from_93$Year == 2025] * 100, 1), "%")),
          #   span(textOutput("future_beds_occupancy_2035", inline = TRUE))
          # ),

          
          actionButton(
            "reset_scenario", 
            "Reset to Historic Projections", 
            class = "btn-primary w-100",
            style = "margin-top: 10px;"
          ),
          
       #   div(
       #     style = "
        #    border: 1px solid #f9bf07;
        #    background-color: #fff8e1;
        #    padding: 8px;
        #    margin-top: 8px;
        #    font-size: 0.75rem;
        #    line-height: 1.15;
        #  ",
        #    "Beds required are calculated automatically from the assumptions."
        #  )
        )
      ),
      
      div(
        style = "padding-left: 10px;",
        
        #h3("Admissions Analysis", style = "margin-bottom: 4px;"),
        #hr(style = "margin: 4px 0 8px 0;"),
        
        # Yellow explanatory box ------------------------------------------------
        # div(
        #   style = "
        #     border: 2px solid #f9bf07;
        #     background-color: #fff8e1;
        #     padding: 6px 10px;
        #     margin-bottom: 8px;
        #     font-size: 0.8rem;
        #     line-height: 1.2;
        #   ",
        #   div(HTML("<b>Historical data (1994-2025)</b> shown in solid lines. <b>Predicted data (2026-2035)</b> shown in dashed lines, based on assumptions in the sidebar.")),
        #   div(HTML("<b>What this is:</b> A headline view of admissions, LoS, beddays, beds required and occupancy over time.")),
        #   div(HTML("<b>Why it is useful:</b> Shows how changing assumptions affects future pressure on beds.")),
        #   div(HTML("<b>How to use it:</b> Adjust the sidebar inputs and compare the projected direction and 2035 values."))
        # ),
        
        ### Chart and interpretation layout
        div(
          style = "
          display: grid;
          grid-template-columns: 1fr 1fr 1fr;
          grid-template-rows: 32vh 55vh;
          gap: 8px;
          height: 80vh;
        ",
          
          card(
            style = "height: 100%;",
            card_header(uiOutput("admissions_header")),
            card_body(
              plotlyOutput("admissions", height = "100%"),
              padding = 8,
              style = "height: calc(100% - 48px); overflow: hidden;"
            )
          ),
          
          card(
            style = "height: 100%;",
            card_header(uiOutput("los_header")),
            card_body(
              plotlyOutput("los", height = "100%"),
              padding = 8,
              style = "height: calc(100% - 48px); overflow: hidden;"
            )
          ),
          
          card(
            style = "height: 100%;",
            card_header(uiOutput("occupancy_header")),
            card_body(
              plotlyOutput("occupancy", height = "100%"),
              padding = 8,
              style = "height: calc(100% - 48px); overflow: hidden;"
            )
          ),
          
          card(
            style = "grid-column: 1 / span 2; height: 100%;",
            card_header(
              uiOutput("beds_header"),
              style = "
              background-color: #fff8e1;
              border-bottom: 1px solid #f9bf07;
              font-weight: 600;
            "
            ),
            card_body(
              plotlyOutput("beds", height = "100%"),
              padding = 8,
              style = "height: calc(100% - 48px); overflow: hidden;"
            )
          ),
          
          div(
            style = "
            display: grid;
            grid-template-rows: 1fr;
            gap: 8px;
            height: 100%;
          ",
            
            card(
              style = "height: 100%;",
              card_header(HTML("Scenario Interpretation")),
              card_body(
                #p("Historically reductions in LoS have allowed continual reductions in the number of beds, despite rising admissions."),
                p("Here we consider the impact of future admission and LoS scenarios on the number of beds."),uiOutput("future_beds_interpretation"),
                padding = 10,
                style = "height: calc(100% - 48px); overflow-y: auto;"
              )
            )
          )
        )
      )
    )
  ),
  
  ## Panel 3: Admissions Calculator nav panel ---------------------------------------
  nav_panel(
    "Admissions Calculator",
    
    layout_sidebar(
      sidebar = sidebar(
        title = NULL,
        id = "sidebar_fixed_beds",
        width = "300px",
        open = "always",
        
        div(
          style = "margin-top: 18px;",
          
          h3(
            "Future Assumptions",
            style = "font-size: 1.2rem; margin: 0 0 6px 0;"
          ),
          
          hr(style = "margin: 18px 0;"),
          
          p(
            "Set assumptions for future beds, length of stay and bed occupancy over the next 10 years, and the supported admissions are calculated.",
            style = "font-size: 0.85rem; line-height: 1.15; margin: 0 0 8px 0;"
          ),
          
          hr(style = "margin: 18px 0;"),
          
          h5("BED SUPPLY", style = "font-size: 0.95rem; margin: 6px 0;"),
          
          div(
            style = "display:flex; justify-content:space-between; font-size:0.8rem;",
            strong("Annual Change (%):")
          ),
          
          sliderInput(
            "bedday_growth", 
            label = NULL, 
            min = -5, 
            max = 5,  
            value = historic_trends$beds, 
            step = 0.1
          ),
          

          # div(
          #   style = "display:flex; justify-content:space-between; font-size:0.75rem; margin-top:8px;",
          #   span(
          #     paste0(
          #       "2025: ",
          #       format(
          #         round(from_93$Beds[from_93$Year == 2025], 0),
          #         big.mark = ",",
          #         scientific = FALSE
          #       )
          #     )
          #   ),
          #   span(textOutput("future_admissions_beds_2035", inline = TRUE))
          # ),

          
          hr(style = "margin: 18px 0;"),
          
          h5("LENGTH OF STAY", style = "font-size: 0.95rem; margin: 6px 0;"),
          
          div(
            style = "display:flex; justify-content:space-between; font-size:0.8rem;",
            strong("Annual Change (%):")
          ),
          
          sliderInput(
            "los_change2",
            NULL,
            min = -5,
            max = 5,
            value = historic_trends$los,
            step = 0.1,
            width = "100%"
          ),
          

          # div(
          #   style = "display:flex; justify-content:space-between; font-size:0.75rem; margin-top:8px;",
          #   span(paste0("2025: ", round(from_93$avgLoS[from_93$Year == 2025], 2), " days")),
          #   span(textOutput("future_admissions_los_2035", inline = TRUE))
          # ),

          
          hr(style = "margin: 18px 0;"),
          
          h5("TARGET BED OCCUPANCY", style = "font-size: 0.95rem; margin: 6px 0;"),
          
          div(
            style = "display:flex; justify-content:space-between; font-size:0.8rem;",
            strong("Fixed Value:")
          ),
          
          sliderInput(
            "bed_occupancy",
            NULL,
            min = 75,
            max = 100,
            value = 88.9,
            step = 0.5,
            width = "100%"
          ),
          

          # div(
          #   style = "display:flex; justify-content:space-between; font-size:0.75rem; margin-top:8px;",
          #   span(paste0("2025: ", round(from_93$occupancy[from_93$Year == 2025] * 100, 1), "%")),
          #   span(textOutput("future_admissions_occupancy_2035", inline = TRUE))
          # ),

          
          actionButton(
            "reset_scenario", 
            "Reset to Historic Projections", 
            class = "btn-primary w-100",
            style = "margin-top: 10px;"
          ),
          
      #    div(
      #      style = "
      #      border: 1px solid #f9bf07;
      #      background-color: #fff8e1;
      #      padding: 8px;
      #      margin-top: 8px;
      #      font-size: 0.75rem;
     #       line-height: 1.15;
     #     ",
     #       "Admissions capacity is calculated from the selected beds, LoS and target bed occupancy assumptions."
     #     )
        )
      ),
      
      div(
        style = "padding-left: 10px;",
        
        # h3("Admissions Calculator", style = "margin-bottom: 4px;"),
        # hr(style = "margin: 4px 0 8px 0;"),
        
        ### Chart and interpretation layout
        div(
          style = "
          display: grid;
          grid-template-columns: 1fr 1fr 1fr;
          grid-template-rows: 32vh 55vh;
          gap: 8px;
          height: 80vh;
        ",
          
          card(
            style = "height: 100%;",
            card_header(uiOutput("beds2_header")),
            card_body(
              plotlyOutput("beds2", height = "100%"),
              padding = 8,
              style = "height: calc(100% - 48px); overflow: hidden;"
            )
          ),
          
          card(
            style = "height: 100%;",
            card_header(uiOutput("los2_header")),
            card_body(
              plotlyOutput("los2", height = "100%"),
              padding = 8,
              style = "height: calc(100% - 48px); overflow: hidden;"
            )
          ),
          
          card(
            style = "height: 100%;",
            card_header(uiOutput("occupancy2_header")),
            card_body(
              plotlyOutput("occupancy2", height = "100%"),
              padding = 8,
              style = "height: calc(100% - 48px); overflow: hidden;"
            )
          ),
          
          card(
            style = "grid-column: 1 / span 2; height: 100%;",
            card_header(
              uiOutput("admissions2_header"),
              style = "
              background-color: #fff8e1;
              border-bottom: 1px solid #f9bf07;
              font-weight: 600;
            "
            ),
            card_body(
              plotlyOutput("admissions2", height = "100%"),
              padding = 8,
              style = "height: calc(100% - 48px); overflow: hidden;"
            )
          ),
          
          div(
            style = "
            display: grid;
            grid-template-rows: 1fr;
            gap: 8px;
            height: 100%;
          ",
            
            card(
              style = "height: 100%;",
              card_header(HTML("Scenario Interpretation")),
              card_body(
                #p("Historically reductions in LoS have allowed continual reductions in the number of beds, despite rising admissions."),
                p("Here we consider the impact of future planned bed supply and LoS scenarios on the number of admissions that could be supported."),uiOutput("future_admissions_interpretation"),
                padding = 10,
                style = "height: calc(100% - 48px); overflow-y: auto;"
              )
            )
          )
        )
      )
    )
  ),
  
  ## Panel 4: Explainer ---------------------------------------------------------
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
        h2("Hospital Admissions Tool", style = "margin: 0 0 6px 0;"),
        p(
          "This interactive tool allows exploration of the relationship between hospital admissions, length of stay (LoS), bed occupancy and available beds.",
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
                  "Historically reduction in LoS has allowed for reduction in bed capacity despite growing admissions. However, there is a limit to the amount LoS can reduce, and if admissions continue to grow bed capacity must also to maintain system performance.",
                  style = "margin: 0 0 6px 0;"
                ),
                p(
                  "This app allows the user to build intuition by:",
                  style = "margin: 0 0 6px 0;"
                ),
                tags$ul(
                  style = "margin: 0; padding-left: 20px;",
                  tags$li("Viewing how admissions, available beds, average length of stay and bed occupancy have changed over the past 30+ years (1994–2025)."),
                  tags$li("How changes in admissions could affect future bed requirements (Future Beds Calculator)."),
                  tags$li("How many admissions could be supported by future growth in bed capacity (Future Admissions Calculator)."),
                  #tags$li("Relationship between admissions, bed capacity, length of stay and bed occupancy.")
                  
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
                p("Use the sliders on the sidebar to set your input assumptions. The model will automatically update the charts based on your selections."),
                p("In the future beds calculator the user inputs their own assumptions about the future growth of admissions, length of stay and set target bed occupancy to be maintained across the next 10 years (2026-2035)."),
                p("In the future admissions calculator the user inputs their own assumptions of how bed capacity and length of stay will change to see how many admissions could be supported under a given occupancy target."),
                #p("Sliders control the following:"),
                #tags$ul(
                #  style = "margin: 0; padding-left: 20px;",
                #  tags$li("Annual % change in admissions/bed capacity over the next 10 years (2026-2035)."),
                #  #tags$li("The Future Bed Calculator gives 3 preset scenarios for admission growth, or the sliders can be adjusted to input your own value."),
                #  tags$li("Annual % change in length of stay over the next 10 years (2026-2035)."),
                #  tags$li("Annual % change in bed capacity over the next 10 years (2026-2035)."),
                #  tags$li("Target bed occupancy (%) you want to maintain.")
                #)
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
                h5("Step 2: Explore Projections", style = "margin: 0 0 4px 0;"),
                p("Four trend charts are displayed on each calculator tab illustrating number of admissions, average length of stay, number of beds and target bed occupancy over time. Charts allow a user to:"),
                tags$ul(
                  style = "margin: 0; padding-left: 20px;",
                  #tags$li("Number of admissions, Average length of stay, Number of beds and Target bed occupancy."),
                  tags$li("View historic trends (solid black line) and projected trends (red dotted line) from 2026-2035 (based on the selected assumptions)."),
                  tags$li("View pre-set admission scenarios (3 dotted grey lines) under do nothing, planned and ambitious admission strategies."),
                  #tags$li("Adjust the sliders to watch trends change in real time."),
                  tags$li("Hover over the chart lines to read off yearly values. Actual values for 2025 and projected values for 2035 are given on the sidebar.")
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
  
  ## Panel 5: Methodology -----------------------------------------------------
  nav_panel(
    "Methodology",
    
    div(
      style = "width: 88vw; max-width: none; margin: 0 auto;",
      
      card(
        card_header("Model Overview"),
        div(
          style = "padding: 4px;",
          p("This model approximates the relationship between bed capacity, patient admissions, length of stay (LoS), and occupancy rates."),
          p("We use well known theory from the M/G/infinity queueing model to derive the number of beds required to meet target occupancy given assumed admission and length of stay scenarios. This model assumes unconstrained capacity and Poisson admissions. Viewing the system as unconstrained allows us to examine the occupancy distribution and set the number of beds equal to the target quantile of occupancy."),
          p("The main result in use is that for an M/G/infinity system the expected number in the system follows a Poisson distribution with mean equal to the arrival rate multiplied by the average length of stay:"),
          div(
            style = "
            border: 2px solid #5881c1;
            background-color: #f4f4f4;
            padding: 12px;
            margin: 10px 0 16px 0;
            font-family: monospace;
          ",
            "λ = Arrival rate (admissions per day) * μ = Average length of stay (days) = Expected number of patients in the system E_NIS"
          ),
          p("we therefore derive the number of beds required to meet a target occupancy level (p) as the p quantile of a Poisson distribution with mean E_NIS."),
          p("This model is designed for intuition building and is not a forecasting tool. It is not designed to give precise estimates of future bed requirements or admissions capacity, but rather to allow users to explore the relationship between these variables and build intuition about how they interact."),
          p("In reality the assumption of Poisson admissions may not hold, and other factors such as seasonal variations, changes in patient demographics, and healthcare policies can impact admission rates. Additionally, the model assumes that bed capacity is unconstrained, which may not reflect real-world limitations and their impact on patient flow and occupancy."),

          p("Parameters:"),
          tags$ul(
            tags$li("Admissions: Number of hospital admissions per year (converted to daily rate for the model)."),
            tags$li("Length of stay: Average number of days in hospital for each admission."),
            tags$li("Beds: Number of available beds at year end (to fit with historical data)."),
            tags$li("Target bed occupancy: % of available beds that are occupied at a given time.")
          ),
          tags$ul(
            tags$li("Change in number of admissions: Assumed % annual change in the number of hospital admissions"),
            tags$li("Change in average length of stay: Assumed % annual change in length of stay"),
            tags$li("Change in number of beds: Assumed % annual growth in the number of available beds")
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
            p("The historic annual number of admissions and average length of stays between 1994 and 2025 were derived from the Hospital Episode Statistics dataset."),
            p("The historic number of beds and bed occupancy was taken from the NHS England Bed Availability and Occupancy (KH03) Collection. Day and overnight beds were combined and the number of beds was taken from Q4 each year."),
            p("The preset admission scenarios (Do nothing, Planned and Ambitious) were derived using The Strategy Unit's NHP Model. Details can be found in the Future Demand for Community Care report (see references).")
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
          tags$ul(
            tags$li("Admissions per day are calculated by dividing annual admissions by 365, assuming a constant rate throughout the year. This ignores seasonal variation."),
            tags$li("Average LoS was estimated using beddays from HES under the assumption that same day admissions take on average 0.2 of a day (5 hours)."),
            tags$li("The historical number of beds was taken from Q4 each year, which may not reflect the average number of beds across the year."),
            tags$li("The historical occpancy is calculated as the historic beddays divided by the number of available beddays given the number of beds. This assumes that bed occupancy is constant across the year and does not reflect seasonal variation. It also doesn't align exactly with the occupancy reported in the NHS England Bed Availability and Occupancy (KH03) Collection."),
          ),
          p(strong("This tool is designed for intuition building only, and not for strategic planning or operational decisions."))
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
                    "Strategy Unit: Future Demand for Community Care",
                    href = "https://www.strategyunitwm.nhs.uk/publications/missing-element-shifting-care",
                    target = "_blank"
                  )),
                
                #tags$li(
                #  tags$a(
                #    "The Nuffield Trust: Hospital Bed Occupancy",
                #    href = "https://www.nuffieldtrust.org.uk/resource/hospital-bed-occupancy",
                #    target = "_blank"
                #  ))
              ))))),
      
    )
  )
  
)


#Server interface --------------------------------------------------------------

server <- function(input, output, session) {
  
  
  observeEvent(input$reset_scenario, {
    updateSliderInput(session, "admissions_change", value = historic_trends$admissions)
    updateSliderInput(session, "los_change", value = historic_trends$los)
    updateSliderInput(session, "target_occupancy", value = 88.9)
  })
  
  observeEvent(input$reset_baseline, {
    updateSliderInput(session, "bedday_growth", value = historic_trends$beds) 
    updateSliderInput(session, "los_change2", value = historic_trends$los) 
    updateSliderInput(session, "bed_occupancy", value = 88.9) 
  })
  
  format_indicator_value <- function(value, suffix = "", digits = 1) {
    paste0(round(value, digits), suffix)
  }
  
  get_indicator_direction <- function(default_value, selected_value, digits = 1) {
    
    default_value <- round(default_value, digits)
    selected_value <- round(selected_value, digits)
    
    change <- selected_value - default_value
    
    if(change > 0.00001){
      return(list(icon = "▲", colour = "#ec6555", change = change))
    }
    
    if(change < -0.00001){
      return(list(icon = "▼", colour = "#238b45", change = change))
    }
    
    return(list(icon = "=", colour = "#686f73", change = change))
  }
  
  format_direction_value <- function(default_value,
                                     selected_value,
                                     suffix = "%",
                                     digits = 1) {
    
    direction <- get_indicator_direction(default_value, selected_value)
    
    icon <- if(direction$icon == "▲"){
      "▲"
    } else if(direction$icon == "▼"){
      "▼"
    } else {
      "="
    }
    
    HTML(
      paste0(
        "<span style='color:",
        direction$colour,
        "; font-weight:800;'>",
        icon,
        " ",
        round(selected_value, digits),
        suffix,
        "</span>"
      )
    )
  }
  
  make_compare_badge <- function(default_value, selected_value, suffix = "", digits = 1) {
    
    direction <- get_indicator_direction(default_value, selected_value, digits)
    
    HTML(paste0(
      "<span style='color:", direction$colour, "; font-weight:900;'>",
      direction$icon,
      " ",
      format_indicator_value(selected_value, suffix, digits),
      "</span>"
    ))
  }
  
  make_change_badge <- function(default_value, selected_value, suffix = "", digits = 1) {
    
    direction <- get_indicator_direction(default_value, selected_value)
    change <- selected_value - default_value
    
    HTML(paste0(
      "<span style='color:", direction$colour, "; font-weight:900;'>",
      direction$icon,
      " ",
      ifelse(change > 0, "+", ""),
      round(change, digits),
      suffix,
      "</span>"
    ))
  }
  
  make_chart_header <- function(title, badge) {
    
    div(
      style = "
        display: flex;
        justify-content: space-between;
        align-items: center;
        width: 100%;
        gap: 10px;
      ",
      span(title, style = "font-weight: 600;"),
      span(
        badge,
        style = "
          font-weight: 700;
          font-size: 0.95rem;
          white-space: nowrap;
        "
      )
    )
  }
  
  make_metric_block <- function(default_value,
                                selected_value,
                                label,
                                suffix = "",
                                digits = 1,
                                show_change = TRUE) {
    
    direction <- get_indicator_direction(default_value, selected_value, digits)
    
    div(
      style = "
      text-align: center;
      flex: 1;
    ",
      
      if(show_change){
        div(
          style = paste0(
            "font-size:1.8rem;",
            "font-weight:800;",
            "color:", direction$colour, ";",
            "line-height:1;",
            "margin-bottom:6px;"
          ),
          HTML(
            paste0(
              direction$icon,
              " ",
              round(direction$change, 1),
              "%"
            )
          )
        )
      },
      
      div(
        HTML(paste0(
          "<span style='font-weight:700;'>",
          format_indicator_value(default_value, suffix, digits),
          "</span>",
          " <span style='font-size:1.5em; vertical-align:-0.08em;'>&rarr;</span> ",
          "<span style='font-weight:800; color:", direction$colour, ";'>",
          direction$icon,
          " ",
          format_indicator_value(selected_value, suffix, digits),
          "</span>"
        )),
        style = "line-height: 1.1;"
      ),
      
      div(
        HTML(label),
        style = "
        font-size: 0.75rem;
        line-height: 1.15;
        margin-top: 5px;
      "
      )
    )
  }
  
  # Chart outputs-----------------------------------------------------------------
  
  
  output$admissions<-renderPlotly({
    
    plot_data<-future_beds_model(from_93, input$admissions_change, input$los_change, input$target_occupancy)
    
    plotting_function(plot_data,
                      NULL,
                      NULL,
                      NULL,
                      "admissions", 
                      "Admissions (millions)")
    
  })
  
  output$los<-renderPlotly({
    
    plot_data<-future_beds_model(from_93, input$admissions_change, input$los_change, input$target_occupancy)
    
    plotting_function(plot_data,
                      NULL,
                      NULL,
                      NULL,
                      "los", 
                      "Length of Stay (days)")
    
  })
  
  output$beds<-renderPlotly({
    
    plot_data<-future_beds_model(from_93, input$admissions_change, input$los_change, input$target_occupancy)
    
    plotting_function(plot_data,
                      NULL,
                      NULL,
                      NULL,
                      "beds",
                      "Beds (thousands)", 
                      show_trend_labels = TRUE)
    
  })
  
  
  output$occupancy<-renderPlotly({
    
    plot_data<-future_beds_model(from_93, input$admissions_change, input$los_change, input$target_occupancy)
    
    plotting_function(plot_data,
                      NULL,
                      NULL,
                      NULL,
                      "occupancy", 
                      "Bed Occupancy (%)")
    
  })
  
  
  
  output$admissions2<-renderPlotly({
    
    plot_data<-future_admissions_model(from_93, input$bedday_growth, input$los_change2, input$bed_occupancy)
    
    plotting_function(plot_data,
                      do_nothing,
                      planned,
                      ambitious,
                      "admissions", 
                      "Admissions (millions)", 
                      show_trend_labels = TRUE)
    
  })
  
  output$los2<-renderPlotly({
    
    plot_data<-future_admissions_model(from_93, input$bedday_growth, input$los_change2, input$bed_occupancy)
    
    plotting_function(plot_data,
                      NULL,
                      NULL,
                      NULL,
                      "los", 
                      "Length of Stay (days)")
    
  })
  
  output$beds2<-renderPlotly({
    
    plot_data<-future_admissions_model(from_93, input$bedday_growth, input$los_change2, input$bed_occupancy)
    
    plotting_function(plot_data,
                      NULL,
                      NULL,
                      NULL,
                      "beds", 
                      "Beds (thousands)")
    
  })
  
  
  output$occupancy2<-renderPlotly({
    
    plot_data<-future_admissions_model(from_93, input$bedday_growth, input$los_change2, input$bed_occupancy)
    
    plotting_function(plot_data,
                      NULL,
                      NULL,
                      NULL,
                      "occupancy", 
                      "Bed Occupancy (%)")
    
  })
  
  #Adding changing values to sidebars
  output$future_beds_admissions_2035 <- renderText({
    
    plot_data <- future_beds_model(
      from_93,
      input$admissions_change,
      input$los_change,
      input$target_occupancy
    )
    
    admissions_2035 <- plot_data$admissions[plot_data$year == 2035] * 1000000
    
    paste0(
      "2035: ",
      format(
        round(admissions_2035, 0),
        big.mark = ",",
        scientific = FALSE
      )
    )
  })
  
  output$future_beds_los_2035 <- renderText({
    
    plot_data <- future_beds_model(
      from_93,
      input$admissions_change,
      input$los_change,
      input$target_occupancy
    )
    
    los_2035 <- plot_data$los[plot_data$year == 2035]
    
    paste0("2035: ", round(los_2035, 2), " days")
  })
  
  output$future_admissions_beds_2035 <- renderText({
    
    plot_data <- future_admissions_model(
      from_93,
      input$bedday_growth,
      input$los_change2,
      input$bed_occupancy
    )
    
    beds_2035 <- plot_data$beds[plot_data$year == 2035]
    
    paste0(
      "2035: ",
      format(
        round(beds_2035, 0),
        big.mark = ",",
        scientific = FALSE
      )
    )
  })
  
  output$future_admissions_los_2035 <- renderText({
    
    plot_data <- future_admissions_model(
      from_93,
      input$bedday_growth,
      input$los_change2,
      input$bed_occupancy
    )
    
    los_2035 <- plot_data$los[plot_data$year == 2035]
    
    paste0("2035: ", round(los_2035, 2), " days")
  })
  
  output$future_beds_occupancy_2035 <- renderText({
    paste0("2026-2035: ", round(input$target_occupancy, 1), "%")
  })
  
  output$future_admissions_occupancy_2035 <- renderText({
    paste0("2026-2035: ", round(input$bed_occupancy, 1), "%")
  })
  
  # Adding changing values to interpretation cards
  
  output$future_beds_interpretation <- renderUI({
    
    plot_data <- future_beds_model(
      from_93,
      input$admissions_change,
      input$los_change,
      input$target_occupancy
    )
    
    default_plot_data <- future_beds_model(
      from_93,
      historic_trends$admissions,
      historic_trends$los,
      88.9
    )
    
    beds_2035 <- plot_data$beds[plot_data$year == 2035]
    default_beds_2035 <- default_plot_data$beds[default_plot_data$year == 2035]
    
    div(
      div(
        class = "infographic-wrap",
        
        div(class = "info-arrow info-arrow-left"),
        div(class = "info-arrow info-arrow-right"),
        div(class = "info-arrow info-arrow-bottom"),
        
        div(
          class = "info-node info-node-blue",
          div(
            class = "info-value",
            make_compare_badge(
              historic_trends$admissions,
              input$admissions_change,
              "%",
              digits = 1
            )
          ),
          div(class = "info-label", "Admissions"),
          div(class = "info-sublabel", "(annual change)")
        ),
        
        div(
          class = "info-node info-node-grey",
          div(
            class = "info-value",
            make_compare_badge(
              historic_trends$los,
              input$los_change,
              "%",
              digits = 1
            )
          ),
          div(class = "info-label", "LoS"),
          div(class = "info-sublabel", "(annual change)")
        ),
        
        div(
          class = "info-node info-node-yellow",
          div(
            class = "info-value",
            make_compare_badge(
              88.9,
              input$target_occupancy,
              "%",
              digits = 1
            )
          ),
          div(class = "info-label", "Target Occupancy"),
          div(class = "info-sublabel", "(fixed value)")
        ),
        
        div(
          class = "info-output",
          div(
            class = "bed-icon",
            icon("bed")
          ),
          div(class = "output-label", "BEDS REQUIRED"),
          div(
            class = "output-value",
            make_compare_badge(
              default_beds_2035,
              beds_2035,
              "k",
              digits = 0
            )
          )
        )
      ),
      
      hr(style = "margin: 4px 0 8px 0;"),
      
      p(
        "These three inputs interact to determine the number of beds required.",
        style = "font-size:0.8rem; margin:0; line-height:1.2;"
      )
    )
  })
  
  
  output$future_admissions_interpretation <- renderUI({
    
    plot_data <- future_admissions_model(
      from_93,
      input$bedday_growth,
      input$los_change2,
      input$bed_occupancy
    )
    
    default_plot_data <- future_admissions_model(
      from_93,
      historic_trends$beds,
      historic_trends$los,
      88.9
    )
    
    admissions_2035 <- plot_data$admissions[plot_data$year == 2035]
    default_admissions_2035 <- default_plot_data$admissions[default_plot_data$year == 2035]
    
    div(
      div(
        class = "panel3-smart-wrap",
        
        div(
          class = "panel3-smart-inputs",
          
          div(
            class = "panel3-smart-input panel3-smart-blue",
            div(
              class = "panel3-smart-value",
              make_compare_badge(
                historic_trends$beds,
                input$bedday_growth,
                "%",
                digits = 1
              )
            ),
            div(class = "panel3-smart-label", "Bed supply"),
            div(class = "panel3-smart-sublabel", "(annual change)")
          ),
          
          div(
            class = "panel3-smart-input panel3-smart-grey",
            div(
              class = "panel3-smart-value",
              make_compare_badge(
                historic_trends$los,
                input$los_change2,
                "%",
                digits = 1
              )
            ),
            div(class = "panel3-smart-label", "LoS"),
            div(class = "panel3-smart-sublabel", "(annual change)")
          ),
          
          div(
            class = "panel3-smart-input panel3-smart-yellow",
            div(
              class = "panel3-smart-value",
              make_compare_badge(
                88.9,
                input$bed_occupancy,
                "%",
                digits = 1
              )
            ),
            div(class = "panel3-smart-label", "Target Occupancy"),
            div(class = "panel3-smart-sublabel", "(fixed value)")
          )
        ),
        
        tags$svg(
          class = "panel3-smart-flow",
          viewBox = "0 0 170 175",
          preserveAspectRatio = "none",
          
          tags$path(
            d = "M0,24 C45,18 95,34 170,70",
            stroke = "#5881c1",
            `stroke-width` = "24",
            fill = "none",
            opacity = "0.28",
            `stroke-linecap` = "round"
          ),
          
          tags$path(
            d = "M0,87 C55,87 110,87 170,87",
            stroke = "#686f73",
            `stroke-width` = "24",
            fill = "none",
            opacity = "0.28",
            `stroke-linecap` = "round"
          ),
          
          tags$path(
            d = "M0,150 C45,150 95,134 170,105",
            stroke = "#f9bf07",
            `stroke-width` = "24",
            fill = "none",
            opacity = "0.34",
            `stroke-linecap` = "round"
          )
        ),
        
        div(
          class = "panel3-smart-output",
          div(class = "bed-icon", icon("chart-line")),
          div(class = "panel3-smart-output-label", HTML("SUPPORTED<br>ADMISSIONS")),
          div(
            class = "panel3-smart-output-value",
            make_compare_badge(
              default_admissions_2035,
              admissions_2035,
              "M",
              digits = 1
            )
          )
        )
      ),
      
      hr(style = "margin: 6px 0 8px 0;"),
      
      p(
        "These three inputs interact to determine the number of admissions that could be supported.",
        class = "panel3-smart-note"
      )
    )
  })
  
  output$admissions_header <- renderUI({
    make_chart_header(
      "Number of Admissions (millions)",
      make_compare_badge(historic_trends$admissions, input$admissions_change, "%")
    )
  })
  
  output$los_header <- renderUI({
    make_chart_header(
      "Average Length of Stay (days)",
      make_compare_badge(historic_trends$los, input$los_change, "%")
    )
  })
  
  output$occupancy_header <- renderUI({
    make_chart_header(
      "Bed Occupancy Rate (%)",
      make_compare_badge(88.9, input$target_occupancy, "%")
    )
  })
  
  output$beds_header <- renderUI({
    
    plot_data <- future_beds_model(
      from_93,
      input$admissions_change,
      input$los_change,
      input$target_occupancy
    )
    
    default_plot_data <- future_beds_model(
      from_93,
      historic_trends$admissions,
      historic_trends$los,
      88.9
    )
    
    beds_2035 <- plot_data$beds[plot_data$year == 2035]
    default_beds_2035 <- default_plot_data$beds[default_plot_data$year == 2035]
    
    make_chart_header(
      "Beds Required (thousands)",
      make_compare_badge(default_beds_2035, beds_2035, "k", digits = 0)
    )
  })
  
  output$beds2_header <- renderUI({
    make_chart_header(
      "Number of Beds (thousands)",
      make_compare_badge(historic_trends$beds, input$bedday_growth, "%")
    )
  })
  
  output$los2_header <- renderUI({
    make_chart_header(
      "Average Length of Stay (days)",
      make_compare_badge(historic_trends$los, input$los_change2, "%")
    )
  })
  
  output$occupancy2_header <- renderUI({
    make_chart_header(
      "Bed Occupancy Rate (%)",
      make_compare_badge(88.9, input$bed_occupancy, "%")
    )
  })
  
  output$admissions2_header <- renderUI({
    
    plot_data <- future_admissions_model(
      from_93,
      input$bedday_growth,
      input$los_change2,
      input$bed_occupancy
    )
    
    default_plot_data <- future_admissions_model(
      from_93,
      historic_trends$beds,
      historic_trends$los,
      88.9
    )
    
    admissions_2035 <- plot_data$admissions[plot_data$year == 2035]
    default_admissions_2035 <- default_plot_data$admissions[default_plot_data$year == 2035]
    
    make_chart_header(
      "Supported Admissions (millions)",
      make_compare_badge(default_admissions_2035, admissions_2035, "M")
    )
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)