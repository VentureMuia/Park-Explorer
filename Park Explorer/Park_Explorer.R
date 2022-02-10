library(shiny)
library(shinythemes)
library(tidyverse)
library(plotly)
source("helpers.R")

# Import data
annual_visits <- read_rds("annual_visits.rds")
monthly_visits <- read_rds("monthly_visits.rds")

parks <- annual_visits$park_name %>% unique()

ui <- fluidPage(theme = shinytheme("sandstone"),
  titlePanel("National Park Visit Explorer"),
  sidebarLayout(
    sidebarPanel(selectInput("selected_park", "Park", choices = parks,
                             selected = "Crater Lake NP"),
                 sliderInput("selected_year", "Year", 
                             value = 2019,
                             min = min(annual_visits$year), 
                             max = max(annual_visits$year),
                             step = 1,
                             sep = "")
                 ),
    mainPanel(
      h1(textOutput("park_name")),
      tabsetPanel(type='pills',
                  tabPanel('Annual_Visits', fluidRow(
                    column(8,
                           plotlyOutput("annual_plot")
                    ),
                    column(4,
                           textOutput("park_summary"))
                  )),
                  tabPanel('Monthly_visits',fluidRow(
                    column(8, 
                           plotlyOutput("monthly_plot"),
                           checkboxInput("display_average", "Display monthly average")
                    ),
                    column(4, 
                           tableOutput("monthly_table"))
                  ))
      )
    )
  )
)

server <- function(input, output, session) {
  output$park_name <- renderText(input$selected_park)

  # Subset data to selected park --------------------------------------------
  
  annual_data <- reactive({
    annual_visits %>% 
      filter(park_name == input$selected_park)
  })
  
  monthly_data <- reactive({
    monthly_visits %>% 
      filter(park_name == input$selected_park) 
  })
  
  # Outputs that use a single year ------------------------------------------

  output$park_summary <- renderText({
    annual_data() %>% 
      filter(year == input$selected_year) %>% 
      summarize_park()
  })

  output$monthly_table <- renderTable(digits = 0, {
    monthly_data() %>% 
      filter(year == input$selected_year) %>% 
      select(month_name, recreation_visits)
  })
  

  # Outputs that use all years ----------------------------------------------

  output$annual_plot <- renderPlotly({
    g<-annual_data() %>% 
      plot_annual(highlight_year = input$selected_year)
    gg<-ggplotly(g)
    return(gg)
  })

  output$monthly_plot <- renderPlotly({
    m<-monthly_data() %>% 
      plot_monthly(display_average = input$display_average,
        highlight_year = input$selected_year)
    mm<-ggplotly(m)
    return(mm)
  })
  
}

shinyApp(ui, server)
