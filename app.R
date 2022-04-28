#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:

#    http://shiny.rstudio.com/

#Shiny Output Link:
# https://wolned-agnes-sithole.shinyapps.io/FinalProjectResults


library(shiny)
library(shinyWidgets)
library(tidyverse)
library(ggthemes)
library(ggplot2)
library(plotly)
library(DT)

apartments <- read.csv("apartment_price_2021.csv")
bedroomspred <- read.csv("BedroomsPredvsActual.csv")
bedroomspred1 <- bedroomspred[c(2,3,4,5,6)]
results <- read.csv("modelResults.csv")

#Define User Interface
ui <- fluidPage(
  titlePanel("Bedroom Prediction Results"),
  sliderInput(inputId = "ActualBedrooms",
              label = "Bedrooms:",
              min = 0,
              max = 8,
              value = 1),
  fluidRow(
    column(8)),
  
# Show a plot of the generated distribution
mainPanel(
  tableOutput("table"),
  plotlyOutput("plot1")
))


# Define server logic required to draw PLOT
  server <- function(input, output){
    output$table <- renderTable(results)
    output$plot1 <- renderPlotly({
        plot1 <- bedroomspred1 %>%  
        filter(ActualBedrooms == input$ActualBedrooms) %>% 
        ggplot() + 
        geom_point(mapping = aes(x = ActualBedrooms, 
                                 y = PredictedBedrooms4,
                                 text = paste(
                                   "Actual Bedrooms:", ActualBedrooms,
                                   "\nPredicted Bedrooms:", PredictedBedrooms4,
                                   "\nBathrooms:", Bathrooms,
                                   "\nSqft Living:", Sqft_living, 
                                   "\nPrice:", dollar(Price)))) + 
          labs(title = "Bedrooms Actual vs Prediction",
             x = "Actual",
             y = "Prediction")
        ggplotly(plot1)
    })
  }

# Run the application 
shinyApp(ui = ui, server = server)
