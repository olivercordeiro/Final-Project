#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggrepel)
library(tidyverse)

koepka <- readRDS("~/Data/Final-Project/koepka_app/koepka.rds")

ui <- fluidPage(
   
   
   titlePanel("Koepka App"),
   
    
   sidebarLayout(
      sidebarPanel(
         selectInput("yaxis",
                     label = "Choose Stat",
                     choices = c("t2g", "total", "arg", "app", "ott"))
      ),
      
      
      mainPanel(
         plotOutput("myPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$myPlot <- renderPlot({
     
     ggplot(koepka, aes_string(x = 'event', y = input$yaxis)) + 
        geom_point() +
        labs(title = "Looking at World Number One Brooks Koepka's 2018 Season", 
             caption = "Data provided by PGA Tour", 
             x = "Event", 
             y = input$yaxis) +
       geom_label_repel(aes(label = event), size = 3, force = 3) +
       theme_bw() +
       theme_linedraw() +
       theme(axis.text.x=element_blank()) 
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

