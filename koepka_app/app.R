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
woods <- readRDS("~/Data/Final-Project/koepka_app/woods.rds")

sg_choices <- c("Total" = "total",
                "Off the Tee" = "ott",
                "Tee to Green" = "t2g",
                "Approach" = "app", 
                "Around Green" = "arg")

ui <- fluidPage(
   
   
   titlePanel("Koepka App"),
   
    
   sidebarLayout(
      sidebarPanel(
         selectInput("yaxis",
                     label = "Choose Strokes Gained Stat",
                     choices = sg_choices)
      ),
      
      
      mainPanel(
         plotOutput("myPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$myPlot <- renderPlot({
     
     ggplot(koepka, aes_string(x = 'event', y = input$yaxis, color = 'win')) + 
        geom_point() +
        labs(title = "Looking at World Number One Brooks Koepka's 2018 Season", 
             caption = "Data provided by PGA Tour", 
             x = "Event", 
             y = input$yaxis) +
       geom_label_repel(aes(label = event), size = 3, force = 3) +
       theme_bw() +
       theme_linedraw() +
       theme(axis.text.x=element_blank())
     
     ggplot(woods, aes_string(x = 'event', y = input$yaxis, color = 'win')) + 
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

