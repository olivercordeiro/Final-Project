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

koepka <- readRDS("~/Data/Final-Project/Players2018/koepka.rds")
johnson <- readRDS("~/Data/Final-Project/Players2018/johnson.rds")
rose <- readRDS("~/Data/Final-Project/Players2018/rose.rds")
woods <- readRDS("~/Data/Final-Project/Players2018/woods.rds")

sg_choices <- c("Total" = "total",
                "Off the Tee" = "ott",
                "Tee to Green" = "t2g",
                "Approach" = "app", 
                "Around Green" = "arg")

#
ui <- fluidPage(
  
 #  
  titlePanel("Fill out github excell"),
  
  #
  sidebarLayout(
    
    #
    sidebarPanel(
      
      #
      selectInput("yaxis",
                  label = "Choose Strokes Gained Stat",
                  choices = sg_choices)),
    #
    br(),
    
    #
    mainPanel(
      
      #
      tabsetPanel(type = "tabs",
                  tabPanel("Brooks Koepka", plotOutput("koepkaPlot")),
                  tabPanel("Justin Rose", plotOutput("rosePlot")), 
                  tabPanel("Dustin Johnson", plotOutput("johnsonPlot")),
                  tabPanel("Tiger Woods", plotOutput("woodsPlot"))
      )
      
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #
  output$koepkaPlot <- renderPlot({
    
    #
    koepka %>% 
      filter(!total %in% 0) %>% 
      ggplot(aes_string(x = 'event', y = input$yaxis, color = 'win')) + 
      geom_point() +
      labs(title = "Looking at World Number One Brooks Koepka's 2018 Season", 
           caption = "Data provided by the PGA Tour", 
           x = "Event", 
           y = input$yaxis, 
           color = "Win") +
      scale_color_manual(values = c("black","red")) +
      geom_label_repel(aes(label = event), size = 3, force = 3) +
      theme_bw() +
      theme_linedraw() +
      theme(axis.text.x = element_blank())
  })
  
  #
  output$johnsonPlot <- renderPlot({
    
    #
    johnson %>% 
      filter(!total %in% 0) %>% 
      ggplot(aes_string(x = 'event', y = input$yaxis, color = 'win')) + 
      geom_point() +
      labs(title = "Looking at World Number One Brooks Koepka's 2018 Season", 
           caption = "Data provided by the PGA Tour", 
           x = "Event", 
           y = input$yaxis, 
           color = "Win") +
      scale_color_manual(values = c("black","red")) +
      geom_label_repel(aes(label = event), size = 3, force = 3) +
      theme_bw() +
      theme_linedraw() +
      theme(axis.text.x = element_blank())
  })
  
  #
  output$rosePlot <- renderPlot({
    
    #
    rose %>% 
      filter(!total %in% 0) %>% 
      ggplot(aes_string(x = 'event', y = input$yaxis, color = 'win')) + 
      geom_point() +
      labs(title = "Looking at World Number One Brooks Koepka's 2018 Season", 
           caption = "Data provided by the PGA Tour", 
           x = "Event", 
           y = input$yaxis, 
           color = "Win") +
      scale_color_manual(values = c("black","red")) +
      geom_label_repel(aes(label = event), size = 3, force = 3) +
      theme_bw() +
      theme_linedraw() +
      theme(axis.text.x = element_blank())
  })
  
  #
  output$woodsPlot <- renderPlot({ 
    
    #
    ggplot(woods, aes_string(x = 'event', y = input$yaxis, color = 'win')) + 
      geom_point() +
      labs(title = "Looking at Tiger Woods' Comeback 2018 Season", 
           caption = "Data provided by the PGA Tour", 
           x = "Event", 
           y = input$yaxis, 
           color = "Win") +
      scale_color_manual(values = c("black","red")) +
      geom_label_repel(aes(label = event), size = 3, force = 3) +
      theme_bw() +
      theme_linedraw() +
      theme(axis.text.x = element_blank()) 
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


