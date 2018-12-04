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


# strokes gained rds
koepka <- readRDS("~/Data/Final-Project/Players2018/koepka.rds")
johnson <- readRDS("~/Data/Final-Project/Players2018/johnson.rds")
rose <- readRDS("~/Data/Final-Project/Players2018/rose.rds")
woods <- readRDS("~/Data/Final-Project/Players2018/woods.rds")

# fairways and greens rds
koepkaFG <- readRDS("~/Data/Final-Project/Players2018/koepkaFG.rds")
woodsFG <- readRDS("~/Data/Final-Project/Players2018/woodsFG.rds")
roseFG <- readRDS("~/Data/Final-Project/Players2018/roseFG.rds")
johnsonFG <- readRDS("~/Data/Final-Project/Players2018/johnsonFG.rds")

sg_choices <- c("Total" = "total",
                "Off the Tee" = "ott",
                "Tee to Green" = "t2g",
                "Approach" = "app", 
                "Around Green" = "arg")

# d
ui <- navbarPage("PGA Tour Data", 
  
 #  d
  tabPanel("Stroke Gained",
  
  # d
  sidebarLayout(
    
    # d
    sidebarPanel(
      
      # d
      selectInput("yaxis",
                  label = "Choose Strokes Gained Stat",
                  choices = sg_choices),
      h6("Using the box above you can select which strokes 
         gained statistic you want to view.")),
    
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
), 

  tabPanel("Fairways and Greens", 
    
    # d
    sidebarLayout(
             
      # d
      sidebarPanel(
               
        # d
        radioButtons("fairway", 
                     "Fairway Hit", 
                     choices = c("Hit", "Missed")
        ), 
        
        radioButtons("green", 
                     "Green Hit", 
                     choices = c("Hit", "Missed")
        )
      ), 
      
      mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("Brooks Koepka", plotOutput("koepkaFGPlot")),
                    tabPanel("Justin Rose", plotOutput("roseFGPlot")), 
                    tabPanel("Dustin Johnson", plotOutput("johnsonFGPlot")),
                    tabPanel("Tiger Woods", plotOutput("woodsFGPlot"))
        )
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
  output$rosePlot <- renderPlot({
    
    # 
    rose %>% 
      filter(!total %in% 0) %>% 
      ggplot(aes_string(x = 'event', y = input$yaxis, color = 'win')) + 
      geom_point() +
      labs(title = "Looking at World Number Two Justin Rose's 2018 Season", 
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
      labs(title = "Looking at World Number Three Dustin Johnson's 2018 Season", 
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
  
  output$koepkaFGPlot <- renderPlot({
    koepkaFG %>% 
      filter(hit_fwy %in% input$fairway, 
             hit_green %in% input$green, 
             rtp_score >= -2) %>% 
      ggplot(aes(x = rtp_score)) +
      geom_histogram(binwidth = .5) +
      labs(title = "Score to Par and Fairways and Greens Hit", 
           caption = "Data provided by the PGA Tour", 
           x = "Score to Par", 
           y = "") +
      theme_bw() +
      theme_linedraw()
      
  })
  
  output$roseFGPlot <- renderPlot({
    roseFG %>% 
      filter(hit_fwy %in% input$fairway, 
             hit_green %in% input$green, 
             rtp_score >= -2) %>% 
      ggplot(aes(x = rtp_score)) +
      geom_histogram(binwidth = .5) +
      labs(title = "Score to Par and Fairways and Greens Hit", 
           caption = "Data provided by the PGA Tour", 
           x = "Score to Par", 
           y = "") +
      theme_bw() +
      theme_linedraw()
    
    
  })
  
  output$johnsonFGPlot <- renderPlot({
    johnsonFG %>% 
      filter(hit_fwy %in% input$fairway, 
             hit_green %in% input$green, 
             rtp_score >= -2) %>% 
      ggplot(aes(x = rtp_score)) +
      geom_histogram(binwidth = .5) +
      labs(title = "Score to Par and Fairways and Greens Hit", 
           caption = "Data provided by the PGA Tour", 
           x = "Score to Par", 
           y = "") +
      theme_bw() +
      theme_linedraw()
    
  })
  
  output$woodsFGPlot <- renderPlot({
    woodsFG %>% 
      filter(hit_fwy %in% input$fairway, 
             hit_green %in% input$green, 
             rtp_score >= -2) %>% 
      ggplot(aes(x = rtp_score)) +
      geom_histogram(binwidth = .5) +
      labs(title = "Score to Par and Fairways and Greens Hit", 
           caption = "Data provided by the PGA Tour", 
           x = "Score to Par", 
           y = "") +
      theme_bw() +
      theme_linedraw()
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


