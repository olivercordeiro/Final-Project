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

# ~/Data/Final-Project/Players2018/
# strokes gained rds
koepka <- readRDS("koepka.rds")
johnson <- readRDS("johnson.rds")
rose <- readRDS("rose.rds")
woods <- readRDS("woods.rds")

# fairways and greens rds
koepkaFG <- readRDS("koepkaFG.rds")
woodsFG <- readRDS("woodsFG.rds")
roseFG <- readRDS("roseFG.rds")
johnsonFG <- readRDS("johnsonFG.rds")

sg_choices <- c("Total" = "total",
                "Off the Tee" = "ott",
                "Tee to Green" = "t2g",
                "Approach" = "app", 
                "Around Green" = "arg",
                "Putting" = "putt")

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
      h5("Use the box above to select which strokes 
         gained statistic you want to view."), 
      br(), 
      h6("The strokes gained statistic was developed by the Professor 
         Mark Broadie of Columbia University alongside the PGA Tour. 
         It is considered the best method for judging play because it 
         isolates the different aspects of the game. If, on a certain 
         day, a player shoots a 69 and the tournament average is 72 
         then that player will have gained three strokes on the 
         field. The statistic uses a combination of distance to the pin, 
         location (fairway or rough) and other factors to calculate a 
         baseline and then compares it with the player's result to see 
         if they gained or lost strokes. ")),
    
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
  ), 
  tabPanel("Insights",
           # d
    sidebarLayout(
             
             # d
      sidebarPanel(
        
        h6("In this tab I will discuss some general conclusions and insights based
                  on the previous two tabs.")
      ), 
        mainPanel(
          
          tabsetPanel(type = "tabs",
                  tabPanel("Strokes Gained", h6(

"In the case of the top three players in the world, 

                                                
                                                ")),
                  tabPanel("Fairways and Greens", h6("conclusions"))
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


