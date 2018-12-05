#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
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
    
                 theme = shinytheme("flatly"),
  
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
        ), 
        
        h5("You can select if a player hit or missed the fairway and then hit or missed 
           the green in regulation. In general, hitting the fairway and then the green should lead to 
           lower scores.")
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
        
        h4("In this tab I discuss some general conclusions and insights based
                  on the previous two tabs.")
      ), 
        mainPanel(
          
          tabsetPanel(type = "tabs",
                  tabPanel("Strokes Gained", h5(

"In the case of the top three players in the world (Brooks Koepka, Justin Rose and Dustin Johnson), 
they tend to lead the field in strokes gained: off the tee in tournaments they won in 2018. This 
is not the case for Tiger Woods, but that is not surprising because he is returning from injury 
and has never been known for driving the ball very consistently but rather hitting great recovery 
shots. As for strokes gained: putting, I was surprised to see that while these players tended to 
gain strokes while putting in tournaments, it was not far and away better than tournaments they did 
not win. For example, Justin Rose, winner of the Fort Worth Invitational, gained around 0.04 strokes 
putting in that tournament but there were five tournaments he did not win where he gained more 
strokes putting. This leads me to believe that a player must excel in all areas in a given week 
but putting is not far more important than driving as is suggested by the expression." 

                                                
                                                )),
                  tabPanel("Fairways and Greens", h5(

"In the second tab I look at whether or not a player hit or missed the fairway and then the green 
and how that impacted their score relative to par. I was not surprised to see that players that 
hit the green made many more pars than those that missed the green. One interesting thing I found 
was that of players who hit the green, there was not a big difference in number of pars made based 
on whether or not they hit the fairway. However, if a player missed the fairway and then hit the 
green, they made far fewer birdies while making a similar number of pars. This was consistent 
across all four players. I think this is a result of being in the fairway allows the player it 
hit it closer than from the rough."
                  ))
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


