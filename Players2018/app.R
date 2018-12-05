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

# I decided to choose 4 golfers - the top 3 in the world at the end of last season 
# (Brooks Koepka, Justin Rose and Dustin Johnson) as well as number 14 Tiger Woods 
# who had a historic comeback season from injury that ended with a win at the Tour 
# Championship 


# I read in the 4 rds files for each player - I opted to make an rds for each player 
# because it was easier to manage and tidier than one big one. These rds are for the
# strokes gained statistic

koepka <- readRDS("koepka.rds")
johnson <- readRDS("johnson.rds")
rose <- readRDS("rose.rds")
woods <- readRDS("woods.rds")

# Here are the rds for the fairways and greens tabs that I created in fairways-greens.Rmd

koepkaFG <- readRDS("koepkaFG.rds")
woodsFG <- readRDS("woodsFG.rds")
roseFG <- readRDS("roseFG.rds")
johnsonFG <- readRDS("johnsonFG.rds")

# This tip from the PSET 7 solutions (thanks Nick and Albert) was very helpful because I was 
# struggling to make the choices in my first sidebar look good. 

sg_choices <- c("Total" = "total",
                "Off the Tee" = "ott",
                "Tee to Green" = "t2g",
                "Approach" = "app", 
                "Around Green" = "arg",
                "Putting" = "putt")

# Define ui as an app with different tabs ----

ui <- navbarPage("PGA Tour Data", 
                 
  # Change theme ----
  theme = shinytheme("flatly"),
  
 # Tab layout with sidebar and main panel ----
 
  tabPanel("Stroke Gained",
  
  # Sidebar layout with a select box and descriptive text ----
  
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    
    sidebarPanel(
      
      # Input: select for strokes gained stat ----
      
      selectInput("yaxis",
                  label = "Choose Strokes Gained Stat",
                  choices = sg_choices),
      
      # Text instructions for the select box ----
      
      h5("Use the box above to select which strokes 
         gained statistic you want to view."), 
      
      # Line break between text ----
      
      br(), 
      
      # More text describing strokes gained in a smaller font ----
      
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
    
    # Main panel displaying tabs ----
    
    mainPanel(
      
      # Main panel has a tab for each player ----
      
      tabsetPanel(type = "tabs",
                  tabPanel("Brooks Koepka", plotOutput("koepkaPlot")),
                  tabPanel("Justin Rose", plotOutput("rosePlot")), 
                  tabPanel("Dustin Johnson", plotOutput("johnsonPlot")),
                  tabPanel("Tiger Woods", plotOutput("woodsPlot"))
      )
    )
  )
), 

  # Tab layout with sidebar and main panel ----

  tabPanel("Fairways and Greens", 
    
    # Sidebar layout with radio button ----
    
    sidebarLayout(
             
      # Sidebar panel for inputs ----
      
      sidebarPanel(
               
        # Input: radio buttons for fairway hit or missed ----
        
        radioButtons("fairway", 
                     "Fairway Hit", 
                     choices = c("Hit", "Missed")
        ), 
        
        # Input: radio buttons for green hit or missed ----
        
        radioButtons("green", 
                     "Green Hit", 
                     choices = c("Hit", "Missed")
        ), 
        
        # Text: text instructions for buttons ----
        
        h5("You can select if a player hit or missed the fairway and then hit or missed 
           the green in regulation. In general, hitting the fairway and then the green should lead to 
           lower scores.")
      ), 
      
      # Main panel displaying tabs ----
      
      mainPanel(
        
        # Main panel has a tab for each player ----
        
        tabsetPanel(type = "tabs",
                    tabPanel("Brooks Koepka", plotOutput("koepkaFGPlot")),
                    tabPanel("Justin Rose", plotOutput("roseFGPlot")), 
                    tabPanel("Dustin Johnson", plotOutput("johnsonFGPlot")),
                    tabPanel("Tiger Woods", plotOutput("woodsFGPlot"))
        )
      )
    )
  ), 

# Tab layout with sidebar and main panel ----

  tabPanel("Insights",
           
           # Sidebar layout with no inputs just text ----
           
    sidebarLayout(
             
             # Sidebar panel with  text describing my insights ----
             
      sidebarPanel(
        
        # Text describing my insights ----
        
        h4("In this tab I discuss some general conclusions and insights based
                  on the previous two tabs.")
      ), 
      
        # Main panel displaying tabs ----
      
        mainPanel(
          
          # Tabs displaying for each project and text describing my conclusions ----
          
          tabsetPanel(type = "tabs",
                      
                      # Tab for strokes gained ----
                      tabPanel("Strokes Gained", 
                               
                               # Text of conclusions ----
                               h5(
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
                                                ), 

                                # Insert line break ----

                                br(), 

                                # More insights ----

                                h5(
  "Another interesting thing I noticed was at the one match play event this season, world number one
  at the time, Dustin Johnson had his worst strokes gained: total score. I wonder if this is due to 
  format of the event or if he just had a bad week."
                                )
),

                  # Tab for Fairways and Greens ----

                  tabPanel("Fairways and Greens", 
                           
                           # Text of conclusions ----
                           
                           h5(
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

# Define server logic ----

server <- function(input, output) {
  
  # Render plot for Koepka strokes gained ----
  
  output$koepkaPlot <- renderPlot({
    
    # I wanted someway to look at different strokes gained stats for each player so I put together
    # this graph. Because there are six different statistics I figured the best way to show it would
    # be a graph with changing y axis for each stat. I first had tournament teh x axis but it looked
    # like it was trying to show something chronological, even though the events were just alphabetical.
    # I decided to remove the x axis and just order the points vertically by strokes gained score. I
    # spent some time fiddling with the location and style of the point labels but ultimately opted to 
    # move them to the side. 
    
    koepka %>% 
      filter(!total %in% 0) %>% 
      ggplot(aes_string(x = '0', y = input$yaxis, color = 'win')) + 
      geom_point() +
      labs(title = "Looking at World Number One Brooks Koepka's 2018 Season", 
           caption = "Data provided by the PGA Tour", 
           x = "", 
           y = input$yaxis, 
           color = "Win") +
      scale_color_manual(values = c("black","red")) +
      geom_label_repel(aes(label = event), size = 3, force = 3, nudge_x = -1, segment.alpha = 0.2, direction = "both", hjust = 0) +
      theme_bw() +
      theme_linedraw() +
      theme(axis.text.x = element_blank()) +
      scale_x_discrete()
  })
  
  # Render plot for Rose strokes gained ----
  
  output$rosePlot <- renderPlot({
    
    # I used the same proces for each graph for strokes ----
    
    rose %>% 
      filter(!total %in% 0) %>% 
      ggplot(aes_string(x = '0', y = input$yaxis, color = 'win')) + 
      geom_point() +
      labs(title = "Looking at World Number Two Justin Rose's 2018 Season", 
           caption = "Data provided by the PGA Tour", 
           x = "", 
           y = input$yaxis, 
           color = "Win") +
      scale_color_manual(values = c("black","red")) +
      geom_label_repel(aes(label = event), size = 3, force = 3, nudge_x = -1, segment.alpha = 0.2, direction = "both", hjust = 0) +
      theme_bw() +
      theme_linedraw() +
      theme(axis.text.x = element_blank()) +
      scale_x_discrete()
  })
  
  # Render plot for Johnson strokes gained ----
  
  output$johnsonPlot <- renderPlot({
    
    # I used the same proces for each graph for strokes ----
    
    johnson %>% 
      filter(!total %in% 0) %>% 
      ggplot(aes_string(x = '0', y = input$yaxis, color = 'win')) + 
      geom_point() +
      labs(title = "Looking at World Number Three Dustin Johnson's 2018 Season", 
           caption = "Data provided by the PGA Tour", 
           x = "", 
           y = input$yaxis, 
           color = "Win") +
      scale_color_manual(values = c("black","red")) +
      geom_label_repel(aes(label = event), size = 3, force = 3, nudge_x = -1, segment.alpha = 0.2, direction = "both", hjust = 0) +
      theme_bw() +
      theme_linedraw() +
      theme(axis.text.x = element_blank()) +
      scale_x_discrete()
  })
  
  # Render plot for Woods strokes gained ----
  
  output$woodsPlot <- renderPlot({ 
    
    # I used the same proces for each graph for strokes ----
    
    ggplot(woods, aes_string(x = '0', y = input$yaxis, color = 'win')) + 
      geom_point() +
      labs(title = "Looking at Tiger Woods' Comeback 2018 Season", 
           caption = "Data provided by the PGA Tour", 
           x = "", 
           y = "Strokes Gained", 
           color = "Win") +
      scale_color_manual(values = c("black","red")) +
      geom_label_repel(aes(label = event), size = 3, force = 3, nudge_x = -1, segment.alpha = 0.2, direction = "both", hjust = 0) +
      theme_bw() +
      theme_linedraw() +
      theme(axis.text.x = element_blank())  +
      scale_x_discrete()
  })
  
  # Render plot for Koepka fairway and greens ----
  
  output$koepkaFGPlot <- renderPlot({
    
    # For my second visual I wanted to look at fairways and greens. I wanted to have the option
    # for the user to select hit or miss for both categories. I figured the best way to do this 
    # was to use radio buttons. I tried to use check boxes but that did not work because you 
    # could select both hit and miss and that just didn't add anything to the display. 
    
    koepkaFG %>% 
      filter(hit_fwy %in% input$fairway, 
             hit_green %in% input$green, 
             rtp_score >= -2) %>% 
      ggplot(aes(x = rtp_score)) +
      geom_histogram(binwidth = 1, fill = "#2E4053") +
      labs(title = "Score to Par and Fairways and Greens Hit", 
           caption = "Data provided by the PGA Tour", 
           x = "Score to Par", 
           y = "") +
      theme_bw() +
      theme_linedraw()
      
  })
  
  # Render plot for Rose fairway and greens ----
  
  output$roseFGPlot <- renderPlot({
    
    # I used the same method for each fairway and green visual ----
    
    roseFG %>% 
      filter(hit_fwy %in% input$fairway, 
             hit_green %in% input$green, 
             rtp_score >= -2) %>% 
      ggplot(aes(x = rtp_score)) +
      geom_histogram(binwidth = 1, fill = "#2E4053") +
      labs(title = "Score to Par and Fairways and Greens Hit", 
           caption = "Data provided by the PGA Tour", 
           x = "Score to Par", 
           y = "") +
      theme_bw() +
      theme_linedraw()
    
    
  })
  
  # Render plot for Johnson fairway and greens ----
  
  output$johnsonFGPlot <- renderPlot({
    
    # I used the same method for each fairway and green visual ----
    
    johnsonFG %>% 
      filter(hit_fwy %in% input$fairway, 
             hit_green %in% input$green, 
             rtp_score >= -2) %>% 
      ggplot(aes(x = rtp_score)) +
      geom_histogram(binwidth = 1, fill = "#2E4053") +
      labs(title = "Score to Par and Fairways and Greens Hit", 
           caption = "Data provided by the PGA Tour", 
           x = "Score to Par", 
           y = "") +
      theme_bw() +
      theme_linedraw()
    
  })
  
  # Render plot for Woods fairway and greens ----
  
  output$woodsFGPlot <- renderPlot({
    
    # I used the same method for each fairway and green visual ----
    
    woodsFG %>% 
      filter(hit_fwy %in% input$fairway, 
             hit_green %in% input$green, 
             rtp_score >= -2) %>% 
      ggplot(aes(x = rtp_score)) +
      geom_histogram(binwidth = 1, fill = "#2E4053") +
      labs(title = "Score to Par and Fairways and Greens Hit", 
           caption = "Data provided by the PGA Tour", 
           x = "Score to Par", 
           y = "") +
      theme_bw() +
      theme_linedraw()
    
  })
}

# Run the application ----

shinyApp(ui = ui, server = server)


