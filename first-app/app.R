#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(dplyr)

download.file("http://statsdownload.pgatourhq.com/20181101014855187.txt/coursesdata012168.ZIP", destfile = "coursesdata.zip")

unzip("coursesdata.zip")

courses <- read_delim("coursesdata.zip", delim = ";", col_names = FALSE, escape_double = FALSE, skip =1)

courses <- courses %>% 
  transmute(year = X1, course_num = X2, course_name = X3, round = X4, hole = X5, fwy_width = X9, rough_heigt = X19, stimp = X21, yardage = X27, par = X28)


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("PGA Tour Data"),
  
  # Sidebar with a select input for course
  sidebarLayout(
    sidebarPanel(
      selectInput("courses",
                  "Select Course",  
                  unique(courses), 
                  multiple = TRUE
                  
      )),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    
    
    courses %>% 
      filter(courses = input$courses) %>% 
      group_by(course_name) %>% 
      summarize(avgyard = sum(yardage)/18) %>% 
      ggplot(aes(x= course_name, y= avgyard)) +
      geom_point() +
      xlab("Course Name") + 
      ylab("Average Yard per Hole") +
      theme_bw()+
      theme_linedraw()
  })
}


# Run the application 
shinyApp(ui = ui, server = server)
