# A little Shiny R Introduction.

# Some R Packages that will be useful ----

# install.packages("PACKAGE TITLE")         Installing a package only needs to be done one time. Once a package is installed, it is downloading on one's computer.
# library(PACKAGE TITLE)                    A package needs to be libraried every R session. Meaning once you close down R, the package needs to be reloaded before it can be used that session.

# Note that there are quotes around the package title when installing but not when librarying.

# Here is a list of packages that we will most likely be using in our application. The only one that needs to be installed for the test is shiny. the install.packages() call can 
# be ran in the console.

# Shiny packages
# install.packages("shiny")
library(shiny)

# library(shinydashboard)
# library(shinyWidgets)
# 
#  Excel packages
# library(openxlsx)
# library(readxl)
# 
#  Data packages
# library(tidyverse)
# library(dplyr)
# library(DT)
# 
#  Graphics packages.
# library(echarts4r)
# library(ggplot2)

# There will be more packages to install and library as we define the application's requirements and scale.

# A Small Shiny Web App ----

# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above. This is the default application created when going 
# to file > new > new Shiny Web App... in R Studio.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Old Faithful Geyser Data"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
}

# Run the application 
shinyApp(ui = ui, server = server)