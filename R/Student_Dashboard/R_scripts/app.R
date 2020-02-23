#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# Authors: Team 3 

# Source Libraries
source("libraries.R", local = TRUE)


# Define UI for application that draws a histogram
ui <- dashboardPagePlus(
    dashboardHeaderPlus(title = "Title"
    )
    , dashboardSidebar(
        sidebarMenu(
            menuItem(tabName = "tabOne", text = "Tab One", icon = icon("one"))
            , menuItem(tabName = "tabTwo", text = "Tab Two", icon = icon("two")))
    )
    , dashboardBody(
        tabItems(
            tabItem(
                tabName = "tabOne", HTML("This is tab one.")
            )
            , tabItem(
                tabName = "tabTwo", uiOutput("tabTwo")
            )
        )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    output$tabTwo <- renderText("This is tab two.")
}

# Run the application 
shinyApp(ui = ui, server = server)
