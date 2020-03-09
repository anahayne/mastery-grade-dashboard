#
# This is a template Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# Author: Owen Bezick

# Source Libraries
source("libraries.R", local = TRUE)


# Define template UI for application 
ui <- dashboardPagePlus(
    dashboardHeaderPlus(title = "Title" # Creates dashboardHeaderPlus title (can inject javascript here to add pictures, fonts, etc.)
    )
    , dashboardSidebar( # Contains a sidebarMenu with menuItems and subMenuItems
        sidebarMenu(
            menuItem(tabName = "tabOne", text = "Tab One", icon = icon("one")) # menuItem
            , menuItem(tabName = "tabTwo", text = "Tab Two", icon = icon("two")))
    )
    , dashboardBody( # Contains tabItems
        tabItems(
            tabItem(
                tabName = "tabOne", HTML("This is tab one.") # This tab has HTML output directly in the UI
            )
            , tabItem(
                tabName = "tabTwo", uiOutput("tabTwo") # This tab uses a uiOutput from the server
            )
        )
    )
)


# Define server logic 
server <- function(input, output) {
    output$tabTwo <- renderText("This is tab two.") # Output to be used in the UI
}

# Run the application 
shinyApp(ui = ui, server = server)
