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
            menuItem(tabName = "tabOne", text = "HTML Output", icon = icon("one")) # menuItem
            , menuItem(tabName = "tabTwo", text = "uiOutput", icon = icon("two"))
            , menuItem(tabName = "tabThree", text = "Action Button & Modal Viewer"))
    )
    , dashboardBody( # Contains tabItems
        tabItems(
            tabItem(
                tabName = "tabOne", HTML("Sample <b> HTML </b> output") # This tab has HTML output directly in the UI
            )
            , tabItem(
                tabName = "tabTwo", uiOutput("tabTwo") # This tab uses a uiOutput from the server
            )
            , tabItem(
                tabName = "tabThree", actionBttn(inputId = 'modal', label = "Click for Modal Viewer", icon = icon('test'))
            )
        )
    )
)


# Define server logic 
server <- function(input, output) {
    output$tabTwo <- renderText("Sample renderText output.") # Output to be used in the UI
    
    observeEvent(input$modal, {
        showModal(
            modalDialog(title = "This is a modal dialog!")
        )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
