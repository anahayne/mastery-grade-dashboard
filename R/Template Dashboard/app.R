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

# UI ----
ui <- dashboardPagePlus(
    dashboardHeaderPlus(title = "Template Shiny App" # Creates dashboardHeaderPlus title (can inject javascript here to add pictures, fonts, etc.)
    )
    # Sidebar ----
    , dashboardSidebar( # Contains a sidebarMenu with menuItems and subMenuItems
        sidebarMenu(
            menuItem(tabName = "tabOne", text = "HTML Output", icon = icon("one")) # menuItem
            , menuItem(tabName = "tabTwo", text = "uiOutput", icon = icon("two"))
            , menuItem(tabName = "tabThree", text = "Action Button & Modal Viewer"))
    )
    # Body ----
    , dashboardBody( # Contains tabItems
        tabItems(
            tabItem(
                tabName = "tabOne"
                , HTML("Sample <b> HTML </b> output") # This is HTML output directly in the UI
                , uiOutput("text") # This is a uiOutput from the server
            )
            , tabItem(
                tabName = "tabTwo"
                , box(width = 12, title = "Cars Datatable", status = "primary" # A box is a UI element that encloses uiOutputs, such as datatables
                      , DTOutput("carsDT") # DTOutput isused for DT tables, instead of uiOutput.
                )
            )
            , tabItem(
                tabName = "tabThree"
                # Action buttons can be implemented directly in the UI or in the server
                , actionBttn(inputId = 'modal' # the inputId will be used in the server in an observeEvent call
                             , label = "Click for Modal Viewer"
                             , icon = icon('test'))
            )
        )
    )
)


# Define server logic 
server <- function(input, output) {
    
    # renderText output ----
    output$text <- renderText("Sample renderText output.") # Output to be used in the UI
    
    # Observe Event w/ Modal Viewer ----
    observeEvent(input$modal, { #When input$modal is no longer NULL (after the button is clicked) the code in the observe event will run
        showModal(
            modalDialog(title = "This is a modal dialog!")
        )
    })
    
    # Data Table ----
    output$carsDT <- renderDT({
        carsData <- mtcars
        datatable(carsData)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
