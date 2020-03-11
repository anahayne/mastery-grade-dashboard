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
            menuItem(tabName = "tabOne", text = "Text Output", icon = icon("one")) # menuItem
            , menuItem(tabName = "tabTwo", text = "Review A", icon = icon("A"))
            , menuItem(tabName = "tabThree", text = "eCharts4R Output", icon = icon("two"))
            , menuItem(tabName = "tabFour", text = "Action Button & Modal Viewer"))
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
                , fluidRow(
                    box(width = 6, title = "Filters", status = "primary" # A box is a UI element that encloses uiOutputs, such as datatables
                        , uiOutput("studentPicker")
                        , uiOutput("topicPicker")
                    )
                    , box(width = 6, status = "primary", title = "Totals"
                          , valueBoxOutput("total_mastered", width = 4)
                          , valueBoxOutput("total_journey", width = 4)
                          , valueBoxOutput("total_apprentice", width = 4)
                    )
                    
                )
                , fluidRow(
                    box(width = 12, title = "Review A Datatable", status = "primary" 
                        , DTOutput("review_a_dt") # DTOutput is used for DT tables, instead of uiOutput.
                    ) 
                )
            )
            , tabItem(
                tabName = "tabThree"
                , box(width = 12, title = "Sample Graphs", status = "danger", collapsible = TRUE
                      , echarts4rOutput("sample_line") # echarts4rOutput is used for echarts4r charts, instead of uiOutput or DTOutput
                      , echarts4rOutput("sample_bar")
                )
            )
            , tabItem(
                tabName = "tabFour"
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
    # Data Intake -----
    # data is a container of reactiveValues that can be recalcualted throughout the app.
    data <- reactiveValues()
    
    # Eventually, we will replace the read excel call with a fetch SQL data table call.
    data$review_a <- read_excel("example_gradebook.xlsx", sheet = "review_a") # Reads in excel sheet and stores in the reactive values container
    
    data$grade_codebook <- read_excel("example_gradebook.xlsx", sheet = "grade_codebook")
    
    data$student_names <- read_excel("example_gradebook.xlsx", sheet = "names")
    
    # renderText output ----
    output$text <- renderText("Sample renderText output.") # Output to be used in the UI
    
    # Observe Event w/ Modal Viewer ----
    observeEvent(input$modal, { #When input$modal is no longer NULL (after the button is clicked) the code in the observe event will run
        showModal(
            modalDialog(title = "This is a modal dialog!")
        )
    })
    
    # Review A Server -----
    
    # List of students
    ls_students <- reactive({
        df <- data$student_names
        df$ID
    })
    
    # Student Picker
    output$studentPicker <- renderUI({
        pickerInput("studentPicker"
                    ,"Select Student by ID"
                    , choices = ls_students()
                    , selected = ls_students()
                    , multiple = TRUE)
    })
    
    # List of topics
    ls_topics <- reactive({
        df <- data$review_a
        df$topic %>% unique()
    })
    
    # Topic Picker
    output$topicPicker <- renderUI({
        pickerInput("topicPicker"
                    ,"Select Topic by Number"
                    , choices = ls_topics()
                    , selected = ls_topics()
                    , multiple = TRUE)
    })
    
    # Review A Data
    df_review_a <- reactive({
        req(input$studentPicker, input$topicPicker)
        
        review_a <- data$review_a
        names <- data$student_names
        merged <- merge(review_a, names, by = "ID") %>%
            mutate(ID = as.character(ID))
        
        filtered <- merged  %>%
            filter(ID %in% input$studentPicker) %>%
            filter(topic %in% input$topicPicker)
    })
    
    # DT output
    output$review_a_dt <- renderDT({
        df <- df_review_a()
        df <- df %>%
            select(ID, First, Last, Topic = topic, Grade = grade)
        
        datatable(df, rownames = FALSE)
    })
    
    # Value boxes 
    # Mastery
    total_m <- reactive({
        df <- df_review_a()
        grades <- df$grade
        table <- table(grades)
        total <- table[names(table)=="M"]
    })
    output$total_mastered <- renderValueBox(
        valueBox(total_m(), subtitle = "Total Masteries")
    )
    
    # Journeymen
    total_j <- reactive({
        df <- df_review_a()
        grades <- df$grade
        table <- table(grades)
        total <- table[names(table)=="J"]
    })
    output$total_journey <- renderValueBox(
        valueBox(total_j(), subtitle = "Total Journeymen")
    )
    
    # Apprentice
    total_a <- reactive({
        df <- df_review_a()
        grades <- df$grade
        table <- table(grades)
        total <- table[names(table)=="A"]
    })
    output$total_apprentice <- renderValueBox(
        valueBox(total_a(), subtitle = "Total Apprentice")
    )
    # Graphs ----
    # Fake Data
    df <- data.frame(
        x = seq(50),
        y = rnorm(50, 10, 3),
        z = rnorm(50, 11, 2),
        w = rnorm(50, 9, 2)
    )
    # Line Chart 
    output$sample_line <- renderEcharts4r({
        df %>% 
            e_charts(x) %>% 
            e_line(z) %>% 
            e_area(w) %>% 
            e_title("Line and area charts")
    })
    
    output$sample_bar <- renderEcharts4r({
        df %>% 
            e_charts(x) %>% 
            e_bar(y, name = "Serie 1") %>% 
            e_step(z, name = "Serie 2") %>% 
            e_title("Bar and step charts")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
