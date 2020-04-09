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
source("dataIntake.R", local = TRUE)

# UI ----
ui <- dashboardPagePlus(
    dashboardHeaderPlus(title = "Professor View" # Creates dashboardHeaderPlus title (can inject javascript here to add pictures, fonts, etc.)
    )
    # Sidebar ----
    , dashboardSidebar( # Contains a sidebarMenu with menuItems and subMenuItems
        sidebarMenu(
            menuItem(tabName = "reviewGrades", text = "View Review Grades", icon = icon("one")) # menuItem
            , menuItem(tabName = "homeworkGrades", text = "View Homework Grades", icon = icon("A"))
        )
    )
    # Body ----
    , dashboardBody( # Contains tabItems
        tabItems(
            tabItem(
                tabName = "reviewGrades"
                , HTML("Sample <b> HTML </b> output") # This is HTML output directly in the UI
                , uiOutput("text") # This is a uiOutput from the server
            )
            , tabItem(
                tabName = "homeworkGrades"
                , fluidRow(
                    box(width = 12, title = "Filter:", status = "primary" # A box is a UI element that encloses uiOutputs, such as datatables
                        ,column(width = 6
                                ,uiOutput("hwStudentPicker")
                        )
                        , column(width = 6
                                 ,uiOutput("hwPicker")
                        )
                    )
                    , box(width = 12, status = "primary"
                          , DTOutput("homeworkGradeTable")
                    )
                )
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
    
    # Homeworks Server -----
    # List of students
    ls_students <- reactive({
        df <- getHomeworkGrades()
        df %>% distinct(student_id) %>% pull()
    })
    #List from homework
    ls_homeworks <- reactive({
        df <- getHomeworkGrades()
        df %>% distinct(homework_id) %>% pull()
    })
    # Student Picker
    output$hwStudentPicker <- renderUI({
        pickerInput("hwStudentPicker"
                    ,"Student by ID"
                    , choices = ls_students()
                    , selected = ls_students()
                    , multiple = TRUE)
    })
    # Homework Picker
    output$hwPicker <- renderUI({
        pickerInput("hwPicker"
                    ,"Homework by ID"
                    , choices = ls_homeworks()
                    , selected = ls_homeworks()
                    , multiple = TRUE)
    })
    # Table
    output$homeworkGradeTable <- renderDT({
        req(input$hwStudentPicker, input$hwPicker)
        df <- getHomeworkGrades()
        df  <- df %>% 
            filter(student_id %in% input$hwStudentPicker) %>%
            filter(homework_id %in% input$hwPicker) %>%
            select(student_id, first_name, last_name, homework_id, grade)
        
        datatable(df)
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
