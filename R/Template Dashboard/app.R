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
ui <- dashboardPage(
    dashboardHeader(title = "Professor View" # Creates dashboardHeaderPlus title (can inject javascript here to add pictures, fonts, etc.)
    )
    # Sidebar ----
    , dashboardSidebar( # Contains a sidebarMenu with menuItems and subMenuItems
        sidebarMenu(
            menuItem(tabName = "home", text = "Home", icon = icon("home"))
            , menuItem(tabName ="viewGrades", text = "View Grades", icon = icon("chalkboard")
                       , menuSubItem(tabName = "reviewGrades", text = "View Review Grades")
                       , menuSubItem(tabName = "homeworkGrades", text = "View Homework Grades")
            )
            , menuItem(tabName ="editGrades", text = "Edit Grades", icon = icon("chalkboard-teacher")
                       , menuSubItem(tabName = "editReviewGrades", text = "Edit Review Grades")
                       , menuSubItem(tabName = "editHomeworkGrades", text = "Edit Homework Grades")
            )
        )
    )
    # Body ----
    , dashboardBody( # Contains tabItems
        tabItems(
            # Home UI ----
            tabItem(
                tabName = "home"
            )
            # Review UI ----
            , tabItem(
                tabName = "reviewGrades"
                ,fluidRow(
                    box(width = 12, title = "Filter:", status = "primary" 
                        ,column(width = 6
                                ,uiOutput("reviewStudentPicker")
                        )
                        , column(width = 6
                                 ,uiOutput("reviewPicker")
                        )
                    )
                )
                , fluidRow(
                    box(width = 12, status = "primary"
                        , DTOutput("totalReviewGrades")
                    )
                )
            )
            # Homework UI ----
            , tabItem(
                tabName = "homeworkGrades"
                , fluidRow(
                    box(width = 12, title = "Filter:", status = "primary" 
                        ,column(width = 6
                                ,uiOutput("hwStudentPicker")
                        )
                        , column(width = 6
                                 ,uiOutput("hwPicker")
                        )
                    )
                )
                , fluidRow(
                    box(width = 12, status = "primary"
                        , DTOutput("homeworkGradeTable")
                    )
                )
            )
            , tabItem(
                tabName = "editReviewGrades"
            )
            , tabItem(
                tabName = "editHomeworkGrades"
            )
            
        )
    )
)


# Define server logic 
server <- function(input, output) {
    # Review Server ---- 
    
    # List of students
    ls_studentsR <- reactive({
        df <- getReviewGrades()
        df %>% distinct(student_id) %>% pull()
    })
    #List from homework
    ls_reviews<- reactive({
        df <- getReviewGrades()
        df %>% distinct(review_id) %>% pull()
    })
    # Student Picker
    output$reviewStudentPicker <- renderUI({
        pickerInput("reviewStudentPicker"
                    ,"Student by ID"
                    , choices = ls_studentsR()
                    , selected = ls_studentsR()
                    , multiple = TRUE)
    })
    # Homework Picker
    output$reviewPicker <- renderUI({
        pickerInput("reviewPicker"
                    ,"Review by ID"
                    , choices = ls_reviews()
                    , selected = ls_reviews()
                    , multiple = TRUE)
    })
    
    # DT output
    output$totalReviewGrades <- renderDT({
        req(input$reviewStudentPicker, input$reviewPicker)
        df <- getReviewGrades()
        df <- df %>%
            filter(review_id %in% input$reviewPicker, student_id %in% input$reviewStudentPicker) %>%
            select(review_id, First = first_name, Last = last_name, Topic = topic_id, Grade = grade)
        datatable(df, rownames = FALSE)
    })
    
    # Homeworks Server -----
    # List of students
    ls_studentsHW <- reactive({
        df <- getHomeworkGrades()
        df %>% distinct(student_id) %>% pull()
    })
    #List from homework
    ls_homeworksHW <- reactive({
        df <- getHomeworkGrades()
        df %>% distinct(homework_id) %>% pull()
    })
    # Student Picker
    output$hwStudentPicker <- renderUI({
        pickerInput("hwStudentPicker"
                    ,"Student by ID"
                    , choices = ls_studentsHW()
                    , selected = ls_studentsHW()
                    , multiple = TRUE)
    })
    # Homework Picker
    output$hwPicker <- renderUI({
        pickerInput("hwPicker"
                    ,"Homework by ID"
                    , choices = ls_homeworksHW()
                    , selected = ls_homeworksHW()
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
        
        datatable(df, rownames = FALSE)
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
