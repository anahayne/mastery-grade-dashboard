#
#
# Authors: Owen Bezick
#
# 

# Source Libraries
source("libraries.R", local = TRUE)
source("dataIntake.R", local = TRUE)

# UI ----
ui <- dashboardPage(
    dashboardHeader(title = "Professor View" 
    )
    # Sidebar ----
    , dashboardSidebar( 
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
            # View Review UI ----
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
                        , column(width = 12
                                 , DTOutput("totalReviewGrades")
                        )
                    )
                )
            )
            # View Homework UI ----
            , tabItem(
                tabName = "homeworkGrades"
                , fluidRow(
                    box(width = 12, title = "Filter:", status = "primary" 
                        ,column(width = 6
                                , uiOutput("hwStudentPicker")
                        )
                        , column(width = 6
                                 ,uiOutput("hwPicker")
                        )
                    )
                )
                , fluidRow(
                    box(width = 6, status = "primary"
                        , DTOutput("homeworkGradeTable")
                    )
                    , box(width = 6, status = "primary"
                          , echarts4rOutput("avgHomeworkGraph")
                    )
                )
            )
            # Edit Review Grades ----
            , tabItem(
                tabName = "editReviewGrades"
                ,fluidRow(
                    box(width = 12, title = "Filter:", status = "primary" 
                        ,column(width = 6
                                ,uiOutput("reviewEditStudentPicker")
                        )
                        , column(width = 6
                                 ,uiOutput("reviewEditPicker")
                        )
                    )
                )
                , fluidRow(
                    box(width = 12, status = "primary", title = "Double-Click Cell to Edit"
                        , column(width = 12
                                 , DTOutput("totalEditReviewGrades")
                        )
                    )
                )
            )
            # Edit Homework UI ----
            , tabItem(
                tabName = "editHomeworkGrades"
                , fluidRow(
                    box(width = 12, title = "Filter:", status = "primary" 
                        ,column(width = 6
                                , uiOutput("hwEditStudentPicker")
                        )
                        , column(width = 6
                                 ,uiOutput("hwEditPicker")
                        )
                    )
                )
                , fluidRow(
                    box(width = 12, status = "primary", title = "Double-Click Cell to Edit"
                        , DTOutput("homeworkEditGradeTable")
                    )
                )
            )
            
        )
    )
)


# Define server logic 
server <- function(input, output) {
    # View Review Server ---- 
    
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

    # View Homeworks Server -----
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
    
    # Homework Average Graph
    # Data
    hwAvg <- reactive({
        req(input$hwStudentPicker, input$hwPicker)
        df <- getHomeworkGrades()
        df  <- df %>% 
            filter(student_id %in% input$hwStudentPicker) %>%
            filter(homework_id %in% input$hwPicker) %>%
            group_by(student_id) %>%
            mutate(homeworkAvg = mean(grade)/100)
    })
    # Graph
    output$avgHomeworkGraph <- renderEcharts4r({
        df <- hwAvg()
        df %>%
            e_chart(last_name) %>%
            e_scatter(homeworkAvg, symbol_size = 10) %>%
            e_theme("westeros") %>%
            e_tooltip(formatter = e_tooltip_item_formatter(
                style = c("percent"),
                digits = 2
            )
            ) %>%
            e_x_axis(axisLabel = list(interval = 0, rotate = 45)) %>%
            e_y_axis(formatter = e_axis_formatter(
                style = c("percent"),
                digits = 2,
            )
            ) %>%
            e_legend(show = F)
    })
    # Edit Review Server ----
    # Student Picker
    output$reviewEditStudentPicker <- renderUI({
        pickerInput("reviewEditStudentPicker"
                    ,"Student by ID"
                    , choices = ls_studentsR()
                    , selected = ls_studentsR()
                    , multiple = TRUE)
    })
    # Homework Picker
    output$reviewEditPicker <- renderUI({
        pickerInput("reviewEditPicker"
                    ,"Review by ID"
                    , choices = ls_reviews()
                    , selected = ls_reviews()
                    , multiple = TRUE)
    })
    
    # DT output
    output$totalEditReviewGrades <- renderDT({
        req(input$reviewEditStudentPicker, input$reviewEditPicker)
        df <- getReviewGrades()
        df <- df %>%
            filter(review_id %in% input$reviewEditPicker) %>%
            filter(student_id %in% input$reviewEditStudentPicker) %>%
            select(review_id, First = first_name, Last = last_name, Topic = topic_id, Grade = grade)
        datatable(df, rownames = FALSE, editable = T)
    })
    
    # Edit Homework Server ----
    # Student Picker
    output$hwEditStudentPicker <- renderUI({
        pickerInput("hwEditStudentPicker"
                    ,"Student by ID"
                    , choices = ls_studentsHW()
                    , selected = ls_studentsHW()
                    , multiple = TRUE)
    })
    
    # Homework Picker
    output$hwEditPicker <- renderUI({
        pickerInput("hwEditPicker"
                    ,"Homework by ID"
                    , choices = ls_homeworksHW()
                    , selected = ls_homeworksHW()
                    , multiple = TRUE)
    })
    # Table
    output$homeworkEditGradeTable <- renderDT({
        req(input$hwEditStudentPicker, input$hwEditPicker)
        df <- getHomeworkGrades()
        df  <- df %>% 
            filter(student_id %in% input$hwEditStudentPicker) %>%
            filter(homework_id %in% input$hwEditPicker) %>%
            select(ID = student_id, First = first_name, Last =  last_name, `Homework ID` = homework_id, Grade = grade)
        
        datatable(df, rownames = FALSE, editable = T)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
