# Project: Mastery Grade System
# Professor Side Dashboard 
# app.R
#
# 4/16/20 -- First release (MIT License), in class demo
#
# Purpose:This file contains the professor side server and user interface. This is where the main development of our app takes place.
# 
# Authors: Owen Bezick, Ana Hayne, Katie Turner, Brad Shook, Abby Santiago,
# Ben Santiago, and Gracie Petty
# 

# Source Libraries
source("utils.R", local = T)
source("libraries.R", local = TRUE)
source("dataIntake.R", local = TRUE)
# UI ----
ui <- dashboardPage(
    dashboardHeader(title = "Student View" 
    )
    # Sidebar ----
    , dashboardSidebar( 
        sidebarMenu(
            menuItem(tabName = "home", text = "Home", icon = icon("home"))
            , menuItem(tabName ="viewGrades", text = "View Grades", icon = icon("chalkboard")
                       , menuSubItem(tabName = "reviewGrades", text = "View Review Grades")
                       , menuSubItem(tabName = "homeworkGrades", text = "View Homework Grades")
            )
        )
    )
    # Body ----
    , dashboardBody( # Contains tabItems
        tabItems(
            # Home UI ----
            tabItem(
                tabName = "home"
                , HTML("<center><h1> Mastery Gradebook Dashboard </h1></center>")
                , div(img(src="davidsonCollege.jpg"), style="text-align: center;")
                , HTML("<center> <h3> Software Design, Group 3. <br> Gracie Petty, Abby Santiago, Ben Santiago, Brad Shook, Katie Turner, Ana Hayne & Owen Bezick </h3></center>")
                , uiOutput("modal")
            )
            # View Review UI ----
            , tabItem(
                tabName = "reviewGrades"
                ,fluidRow(
                    box(width = 12, title = "Filter:", status = "primary" 
                        , column(width = 6
                                 ,uiOutput("reviewPicker")
                        )
                        ,column(width = 6
                                ,uiOutput("reviewTopicPicker")
                        )
                        
                    )
                )
                , fluidRow(
                    box(width = 6, status = "primary", title = "Review Grades", height = "550"
                        , DTOutput("totalReviewGrades")
                    )
                    , box(width = 6, height = "550", stauts = "primary", title = "Total Grades", status = "primary"
                          , echarts4rOutput("gradeBar"))
                )
            )
            
            # View Homework UI ----
            , tabItem(
                tabName = "homeworkGrades"
                , fluidRow(
                    box(width = 6, title = "Filter:", status = "primary" 
                        ,uiOutput("hwPicker")
                    )
                )
                , fluidRow(
                    box(width = 6, status = "primary", title = "Homework Grades"
                        , DTOutput("homeworkGradeTable")
                    )
                )
            )
        )
    )
)


# Define server logic 
server <- function(input, output) {
    
    output$modal <- renderUI({
        showModal(
            modalDialog(title = "Authentication", easyClose = F, footer = actionBttn(inputId = "auth_save", label = "Continue")
                        , numericInput(inputId = "student_id"
                                       , label = "Enter your Davidson ID:"
                                       , value = 801000000)
            )
        )
    })
    
    auth_student_id <- reactive(input$student_id)
    
    observeEvent(input$auth_save, {
        removeModal()
    })
    
    # View Review Server ---- 
    #List of reviews
    ls_reviews <- reactive({
        df <- getReviewGrades()
        df %>% distinct(review_id) %>% pull()
    })
    # List of topics
    ls_review_topics <- reactive({
        df <- getReviewGrades()
        df %>% distinct(topic_id) %>% pull()
    })
    
    # Topic Picker
    output$reviewTopicPicker <- renderUI({
        pickerInput("reviewTopicPicker"
                    ,"Topic"
                    , choices = ls_review_topics()
                    , selected = ls_review_topics()
                    , multiple = TRUE)
    })
    # Review Picker
    output$reviewPicker <- renderUI({
        pickerInput("reviewPicker"
                    ,"Review by ID"
                    , choices = ls_reviews()
                    , selected = ls_reviews()
                    , multiple = TRUE)
    })
    
    # DT output
    output$totalReviewGrades <- renderDT({
        req(input$reviewTopicPicker, input$reviewPicker)
        auth_student_id <- auth_student_id()
        df <- getReviewGrades()
        df <- df %>%
            filter(student_id == as.numeric(auth_student_id)) %>%
            filter(review_id %in% input$reviewPicker, topic_id %in% input$reviewTopicPicker) %>%
            select( First = first_name, Last = last_name,`Review ID` = review_id, Topic = topic_id, Grade = grade)
        datatable(df, rownames = FALSE)
    })
    
    # Total Grades Chart
    output$gradeBar <- renderEcharts4r({
        req(input$reviewTopicPicker, input$reviewPicker)
        auth_student_id <- auth_student_id()
        df <- getReviewGrades() %>%
            filter(student_id == as.numeric(auth_student_id)) %>%
            filter(review_id %in% input$reviewPicker, topic_id %in% input$reviewTopicPicker) %>%
            select(grade) %>%
            count(grade)
        graph_df <- as_data_frame(t(df)) %>% 
            mutate(chart = "")
        
        graph_df %>%
            e_chart(chart) %>%
            e_bar("V1", name = "Apprentice") %>%
            e_bar("V2", name = "Journeyman")  %>%
            e_bar("V3", name = "Master") %>%
            e_theme("westeros") %>%
            e_tooltip() %>%
            e_legend(bottom = 0)
    })
    
    # View Homeworks Server -----
    #List from homework
    ls_homeworksHW <- reactive(
        getHomeworkGrades() %>%
            distinct(homework_id) %>% pull()
    )
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
        req(input$hwPicker)
        auth_student_id <- auth_student_id()
        df <- getHomeworkGrades()
        df  <- df %>% 
            filter(student_id == as.numeric(auth_student_id)) %>%
            filter(homework_id %in% input$hwPicker) %>%
            select(First = first_name, Last = last_name, `Homework ID` = homework_id, Grade= grade)
        
        datatable(df, rownames = FALSE)
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
