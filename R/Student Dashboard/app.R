# Project: Mastery Grade System
# Student Side Dashboard 
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
source("dataintake.R", local = TRUE)
# UI ----
ui <- dashboardPage(
  dashboardHeader(title = "Student View" 
  )
  # Sidebar ----
  , dashboardSidebar( 
    sidebarMenu(
      menuItem(tabName = "home", text = "Home", icon = icon("home"))
      , menuItem(tabName = "profile", text = "Profile", icon = icon("user"))
      , menuItem(tabName ="viewGrades", text = "View Grades", icon = icon("chalkboard")
                 , menuSubItem(tabName = "reviewGrades", text = "View Review Grades")
                 , menuSubItem(tabName = "homeworkGrades", text = "View Homework Grades")
                 , menuSubItem(tabName = "gradeCalculator", text = "Grade Calculator")
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
        , uiOutput("authModal")
      )
      , tabItem(
        tabName = "profile"
        , uiOutput("profileRow")
        , uiOutput("PprofileRow")
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
      
      #Grade Calculator UI
      , tabItem(
        tabName = "gradeCalculator"
        , fluidPage(
          box(width = 14, title = "Grade Calculator")
        , div(img(src="gradeScale.jpg"), style="text-align: center;")
        , box(width = 14, title = "Current Grade Information")
        , HTML("<p> Homework Average: </p>")
        , HTML("<p> Topics Mastered: </p>")
        
        )
      )
    )
  )
)


# Define server logic 
server <- function(input, output) {
  # Authentication ----
  output$authModal <- renderUI({
    showModal(
      modalDialog(title = "Authentication", easyClose = F, footer = actionBttn(inputId = "auth_save", label = "Continue")
                  , numericInput(inputId = "student_id"
                                 , label = "Enter your Davidson ID:"
                                 , value = 80100000)
      )
    )
  })
  
  auth_student_id <- reactive(input$student_id)
  
  ls_student_id <- df_students %>%
    distinct(student_id) %>% pull()
  
  observeEvent(input$auth_save, {
    if (input$student_id %in% ls_student_id){
      name <- df_students %>%
        filter(student_id == input$student_id) %>% select(first_name) %>% pull()
      showNotification(paste("Welcome,", name, "!"), type = "message")
      removeModal()
    } else {
      showNotification(paste(as.character(input$student_id), "not found. Please try again."), type = "error")
    }
    
  })
  
  # Profile Data
  
  # Profile Server ----
  output$profileRow <- renderUI({
    df <- df_students %>%
      filter(student_id == auth_student_id())
    
    box(width= 4, title = " Student Information"
        , column(width = 12
                 , fluidRow(
                   HTML("<b> Student ID: </b>")
                   , df$student_id
                 )
                 , fluidRow(
                   HTML("<b> Name: </b>")
                   , paste(df$first_name, df$last_name)
                 )
                 , fluidRow(
                   HTML("<b> Photo: </b>")
                 )
                 , fluidRow(
                   img(src= paste0(as.character(df$student_id), ".jpg"))
                 )
        )
    )
  })
  
  #Professor Contact Information
  output$PprofileRow <- renderUI({
    box(width= 4, title = " Professor Information"
        , column(width = 12
                 , fluidRow(
                   HTML("<b> Dr. yourProfessor </b>"),
                 )
                 , fluidRow(
                   HTML("<b> Email: </b>"),
                   HTML("yourProfessor@davidson.edu")
                 )
                 , fluidRow(
                   HTML("<b> Office Hours: </b>")
                   , HTML("MWF: 9:30- 11")
                   , HTML("TTh: 1:40-3:00")
                 )
                
        )
    )
    
    
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
  
  # data
  reviewData <- reactive({req(input$reviewTopicPicker, input$reviewPicker)
    auth_student_id <- auth_student_id()
    df <- getReviewGrades()
    df <- df %>%
      filter(student_id == as.numeric(auth_student_id)) %>%
      filter(review_id %in% input$reviewPicker, topic_id %in% input$reviewTopicPicker) %>%
      select( First = first_name, Last = last_name,`Review ID` = review_id, Topic = topic_id, Grade = grade)
  })
  # DT output
  output$totalReviewGrades <- renderDT({
    df <- reviewData()
    datatable(df, rownames = FALSE)
  })
  
  # Total Grades Chart
  output$gradeBar <- renderEcharts4r({
    req(input$reviewTopicPicker, input$reviewPicker)
    auth_student_id <- auth_student_id()
    df <- reviewData() %>%
      select(Grade) %>%
      count(Grade)
    
    apprentice <- df[1,2]
    journey <- df[2,2]
    master <- df[3,2]
    
    graph_df <- data_frame(A = c(0 + apprentice)
                           , J = c(0 + journey)
                           , M = c(0 + master)
                           , chart = c(""))
    graph_df %>%
      e_chart(chart) %>%
      e_bar("A", name = "Apprentice") %>%
      e_bar("J", name = "Journeyman")  %>%
      e_bar("M", name = "Master") %>%
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

