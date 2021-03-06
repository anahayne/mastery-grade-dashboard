# Project: Mastery Grade System
# Student Side Dashboard 
# app.R
#
# 4/16/20 -- First release (MIT License), in class demo
# 5/1/20 -- Final Version presented
#
# Purpose:This file contains the student side server and user interface. This is where the main development of our app takes place.
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
  skin = "red",
  
  dashboardHeader(title = "Student View" 
  )
  # Sidebar ----
  , dashboardSidebar( 
    sidebarMenu(
      menuItem(tabName = "home", text = "Home", icon = icon("home"))
      , menuItem(tabName ="viewGrades", text = "View Grades", icon = icon("chalkboard"))
      , menuItem(tabName ="gradeCalculator", text = "Grade Calculator", icon = icon("calculator"))
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
        , HTML("<center> <h3> Student View </h3></center>")
        , uiOutput("authModal")
        , uiOutput("profile")
      )
      # View Grades UI ----
     , tabItem(
        tabName = "viewGrades"
        , tabBox(title = "View Grades", width = 12, side = c("right"),
                 # Review Grades
                 tabPanel( title = "Review Grades", width = 12
                             , fluidRow(
                               box(width = 12, title = "Filter:", status = "danger" 
                                   , column(width = 6
                                            ,uiOutput("reviewPicker")
                                   )
                                   ,column(width = 6
                                           ,uiOutput("reviewTopicPicker")
                                   )
                                   
                               )
                             )
                             , fluidRow(
                               box(width = 6, status = "danger", title = "Review Grades", height = "550"
                                   , DTOutput("totalReviewGrades")
                               )
                               , box(width = 6, height = "550", stauts = "danger", title = "Total Grades", status = "danger"
                                     , textOutput("masteredTopics")
                                     , echarts4rOutput("gradeBar")
                                     
                               )
                             )
                 )
                 # Homework Grades
                 , tabPanel("Homework Grades", width = 12
                            , fluidRow(
                              box(width = 12, title = "Filter:", status = "danger" 
                                  ,uiOutput("hwPicker")
                              )
                            )
                            , fluidRow(
                              box(width = 6, status = "danger", title = "Homework Grades"
                                  , DTOutput("homeworkGradeTable")
                              )
                              , box(width = 6, title = "Homework Grades", status = "danger"
                                    , echarts4rOutput("homeworkScatter")
                              )
                            )
                 )
        )
      )
      
      
      # Grade Calculator UI ----
      , tabItem(
        tabName = "gradeCalculator"
        , fluidRow(
          box(width = 12, title = "Current Grade Information", status = "danger"
              , valueBoxOutput("homeworkAVG")
              , valueBoxOutput("totalMastered")
          )
        )
        , fluidRow(
          box(width = 12, title = "Grade Calculator", status = "danger"
              , div(img(src="gradeScale.jpg", height = "100%", width = "100%"), style="text-align: center;")
          )
        )
      )
    )
  )
)



# Define server logic 
server <- function(input, output) {
  
  # Authentication ---- Ensures input student id is valid
  output$authModal <- renderUI({
    showModal(
      modalDialog(title = "Authentication", easyClose = F, footer = actionBttn(inputId = "auth_save", label = "Continue")
                  , numericInput(inputId = "student_id"
                                 , label = "Enter your Davidson ID:"
                                 , value = 80100000)
      )
    )
  })
  
  #Pulls input information
  is <- reactiveValues(auth = F)
  auth_student_id <- reactive(input$student_id)
  
  #List of valid student idss
  ls_student_id <- df_students %>%
    distinct(student_id) %>% pull()
  
  #Implements the welcome message by verifying user input
  observeEvent(input$auth_save, {
    if (input$student_id %in% ls_student_id){
      name <- df_students %>%
        filter(student_id == input$student_id) %>% select(first_name) %>% pull()
      showNotification(paste("Welcome,", name, "!"), type = "message")
      is$auth <- T
      removeModal()
    } else {
      showNotification(paste(as.character(input$student_id), "not found. Please try again."), type = "error")
    }
  })
  
  
  # Profile ---- Sets up view boxes for professor and student profile information
  # pulls student name from the database and uses hard coded professor info
  output$profile <- renderUI({
    req(is$auth) #requires authentication for viewing
    df <- df_students %>%
      filter(student_id == auth_student_id())
    
    fluidRow(
      column(width = 1)
      , column(width = 5
             , box(width= 14, title = "Student Information", status = "danger"
                   , column(width = 7
                            , fluidRow(
                              HTML(paste0("<b>", paste(df$first_name, df$last_name), "</b>"))
                            )
                            , fluidRow(
                              img(src= paste0(as.character(df$student_id), ".jpg"))
                            )
                   )
                   , column(width = 7
                            , br()
                            , fluidRow(
                              HTML("<b> Student ID: </b>")
                              , df$student_id
                            )
                            , br()
                            , fluidRow(
                              HTML("<b> Email: </b>")
                              , tolower(paste0(substr(df$first_name, 0, 2), df$last_name, "@davidson.edu"))
                            )
                   )
             )
      )
      ,  column(width = 5,
                box(width= 14, title = "Professor Information", status = "danger"
                    , column(width = 7
                             , fluidRow(
                               HTML("<b> Dr. Professorson </b>"),
                             )
                             , fluidRow(
                               img(src= "mascot.jpg")
                             )
                    )
                    , column(width = 7
                             , br()
                             , fluidRow(
                               HTML("<b> Email: </b>"),
                               HTML("drprofessorson@davidson.edu")
                             )
                             , br()
                             , fluidRow(
                               HTML("<b> Office Hours: </b>")
                               , br()
                               , HTML("MWF: 9:30- 11")
                               , br()
                               , HTML("TTh: 1:40-3:00")
                             )
                    )
                )
      )
      , column(width = 1)
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
  
  # Pulls review data from the database for the student by id
  reviewData <- reactive({req(input$reviewTopicPicker, input$reviewPicker)
    auth_student_id <- auth_student_id()
    df <- getReviewGrades()
    df <- df %>%
      filter(student_id == as.numeric(auth_student_id)) %>%
      filter(review_id %in% input$reviewPicker, topic_id %in% input$reviewTopicPicker) %>%
      select( First = first_name, Last = last_name,`Review ID` = review_id, Topic = topic_id, Grade = grade)
  })
  
  # Mastered Topics
  output$masteredTopics <- renderText({
    df <- reviewData()
    mastered_topics <- df %>%
      filter(Grade == "M") %>%
      select(Topic) %>%
      pull() %>%
      unique()
   
    output <- "Mastered Topics: "
    
    for (topic in mastered_topics){
      output <- paste(output, as.character(topic))
    }
    return(output)
    
  })
  # DT output --- outputs the grade information to the UI
  output$totalReviewGrades <- renderDT({
    df <- reviewData()
    datatable(df, rownames = FALSE)
  })
  
  # Total Grades Chart-- builds bar chart visualization for the reviews, pulling
  # info from database
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
      e_theme("dark") %>%
      e_tooltip() %>%
      e_legend(bottom = 0)
  })
  
  # View Homeworks Server --- List from homework pulled from database
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
  
  # Homework Table --- filters homework by students
  homeworkData <- reactive({
    req(input$hwPicker)
    auth_student_id <- auth_student_id()
    df <- getHomeworkGrades()
    df  <- df %>% 
      filter(student_id == as.numeric(auth_student_id)) %>%
      filter(homework_id %in% input$hwPicker)
  })
  
  # Creates the data table to be displayed to the student
  output$homeworkGradeTable <- renderDT({
    df <- homeworkData()
    df <- df %>%
      select(First = first_name, Last = last_name, `Homework ID` = homework_id, Grade= grade)
    datatable(df, rownames = FALSE)
  })
  
  # Outputs a scatter plot showing homework average by assignment
  output$homeworkScatter <- renderEcharts4r({
    df <- homeworkData()
    df <- df %>%
      mutate(homework_id = as.character(homework_id))
    df %>%
      e_chart(homework_id) %>%
      e_scatter(grade, symbol_size = 15) %>%
      e_tooltip() %>%
      e_theme('dark') %>%
      e_legend(show=F)
    
    
  })
  
  
  # Grade Calc Server ---- pulls data from database and calculates homework average 
  # and total masteries, renders output boxes to be displayed to the student
  output$homeworkAVG <- renderValueBox({
    auth_student_id <- auth_student_id()
    df <- getHomeworkGrades()
    df  <- df %>% 
      filter(student_id == as.numeric(auth_student_id)) %>%
      mutate(avg = mean(grade))
    mean <- round(df$avg)
    valueBox(paste0(as.character(mean[1]), "%"), "Homework Average", icon = icon("calculator"),)
  })
  output$totalMastered <- renderValueBox({
    auth_student_id <- auth_student_id()
    df <- getReviewGrades()
    df  <- df %>% 
      filter(student_id == as.numeric(auth_student_id)) %>%
      filter(grade == "M") %>%
      distinct(topic_id) 
    total <- nrow(df)
    valueBox(total, "Topics Mastered", icon = icon("check"), color ="green")
  })
}


# Run the application 
shinyApp(ui = ui, server = server)

