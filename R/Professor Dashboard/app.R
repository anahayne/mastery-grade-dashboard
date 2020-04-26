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
source("libraries.R", local = TRUE)
source("dataIntake.R", local = TRUE)

# UI ----
ui <- dashboardPage(
    skin = "red",
    
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
                , HTML("<center><h1> Mastery Gradebook Dashboard </h1></center>")
                , div(img(src="davidsonCollege.jpg"), style="text-align: center;")
                , HTML("<center> <h3> Software Design, Group 3. <br> Gracie Petty, Abby Santiago, Ben Santiago, Brad Shook, Katie Turner, Ana Hayne & Owen Bezick </h3></center>")
            )
            # View Review UI ----
            , tabItem(
                tabName = "reviewGrades"
                ,fluidRow(
                    box(width = 12, title = "Filter:", status = "danger" 
                        ,column(width = 6
                                ,uiOutput("reviewStudentPicker")
                        )
                        , column(width = 6
                                 ,uiOutput("reviewPicker")
                        )
                    )
                )
                , fluidRow(
                    box(width = 6, status = "danger", title = "Review Grades", height = "550"
                        , DTOutput("totalReviewGrades")
                    )
                    , box(width = 6, height = "550", stauts = "danger", title = "Total Grades", status = "danger"
                          , echarts4rOutput("gradeBar"))
                )
            )
            
            # View Homework UI ----
            , tabItem(
                tabName = "homeworkGrades"
                , fluidRow(
                    box(width = 12, title = "Filter:", status = "danger" 
                        ,column(width = 6
                                , uiOutput("hwStudentPicker")
                        )
                        , column(width = 6
                                 ,uiOutput("hwPicker")
                        )
                    )
                )
                , fluidRow(
                    box(width = 6, status = "danger", height= "550", title = "Homework Grades"
                        , DTOutput("homeworkGradeTable")
                    )
                    , box(width = 6, status = "danger", height= "550", title = "Homework Averages"
                          , echarts4rOutput("avgHomeworkGraph")
                    )
                )
            )
            # Edit Review Grades ----
            , tabItem(
                tabName = "editReviewGrades"
                , actionBttn(inputId = "addReview", label = "Add Review", style = "fill", color = "danger", block = T)
                , fluidRow(
                    box(width = 12, status = "danger", title = "Edit Review Grades"
                        , column(width = 12
                                 , DTOutput("totalEditReviewGrades")
                        )
                    )
                )
            )
            # Edit Homework UI ----
            , tabItem(
                tabName = "editHomeworkGrades"
                , actionBttn(inputId = "addHW", label = "Add Homework Assignment", style = "fill", color = "danger", block = T)
                , fluidRow(
                    box(width = 12, status = "danger", title = "Edit Homework Grades"
                        , DTOutput("editHomeworkGrades")
                    )
                )
            )
            
        )
    )
)


# Define server logic 
server <- function(input, output) {
    # View Review Server ---- 
    # List of students by ID
    ls_studentsR <- reactive({
        df <- getReviewGrades()
        df %>% distinct(firstLast) %>% pull()
    })
    # List of students by first_name
    #List of reviews
    ls_reviews <- reactive({
        df <- getReviewGrades()
        df %>% distinct(review_id) %>% pull()
    })
    # Student Picker
    output$reviewStudentPicker <- renderUI({
        pickerInput("reviewStudentPicker"
                    ,"Student"
                    , choices = ls_studentsR()
                    , selected = ls_studentsR()
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
        req(input$reviewStudentPicker, input$reviewPicker)
        df <- getReviewGrades()
        df <- df %>%
            filter(review_id %in% input$reviewPicker, firstLast %in% input$reviewStudentPicker) %>%
            select( First = first_name, Last = last_name,`Review ID` = review_id, Topic = topic_id, Grade = grade)
        datatable(df, rownames = FALSE)
    })
    
    # Total Grades Chart
    output$gradeBar <- renderEcharts4r({
        req(input$reviewStudentPicker, input$reviewPicker)
        df <- getReviewGrades() %>%
            filter(review_id %in% input$reviewPicker, firstLast %in% input$reviewStudentPicker) %>%
            select(grade) %>%
            count(grade)
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
    # List of students by firstLast
    ls_studentsHW <- reactive({
        df <- getHomeworkGrades()
        df %>% distinct(firstLast) %>% pull()
    })
    #List from homework
    ls_homeworksHW <- reactive({
        df <- getHomeworkGrades()
        df %>% distinct(homework_id) %>% pull()
    })
    # Student Picker
    output$hwStudentPicker <- renderUI({
        pickerInput("hwStudentPicker"
                    ,"Student"
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
            filter(firstLast %in% input$hwStudentPicker) %>%
            filter(homework_id %in% input$hwPicker) %>%
            select(First = first_name, Last = last_name, `Homework ID` = homework_id, Grade= grade)
        
        datatable(df, rownames = FALSE)
    })
    
    # Homework Average Graph
    # Data
    hwAvg <- reactive({
        req(input$hwStudentPicker, input$hwPicker)
        df <- getHomeworkGrades()
        df  <- df %>% 
            filter(firstLast %in% input$hwStudentPicker) %>%
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
    # DT output
    reviewGradesData <- reactive({
        df <- getReviewGrades()
        df <- df %>%
            select(First = first_name, Last = last_name,`Review Id` = review_id,Topic = topic_id, Grade = grade)
    })
    
    output$totalEditReviewGrades <- renderDT({
        df <- reviewGradesData()
        datatable(df, rownames = FALSE
                  , selection = list(mode = 'single', target = 'row')
                  , filter = 'top', caption = "Click a Row to Edit")
    })
    
    observeEvent(input$totalEditReviewGrades_rows_selected,{
        rowNumber <- input$totalEditReviewGrades_rows_selected
        df <- reviewGradesData()
        rowData <- df[rowNumber, ]
        showModal(
            modalDialog(title = "Edit Grade", easyClose = T
                        ,box(width = 12, status = "danger"
                             , HTML("<b> Name: </b>")
                             , renderText(paste(rowData$First, rowData$Last))
                             , HTML("<b> Topic ID: </b>")
                             , renderText(rowData$Topic)
                             , pickerInput("grade", "Grade:", choices = c("M", "J", "A", "NA")
                                           , selected = as.character(rowData$Grade))
                        )
                        , footer = fluidRow(
                            column(width = 6
                                   , actionBttn("gradeSave"
                                                , "Save"
                                                , icon = icon("save")
                                                , style = "material-flat"
                                                , block = T
                                   )
                            )
                            , column(width = 6
                                     , actionBttn("gradeDismiss"
                                                  , "Dismiss"
                                                  , icon = icon("close")
                                                  , style = "material-flat"
                                                  , block = T)
                            )
                        )
            )
        )
    })
    
    # When the "Grade Dismiss" button is pressed
    observeEvent(input$gradeDismiss,{
        removeModal()
    })
    
    #When the "Save Grade" button is pressed
    observeEvent(input$gradeSave,{
        rowNumber <- input$totalEditReviewGrades_rows_selected
        df <- reviewGradesData()
        rowData <- df[rowNumber, ]
        topic_id <- rowData$Topic
        newGrade <- as.character(input$grade)
        review_id <- rowData[1, 3]
        
        df <- df_students %>%
            filter(first_name == rowData$First) %>%
            filter(last_name == rowData$Last) 
        
        student_id <- df$student_id
        
        # Write to Database
        sql_query <- paste0("update Shiny.dbo.reviewGrades set grade = '", newGrade, "' where (topic_id = ", topic_id, " and student_id = ", student_id, " and review_id = ", review_id, ")")
        dbExecute(con, sql_query)
        
        # Background App Refresh
        sql_query <- 'Select * from Shiny.dbo.reviewGrades'
        df_reviewGrades <- dbGetQuery(con, sql_query)
        reactive$df_reviewGrades <- df_reviewGrades
        
        showNotification("Changes Saved to Remote Database.", type = c("message"), duration = 3)
        
        removeModal()
    })
    

    #
    #  Add Review Button press ----
    #
    observeEvent(input$addReview, {
        showModal(
            modalDialog(title = "Add a Review",  easyClose = T
                        , box(width = 12, status = "danger", title = "Review Information"
                              , fluidRow(
                                  column(width = 6
                                         , numericInput(inputId = "addReviewID", label = "Review ID", value = 1 + max(df_reviews$review_id))
                                         , dateInput(inputId = "addReviewStartDate", label = "Date Assigned", value = 1 + max(df_reviews$date_assigned))
                                  )
                                  , column(width = 6
                                           , textInput(inputId = "addReviewTitle", label = "Review Title", value = "Title")
                                           , dateInput(inputId = "addReviewEndDate", label = "Date Due", value = 1 + max(df_reviews$date_due))
                                  )
                                  , column(width = 12
                                           , textAreaInput(inputId = "addReviewDesc", label = " Review Description", value = "Description"))
                              )
                        )
                        , footer = fluidRow(
                            column(width = 6
                                   , actionBttn("saveReview"
                                                , "Save"
                                                , icon = icon("save")
                                                , style = "material-flat"
                                                , color = "danger"
                                                , block = T
                                   )
                            )
                            , column(width = 6
                                     , actionBttn("addReviewDismiss"
                                                  , "Dismiss"
                                                  , icon = icon("close")
                                                  , style = "material-flat"
                                                  , color = "danger"
                                                  , block = T)
                            )
                        )
            )
        )
    })
    # Dismiss
    observeEvent(input$addReviewDismiss,{
        removeModal()
    })
    
    observeEvent(input$saveReview, {
        con <- db_connect()
        row <- data_frame(Review_id = c(as.numeric(input$addReviewID))
                          , date_assigned = c(as.character(input$addReviewStartDate))
                          , date_due = c(as.character(input$addReviewEndDate))
                          , title = c(as.character(input$addReviewTitle))
                          , description = c(as.character(input$addReviewDesc))
        )
        
        query <- sqlAppendTable(con, "shiny.dbo.reviews", quotes(row), row.names = FALSE)
        
        query_character <- as.character(query)
        noDouble <- gsub('"',"",query)
        noNew <- gsub('\n'," ",noDouble)
        dbSendQuery(con, noNew)
        
        showNotification(paste0("Review Added as: ", as.character(input$addReviewID)))
    })
    
    # Edit Homework Server ----
    # DT output
    homeworkGradesData <- reactive({
        df <- getHomeworkGrades()
        df <- df %>%
            select(First = first_name, Last = last_name,`Homework Id` = homework_id, Grade = grade)
    })
    
    output$editHomeworkGrades <- renderDT({
        df <- homeworkGradesData()
        datatable(df, rownames = FALSE, selection = list(mode = 'single', target = 'row'), filter = 'top', caption = "Click a Row to Edit")
    })
    
    observeEvent(input$editHomeworkGrades_rows_selected,{
        rowNumber <- input$editHomeworkGrades_rows_selected
        df <- homeworkGradesData()
        rowData <- df[rowNumber, ]
        showModal(
            modalDialog(title = "Edit Grade", easyClose = T
                        ,box(width = 12, status = "danger"
                             , HTML("<b> Name: </b>")
                             , renderText(paste(rowData$First, rowData$Last))
                             , HTML("<b> Homework ID: </b>")
                             , renderText(rowData[1,3])
                             , numericInput("hwGrade", "Grade:",  value = as.numeric(rowData$Grade), max = 100)
                        )
                        , footer = fluidRow(
                            column(width = 6
                                   , actionBttn("hwgradeSave"
                                                , "Save"
                                                , icon = icon("save")
                                                , style = "material-flat"
                                                , block = T
                                                , color = "danger"
                                   )
                            )
                            , column(width = 6
                                     , actionBttn("hwgradeDismiss"
                                                  , "Dismiss"
                                                  , icon = icon("close")
                                                  , style = "material-flat"
                                                  , color = "danger"
                                                  , block = T)
                            )
                        )
            )
        )
    })
    
    # Dismiss
    observeEvent(input$hwgradeDismiss,{
        removeModal()
    })
    #When the "Save Grade" button is pressed
    observeEvent(input$hwgradeSave,{
        rowNumber <- input$editHomeworkGrades_rows_selected
        df <- homeworkGradesData()
        rowData <- df[rowNumber, ]
        newGrade <- as.character(input$hwGrade)
        hw_ID <- rowData[1, 3]
        
        df <- df_students %>%
            filter(first_name == rowData$First) %>%
            filter(last_name == rowData$Last) 
        
        student_id <- df$student_id
        
        # Write to Database
        sql_query <- paste0("update Shiny.dbo.homeworkGrades set grade = '", newGrade, "' where (homework_id = ", hw_ID, " and student_id = ", student_id, ")")
        dbExecute(con, sql_query)
        
        # Background App Refresh
        sql_query <- 'Select * from Shiny.dbo.homeworkGrades'
        df_homeworkGrades <- dbGetQuery(con, sql_query)
        reactive$df_homeworkGrades <- df_homeworkGrades
        
        showNotification("Changes Saved to Remote Database.", type = c("message"), duration = 3)
        removeModal()
    })
    
    #
    #  Add HW Button press ----
    #
    observeEvent(input$addHW, {
        showModal(
            modalDialog(title = "Add a Homework",  easyClose = T
                        , box(width = 12, status = "danger", title = "Homework Information"
                              , fluidRow(
                                  column(width = 6
                                         , numericInput(inputId = "hwAddID", label = "Homework ID", value = 1 + max(df_homeworks$homework_id))
                                         , dateInput(inputId = "addHWStartDate", label = "Date Assigned", value = 1 + max(df_homeworks$date_assigned))
                                  )
                                  , column(width = 6
                                           , textInput(inputId = "hwAddTitle", label = "Homework Title", value = "Title")
                                           , dateInput(inputId = "addHWEndDate", label = "Date Due", value = 1 + max(df_homeworks$date_due))
                                  )
                                  , column(width = 12
                                           , textAreaInput(inputId = "hwAddDesc", label = " Homework Description", value = "Description"))
                              )
                        )
                        , footer = fluidRow(
                            column(width = 6
                                   , actionBttn("saveHW"
                                                , "Save"
                                                , icon = icon("save")
                                                , style = "material-flat"
                                                , block = T
                                                , color = "danger"
                                   )
                            )
                            , column(width = 6
                                     , actionBttn("hwAddDismiss"
                                                  , "Dismiss"
                                                  , icon = icon("close")
                                                  , style = "material-flat"
                                                  , color = "danger"
                                                  , block = T)
                                     
                            )
                        )
            )
        )
    })
    # Dismiss
    observeEvent(input$hwAddDismiss,{
        removeModal()
    })
    observeEvent(input$saveHW, {
        con <- db_connect()
        row <- data_frame(homework_id = c(as.numeric(input$hwAddID))
                               , date_assigned = c(as.character(input$addHWStartDate))
                               , date_due = c(as.character(input$addHWEndDate))
                               , title = c(as.character(input$hwAddTitle))
                               , description = c(as.character(input$hwAddDesc))
        )
        
        query <- sqlAppendTable(con, "shiny.dbo.homeworks", quotes(row), row.names = FALSE)
        
        query_character <- as.character(query)
        noDouble <- gsub('"',"",query)
        noNew <- gsub('\n'," ",noDouble)
        dbSendQuery(con, noNew)
        
        showNotification(paste0("Homework Added as: ", as.character(input$hwAddTitle)))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)