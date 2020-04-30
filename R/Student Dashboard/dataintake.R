# DB Connnection ----
con  <- db_connect()

# Pull each data table from SQL ----
#Topics
sql_query <- 'Select * from Shiny.dbo.topics' 
df_topics <- dbGetQuery(con, sql_query)

#Reviews
sql_query <- 'Select * from Shiny.dbo.reviews' 
df_reviews <- dbGetQuery(con, sql_query)

#Homeworks
sql_query <- 'Select * from Shiny.dbo.homeworks' 
df_homeworks <- dbGetQuery(con, sql_query)

#Students
sql_query <- 'Select * from Shiny.dbo.students' 
df_students <- dbGetQuery(con, sql_query)

#Review Grades
sql_query <- 'Select * from Shiny.dbo.reviewGrades' 
df_reviewGrades <- dbGetQuery(con, sql_query)

#Homework Grades
sql_query <- 'Select * from Shiny.dbo.homeworkGrades' 
df_homeworkGrades <- dbGetQuery(con, sql_query)

# Allows values to change and be updated
reactive <- reactiveValues(df_reviewGrades = df_reviewGrades, df_homeworkGrades = df_homeworkGrades)


# Database Methods
# Get Homework Grades-- pulls grades for each student and aggregates all assignments
getHomeworkGrades <- reactive({
  merge(reactive$df_homeworkGrades, df_students) %>% merge(df_homeworks) %>%
    mutate(firstLast = paste(first_name, last_name))
}) 

# Get Review Grades -- pulls reviews for each student and aggregates all assignments
getReviewGrades <- reactive({
  merge(reactive$df_reviewGrades, df_students) %>% merge(df_reviews) %>%
    mutate(firstLast = paste(first_name, last_name))
}) 
