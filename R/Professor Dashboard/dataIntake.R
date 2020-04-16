# DB Connnection ----
con  <- db_connect()

# Pull each data table from SQL ----
sql_query <- 'Select * from Shiny.dbo.topics' 
df_topics <- dbGetQuery(con, sql_query)

sql_query <- 'Select * from Shiny.dbo.reviews' 
df_reviews <- dbGetQuery(con, sql_query)

sql_query <- 'Select * from Shiny.dbo.homeworks' 
df_homeworks <- dbGetQuery(con, sql_query)

sql_query <- 'Select * from Shiny.dbo.students' 
df_students <- dbGetQuery(con, sql_query)

sql_query <- 'Select * from Shiny.dbo.reviewGrades' 
df_reviewGrades <- dbGetQuery(con, sql_query)

sql_query <- 'Select * from Shiny.dbo.homeworkGrades' 
df_homeworkGrades <- dbGetQuery(con, sql_query)

reactive <- reactiveValues(df_reviewGrades = df_reviewGrades)

# Get Homework Grades
getHomeworkGrades <- reactive({
  merge(df_homeworkGrades, df_students) %>% merge(df_homeworks) %>%
    mutate(firstLast = paste(first_name, last_name))
}) 

# Get Review Grades
getReviewGrades <- reactive({
  merge(reactive$df_reviewGrades, df_students) %>% merge(df_reviews) %>%
    mutate(firstLast = paste(first_name, last_name))
}) 
