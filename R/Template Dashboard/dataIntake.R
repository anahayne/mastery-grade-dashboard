# Script to read in data from excel sheets
# Libraries are loaded from app.R
# data is a 'container' filled with reactive values. Reactive values are values in R that can be easily recalculated depending on the reactivity of the app
data <- reactiveValues()

data$review_a <- read_excel("example_gradebook.xlsx", sheet = "review_a") # Reads in excel sheet and stores in the reactive values container

data$grade_codebook <- read_excel("example_gradebook.xlsx", sheet = "grade_codebook")

data$student_names <- read_excel("example_gradebook.xlsx", sheet = "names")

# DB Connnection ----
con  <- db_connect()

# Pull each data table from SQL ----
sql_query <- 'Select * from Shiny.dbo.TABLENAME' 
df_campus <- dbGetQuery(con, sql_query)