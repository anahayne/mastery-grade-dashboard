# Create SQL table from R dataframe
tbl_create <- function(con, data, name) {
  copy_to(
    dest = con,
    df = data,
    name = name,
    overwrite = TRUE,
    temporary = FALSE
  )
}

# Connect to SQL database
db_connect <- function(
  server = "mydbinstance.c0eoxulijuju.us-east-2.rds.amazonaws.com",
  database = "shiny",
  uid = "datacats",
  pwd = "davidson",
  port = 1433,
  tds_version = 9.0,
  local = Sys.getenv('SHINY_PORT') == ""
) {
  if (local) {
    dbConnect(
      odbc(), 
      Driver = "ODBC Driver 17 for SQL Server",
      Server = server, 
      Database = database, 
      uid = uid, 
      pwd = pwd
    )
  } else {
    dbConnect(
      odbc(),
      Driver   = "libtdsodbc.so", 
      Database = database,
      Uid      = uid,
      Pwd      = pwd,
      Server   = server,
      Port     = port 
    )
  }
}

# Used when appending dataframes to correct quotes for SQL 
quotes <- function(df) {
  for (c in 1:ncol(df))
    if (!class(df[,c]) %in% c("numeric", "integer")){
      df[,c] <- sQuote(df[,c], options(useFancyQuotes = FALSE))
    }
  df
}
