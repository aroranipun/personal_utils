#Get Required libraries--------------------------------
library("stringr")
library("stringi")
library("RPostgreSQL")
library("RMySQL")
#Postgresql Connection ()-------------------------------------------------
PGS_connect <- function(creds) {
  #Connection details and creds---------------------------
  
  driver <- dbDriver("PostgreSQL")
  
  return(dbConnect(driver,
                   dbname = creds$DB,
                   host = creds$Hostname,
                   port = creds$PORT,
                   user = creds$USER,
                   password = creds$PW
  )
  )
}
#Kill COnnections------------------------
killDbConnections <- function () {

  all_cons <- dbListConnections(MySQL())
  
  print(all_cons)
  
  for (con in all_cons)
    +  dbDisconnect(con)
  
  print(paste(length(all_cons), " connections killed."))
  
  lapply(dbListConnections(drv = dbDriver("PostgreSQL")), function(x) {dbDisconnect(conn = x)})
  
}
#GEt sql
getSQL <-
  function(filepath) {
    con = file(filepath, "r")
    sql.string <- ""
    
    while (TRUE) {
      line <- readLines(con, n = 1,warn = F)
      
      if (length(line) == 0) {
        break
      }
      
      line <- gsub("\\t", " ", line)
      
      if (grepl("--", line) == TRUE) {
        line <- paste(sub("--", "/*", line), "*/")
      }
      
      sql.string <- paste(sql.string, line)
    }
    
    close(con)
    return(sql.string)
  }
#get creds---------
get_creds<-function(match_string) {
  creds = list()
  creds$PORT = Sys.getenv(paste(match_string, "PORT", sep = "_"))
  creds$DB = Sys.getenv(paste(match_string, "DATABASE", sep = "_"))
  creds$USER = Sys.getenv(paste(match_string, "USER", sep = "_"))
  creds$PW = Sys.getenv(paste(match_string, "PASS", sep = "_"))
  creds$Hostname = Sys.getenv(paste(match_string, "HOST", sep = "_"))
  return(creds)
}

#function to create connection--------------------------------
establish_connection<-function(creds){
  if(exists("conn")) {  
    dbDisconnect(conn)}
  return( PGS_connect(creds))
}
