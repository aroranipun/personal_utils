
Sys.setenv(JAVA_HOME='c://Program Files/Java/jre1.8.0_201/') #setup your java home dir
options(java.parameters = "-Xmx4g") #setups up memory to 4gs

#Needed libraries--------------------------------
library(RJDBC)
library(stringr)
library(stringi)
library(dbplyr)
library(tictoc)
library(sqldf)
library(RMySQL)

#Read the enviorment file; in this case file containing last pass credentials import
pass<-read.csv(file = "G:/My Drive/Meta/Pass.csv")
pass<-pass[which(pass$grouping=="DBs"),]

#names(pass)
establish_connection<-function(database_name,schema=NULL){
  if(exists("conn")) {  
    dbDisconnect(conn)}
  
  # get data for URL creation------------------
  DB_Data<-pass[which(pass$name==database_name),]
  temp<-stri_split(str = DB_Data$extra,regex = "\n")
  temp<-str_split(string = temp[[1]],pattern = ":")
  Field=Value=NULL
  Field<-sapply(temp, function(x) append(Field,x[1]));  
  Value<-sapply(temp, function(x) append(Value,x[2]));
  DB_Details<-data.frame(t(data.frame(Value)),stringsAsFactors = F);colnames(DB_Details)<-Field
  if(!is.null(schema)){
    DB_Details$Database=schema
    }
  if(database_name=="snowplow"){
    AWS_connect(DB_Details)} else PGS_connect(DB_Details)
}
#Redshift Connection-------------------------------------------------
AWS_connect <- function(DB_Details) {
  #Pre-requisites
  #install.packages("RJDBC")
  #download Amazon Redshift JDBC driver
  #Download the driver directly from website and paste it in the dir
  download.file(url = 'https://s3.amazonaws.com/redshift-downloads/drivers/jdbc/1.2.20.1043/RedshiftJDBC42-no-awssdk-1.2.20.1043.jar',
                destfile =  'RedshiftJDBC42-no-awssdk-1.2.20.1043.jar')
  
  # connect to Amazon Redshift
  #driver<-JDBC(*Classname of Driver*, *File name for driver*) #Format
  driver <- JDBC(driverClass = "com.amazon.redshift.jdbc42.Driver",
                 classPath =  "RedshiftJDBC42-no-awssdk-1.2.20.1043.jar")
  #Debugging:
  #turn on elaborate bug report
  ####.jclassLoader()$setDebug(1L)
 
  JDBCURL <- paste("jdbc:redshift://", DB_Details$Hostname, sep = "")
  # url <- "<JDBCURL>:<PORT>/<DBNAME>?user=<USER>&password=<PW>
  #Connection details and creds---------------------------
  url <-
    paste(
      JDBCURL,
      ":",
      DB_Details$Port,
      "/",
      DB_Details$Database,
      "?user=",
      DB_Details$Username,
      "&password=",
      DB_Details$Password,
      sep = ""
    )
  return(dbConnect(driver, url))
}
#Postgresql Connection ()-------------------------------------------------
PGS_connect <- function(DB_Details) {
  #Connection details and creds---------------------------
  require("RPostgreSQL")
  driver <- dbDriver("PostgreSQL")
  return(dbConnect(
      driver,
      dbname = DB_Details$Database,
      host = DB_Details$Hostname,
      port = DB_Details$Port,
      user = DB_Details$Username,
      password = DB_Details$Password
  )
  )
}
#Kill COnnections------------------------
killDbConnections <- function () {
  require(RMySQL)
  all_cons <- dbListConnections(MySQL())
  
  print(all_cons)
  
  for (con in all_cons)
    +  dbDisconnect(con)
  
  print(paste(length(all_cons), " connections killed."))
  
  lapply(dbListConnections(drv = dbDriver("PostgreSQL")), function(x) {dbDisconnect(conn = x)})
  
}


#TEST-----------------------------
# dbGetQuery(conn, "select * from snowplow.information_schema.sql_features limit 10 ") #AWS
# dbGetQuery(conn, "select * from  oh.address limit 10") #SDD
# dbGetQuery(conn, "select * from  intercom.intercom_admin limit 10") #SDD
# 
# dbDisconnect(conn)

