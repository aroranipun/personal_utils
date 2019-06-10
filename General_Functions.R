# General Functions----------------------------------------------------------------------------------
# Package install list

# install.packages(c("XLConnect","Rcmdr","tictoc","data.table","chron","stringr"),dependencies=T)
# #General
# install.packages(c("psych"),dependencies=T)
# #STATS
# install.packages(c("ggplot2"),dependencies=T)
# #PLOTS
# install.packages(c("dbplyr","sqldf","RJDBC","RPostgreSQL"),dependencies=T)
# #DBCONNECTIONS
# install.packages(c("uuid"),dependencies=T)
# #Randomnumbergen
# install.packages("tictoc",dependencies = T)
# install.packages("rlist","RJSONIO") 
# #handling JSON data
# if(!requireNamespace("BiocManager",quietly=TRUE))install.packages("BiocManager")
# BiocManager::install("graph",version="3.8")
# BiocManager::install("RBGL",version="3.8")
# BiocManager::install("Rgraphviz",version="3.8")
# install.packages("listviewer")
# install.packages("beepr")
# install.packages(c("dplyr","tidyr"))

#Package installing if needed
bringpackage <-  function(packages_needed) {
  installed <- data.frame(installed.packages())
  
  existing <-
    packages_needed[which(packages_needed %in% installed$Package)]
  install <-
    packages_needed[which(!packages_needed %in% installed$Package)]
  
  for (i in existing) {
    lapply(existing, library, character.only  = TRUE)
  }
  
  for (i in install) {
    tryCatch(expr = install.packages(i))
    lapply(i, library, character.only  = TRUE)
  }
}



bringpackage(c("dplyr","tidyr"))
#List of unique elements in all coluns of a data frame
u_col<-function(dataframe){
  return(sapply(dataframe,function(x) unique(x)))
}
#Length of unique elements in a vector
len_unique<-function(x){
  return(length(unique(x)))
}
#Length of True elements in a logical vector 
len_which <- function(x) {
  
    return (length(which(x)))
}
#Length of NA elements in a logical vector 
len_na <- function(x) {
  
  return (length(which(is.na(x))))
}

#List of na elements in all coluns of a data frame
NAs<-function(dataframe){
  return(sapply(dataframe,function(x) which(is.na(x))))
}

#Frequency Table
Freq_tb <- function(x,
                    y = NA,
                    Greater_Than_y = F) {
  t <- data.frame(table(x))
  
  if (!is.na(y)) {
    if (Greater_Than_y) {
      return(t[which(t$Freq > y),])
    } else
    {
      return(t[which(t$Freq == y),])
    }
  } else
  {
    return(t)
  }
}


#Data Wranggling---------------------------------------------------
DFtolower <- function(dataframe,
                      col.index = NULL,
                      col.string = NULL,
                      col.class = NULL) {
  cols <- NULL
  
  if (!length(col.index) == 0) {
    cols <- append(cols, col.index)
  }
  if (!length(col.string) == 0) {
    matches <- grep(pattern = paste(col.string,collapse = "|"), names(dataframe))
    cols <- append(cols, matches)
  }
  if (!length(col.class) == 0) {
    matches <- which(sapply(dataframe, class) == col.class)
    cols <- append(cols, matches)
  }
  cols <- unique(cols)
  for (i in cols) {
    dataframe[, i] <- tolower(dataframe[, i])
  }
  print(paste("changes made to ", paste(names(dataframe[cols]), collapse = (", "))))
  return(dataframe)
}


#Data cleaning---------------------------------------------------
is_outlier<-function(x) {
  return(x < quantile(x, 0.25) - 1.5*IQR(x) | x > quantile(x, 0.75) + 1.5*IQR(x))}

Create_outlier_column<-function(Data,Col,Group){
  #Data= Data File
  #Col: Columns in which outlier need to be found
  #Group: Columns in which you have variable acc. to which groups sub-sets needs to be formed
  
  out<-do.call(data.frame,aggregate(x = Data[,Col],by=list(Data[,Group]),FUN=is_outlier))
  Outlier<-data.frame(NULL)
  for(i in 1: length(unique(Data[,Group]))){
    df<-data.frame(rep(x = as.character(out[i,1]),length(out[1,-1])),as.character(out[i,-1]))
    names(df)<-c(Group,paste("Outlier-",Col))
    Outlier<-rbind(Outlier,df)
  }
  Data<-cbind(Data,Outlier[,2]); names(Data)[length(names(Data))]<-paste("Outlier-",Col)
  return(Data)
}

UUID_assign <- function(Change_vector) {
  source_table_key_Var <-
    data.frame(Variable = unique(Change_vector),
               UUID =
                 tolower(replicate(
                   length(unique(Change_vector)),
                   uuid::UUIDgenerate(use.time = TRUE)
                 )))
  changes<-
    vlookup(Change_vector = Change_vector, source_table_key_Var = source_table_key_Var)
  return(changes)
}

vlookup <-
  function(Change_vector,
           source_table_key_Var,
           na_treatment = 0) {
    # Change_vector:    Column in DF which needs to be changed
    # source_table_Var_key: DF with @ columns- var and key where Var has same calues as change vector
    # na_treatment: 0- Leave NAs   1- ORiginal value   2- replace with UUID
    
    source_table_key_Var<-source_table_key_Var[which(!is.na(source_table_key_Var[1])),]
    
    
  #  if(len_which(is.na(Change_vector)>0)) stop("Change_vector contains NAs")
    
    temp <-
      data.frame(Variable = Change_vector, Key = seq(1:length(Change_vector)))
    temp <-
      merge(
        x = temp,
        y = source_table_key_Var,
        by.x = "Variable",
        by.y = names(source_table_key_Var)[1],
        all.x = T
      )
    
    #Handle NAs-------------------------
    
    na_rows <- which(is.na(temp[3]))
    if (length(na_rows) != 0) {
      if (na_treatment == 1) {
        temp[na_rows, 3] <- as.character(temp[na_rows, 1])
      } else if (na_treatment == 2) {
        temp[na_rows, 3] <- UUID_assign(Change_vector = temp[na_rows, 1])
      }
    }
    which(temp$Variable=="afe69303-21ff-4798-94c0-02d2122400e3");temp[2915,]
    #Getting value and indexes--------------------
    temp$Variable = temp[, 3]
    temp <- temp[order(temp$Key), ]
    na_rows <- which(is.na(temp[3])) #Getting NA indexes for re-orderd rows
    
    new_values = as.character(temp$Variable)
    if (length(na_rows) != 0) {
      values_changed = temp$Key[-na_rows]
    } else{
      values_changed = temp$Key
      na_rows="No NA Rows"
    }
    return(list(new_values=new_values,values_changed=values_changed,na_rows=na_rows))
  }
# remove NA
remove_NA<-function(x){
  nas<-which(is.na(x))
  if(length(nas)>0){
    return(x[-nas])
  }else return(x)
}

#UUID Check

Which_UUID <- function(x) {
  grep(
    "^[a-fA-F0-9]{8}-[a-fA-F0-9]{4}-[a-fA-F0-9]{4}-[a-fA-F0-9]{4}-[a-fA-F0-9]{12}$",
    x
  )
}

#Graph Based grouping--------------------------------------

find_graph_groups<-function(pairs){
  require(graph)
  require("RBGL")
  require(Rgraphviz)
  
  test <-
    ftM2graphNEL(as.matrix(pairs))
  
  cc <- connectedComp(test)
  ## Massage results into the format you're after
  ld <- lapply(seq_along(cc),
               function(i)
                 data.frame(group = names(cc)[i], id = cc[[i]]))
  return( do.call(rbind, ld))
}

diff_appened <- function(x, add_at_end = F) {
  if (add_at_end) {
    return(c(diff(x), 0))
  } else
    return(c(0, diff(x)))
}
# Create folder------------------------------------
dir_skeleton<-function() {
  folders <-
    c(
      "data",
      "data/raw",
      "data/temp",
      "output",
      "src",
      "src/r",
      "src/sql",
      "src/python",
      "models",
      "code"
    )
  for (i in folders) {
    dir.create(path = i,
               recursive = F,
               showWarnings = T)
  }
}

code_skeleton<-function(){
  files <- c("1_get_data.R",
             "2_clean_up.R",
             "3_feature creation.R")
  
  for(i in files){
    file.create(paste("code/",files,sep=""))
  }
  
  file.create(paste("src/r/functions_1.R",sep=""))
}


#Get consecutive distance--------------------
dist_consecutive<-function(lat,long){
  require(geosphere)
  lat=as.numeric(lat)
  long=as.numeric(long)
  
  x=as.matrix(data.frame(long,lat))
  y = as.matrix(data.frame(
    long = lag(long, default = long[1]),
    lat = lag(lat, default = lat[1])
  ))
  distance<-NULL
  for(i in 1:nrow(x)){
    distance<-append(distance,distHaversine(p1 = x[i,],p2 = y[i,], r=6378137))
  }
  return(distance)
}

#Create sub-groups for 
sub_groups <- function(x,
                       value_based = F,
                       range_based = F,
                       diff_based = F,
                       delta = NA,
                       use_absolute = T) {
  k = 1
  session = c(k)
  if (length(x) == 1) {
    return(session)
  } else {
    for (i in 2:length(x)) {
      if (value_based) {
        change = x[i] != x[i - 1]
      } else if (range_based) {
        change = x[i] >= delta
      } else {
        compare = ifelse(test = use_absolute,
                         yes = abs(x[i] - x[i - 1]) ,
                         no = x[i] - x[i - 1])
        
        change = compare >= delta
      }
      if (change) {
        k = k + 1
      }
      session = append(session, k)
    }
  }
  return(session)
}
# x = c(1,2,4,4,6,7,32,33,34,30)
# sub_groups(x = x,value_based = F,value_if_diff_based = 3,use_absolute = T)
# x = c(1,2,4,4,6,1,2,4,2,1)
# sub_groups(x = x,range_based = T,delta = 4)
# diff_appened(x)

standardize <- function(data, cols, type) {
  require(dplyr)
  existing <- names(data) [which(names(data) %in% cols)]
  new <- cols [which(!cols %in% names(data))]
  
  data <- add_columns(data = data, cols = new)
  data<-as.data.frame(data)
  for (i in 1:length(type)) {
    col=which(names(data)==cols[i])
    if (type[i] == "char") {
      data[, col]  <- as.character(data[, col])
    }
    if (type[i] == "numeric") {
      data[, col] <- as.numeric(data[, col])
    }
    #logical cannot handle characterized numeric values properly
    # if (type[i] == "logical") {
    #   if(class(data[, col]))
    #   data[, col]  <- as.logical(data[, col])
    # }
  }
  return(data %>% select(cols))
}

add_columns <- function(data, cols) {
  data[, cols] <- NA
  return(data)
}






