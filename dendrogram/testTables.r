library(rjson)
library(dplyr)




# only keep the first column and skip the rest of the columns
# get rid of all the blank rows
df1 <- read.csv("testTables.csv", header=F, na.strings = c("", "NA"), colClasses=c(NA, rep("NULL", 12)))
df1 <- df1 %>% na.omit()
names(df1) <- c("name")


############################################################

retrieveData_modified <- function(df, outFilePre){
  
  sinkName <- paste0(outFilePre, ".txt")
  sink(sinkName)
  
  tableNames <- unique(df$source)
  totalTables <- length(tableNames)
  

  for (i in 1: totalTables){
    small <- df %>% filter(source == tableNames[i]) %>% 
      select(colname, selected) %>% 
      rename(name = colname) %>%
      mutate(size = 1000) %>%
      select(name, size, selected)
    
      # convert to json format
      x <- toJSON(unname(split(small, 1:nrow(small))))
      x <- paste0('"children"', ':', x)
      
      preX <- paste0('"name":', '"', tableNames[i], '"', ",")
      
      cat("{")
      cat("\n")
      cat(preX)
      cat("\n")
      cat(x)
      cat("\n")
      cat("},")
    }
  
  sink()
  file.show(sinkName)
}



retrieveData <- function(df, outFilePre){
  tableNames <- names(df)
  
  sinkName <- paste0(outFilePre, ".txt")
  sink(sinkName)
  
  for (i in 1:ncol(df)){
    
    small <- df %>% select(i) %>% na.omit() 
    small$size <- 1000
    names(small) <- c("name", "size")
    
    # convert to json format
    x <- toJSON(unname(split(small, 1:nrow(small))))
    x <- paste0('"children"', ':', x)
    
    
    preX <- paste0('"name":', '"', tableNames[i], '"', ",")
    
    cat("{")
    cat("\n")
    cat(preX)
    cat("\n")
    cat(x)
    cat("\n")
    cat("},")
  }
  
  sink()
  file.show(sinkName)
}


retrieveData2 <- function(df, outFilePre){
  
  for (i in 1:ncol(df)){
  
    small <- df %>% select(i) %>% na.omit() 
    small$size <- 1000
    names(small) <- c("name", "size")
  
    # convert to json format
    x <- toJSON(unname(split(small, 1:nrow(small))))
    x <- paste0('"children"', ':', x)
  
    sinkName <- paste0(outFilePre, i, ".txt")
    sink(sinkName)
    preX <- paste0('"name":', '"', tableNames[i], '"', ",")
    
    cat("{")
    cat("\n")
    cat(preX)
    cat("\n")
    cat(x)
    cat("\n")
    cat("},")
    sink()
    file.show(sinkName)
  }
}


# or do like this (for now)
mydf1 <- read.csv("testTables1.csv", header=T, na.strings = c("", "NA"))
retrieveData(mydf1, "administrative ")

###############################################################################

mydf2 <- read.csv("testTables2.csv", header=T, na.strings = c("", "NA"))
retrieveData(mydf2, "clinical")

###############################################################################

mydf3 <- read.csv("testTables1_1.csv", header=T, na.strings = c("", "NA"))
retrieveData_modified(mydf3, "administrative") # it will write out 'administrative.txt'
