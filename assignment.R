### machine learning final assignment ####

in_df <- read.csv(file = "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", sep = ",", header = T, dec = ".", na.strings = c(""," ","#DIV/0!","NA", colClasses = "dec"))
teste_final <- read.csv(file = "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", sep = ",", header = T, dec = ".")



set_library <- function(){
  library(tidyverse)
  library(caret)
  library(lubridate)
  library(randomForest)
  library(readr)
  library(forecast)
  library(gbm)
  library(stringr)
  library(impute)

} 
  
splitting_sensors <- function(){
 
  teste <- list()
  # reading_files()
  sensor_places <- c("belt","arm","dumbbell","forearm")
  for (i in seq_along(sensor_places)){
    teste[[i]] <- grep(paste("_",sensor_places[i], sep = ""),names(in_df))
  
  }
  
  # return(teste)
  
  dfs <- NULL
  for (i in seq_along(sensor_places)){
    dfs[[i]] <- in_df[,teste[[i]]]
    
  }
  return(dfs)
  
  
  
    
  
}

playing_belt <- function(){
  belt <- splitting_sensors()[[1]]
  rows <- nrow(belt)
  x = NULL
  c = 0
  for (i in seq_len(ncol(belt))){
    
    soma <- sum(is.na(belt[,i]))
    if (soma/rows >=.80){
      c <- c + 1
      x[c] <- i
    }
    
  }
  belt <- belt[,-x]
  
  
  for (i in seq_len(ncol(belt))){
    belt[,i] <- as.numeric(belt[,i])
    
  }
  
  
  belt <- treating_NA(belt)
  belt <- as.data.frame(belt)
  return(belt)
  
  
}

treating_NA <- function(nadf){
  nadf <- impute.knn(as.matrix(nadf))$data
  return(nadf)
  
}

