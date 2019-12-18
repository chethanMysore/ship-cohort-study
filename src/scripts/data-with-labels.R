library(tidyverse)

data_with_labels <- function(data_df){
  shipData_with_labels <- subset(data_df, data_df$liver_fat > 0)
  return(shipData_with_labels)
}

#function to compute class label from hepatic_steatosis
calci_classLabel <- function(hepatic_steatosis){
  temp <- c()
  for(value in hepatic_steatosis){
    if(value > 10){
      temp <- c(temp, 1)
    }
    else{
      temp <- c(temp, 0)
    }
  }
  return(temp)
}


only_labels <- function(data_df = NULL, labels = NULL){
  shipData_labelTest_1 <- NULL
  if(!is.null(data_df)){
    shipData_labelTest_1 <- subset(data_df, data_df$liver_fat > 0) %>% # this is probably not required
      mutate(liver_fat = calci_classLabel(liver_fat)) %>%
      mutate(liver_fat = factor(liver_fat,levels = c(1, 0), labels = c(1,0))) %>%
      select(liver_fat)
  }
  else if(!is.null(labels)){
    labels <- as.data.frame(labels)
    labels$liver_fat <- labels[[1]]
    shipData_labelTest_1 <- subset(labels, labels$liver_fat > 0) %>%
      mutate(liver_fat = calci_classLabel(liver_fat)) %>%
      mutate(liver_fat = factor(liver_fat,levels = c(1, 0), labels = c(1,0))) %>%
      select(liver_fat)
  }
  return(shipData_labelTest_1)
}
