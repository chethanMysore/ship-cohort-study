library(tidyverse)

data_with_labels <- function(data_df){
  shipData_with_labels <- subset(data_df, data_df$liver_fat > 0)
  return(shipData_with_labels)
}

#function to compute class label from hepatic_steatosis
calci_classLabel <- function(hepatic_steatosis){
  temp <- c()
  for(value in hepatic_steatosis){
    print(value)
    if(value > 10){
      temp <- c(temp, "Yes")
    }
    else{
      temp <- c(temp,"No")
    }
  }
  return(temp)
}


only_labels <- function(data_df){
  shipData_labelTest_1 <- subset(data_df, data_df$liver_fat > 0) %>%
    mutate(liver_fat = calci_classLabel(liver_fat)) %>%
    mutate(liver_fat = factor(liver_fat,levels = c("Yes", "No"), labels = c("Yes","No"))) %>%
    select(liver_fat)
  return(shipData_labelTest_1)
}
