library(tidyverse)

factor_timestamp <- function(data_df, col_name){
  sample_df <- data_df %>%
    mutate_at(vars(contains(col_name)), (function(x) return(as.numeric(as.POSIXct(x)))))
  return(sample_df)
}

factor_hms <- function(data_df_1, col_name_1){
  sample_df <- data_df_1 %>%
    mutate_at(vars(contains(col_name_1)), (function(x) return(as.numeric(as_hms(x)))))
  return(sample_df)
}

factor_liver_fat <- function (data_df, col_name_2){
  sample_df <- data_df %>%
    mutate(factored_liver_fat=factor(liver_fat,levels = c("Yes","No"),labels = c(1,0)))
  return((sample_df))
}