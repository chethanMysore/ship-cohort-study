library(tidyverse)

factor_timestamp <- function(data_df, col_name){
  sample_df <- data_df %>%
    mutate_at(vars(contains(col_name)), (function(x) return(as.numeric(as.POSIXct(x)))))
  return(sample_df)
}