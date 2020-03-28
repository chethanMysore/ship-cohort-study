
imputed_data_df <- NULL

impute_ageship <- function(data_df){
  for(row in 1:nrow(sample_df)){
    age_ship_s0 <- imputed_data_df[row, "age_ship_s0"]
    age_ship_s1 <- imputed_data_df[row, "age_ship_s1"]
    age_ship_s2 <- imputed_data_df[row, "age_ship_s2"]
    if(is.na(imputed_data_df[row, "age_ship_s1"])){
      imputed_data_df[row, "age_ship_s1"] <<- age_ship_s0 + 5
      age_ship_s1 <- age_ship_s0 + 5
    }
    if(is.na(sample_df[row, "age_ship_s2"])){
      imputed_data_df[row, "age_ship_s1"] <<- age_ship_s1 + 5
    }
  }
}

## Imputing data with mean and mode
data_imputation <- function(colname, data_df){
  coldata<-as.vector(imputed_data_df[colname][[1]])
  imputed_data<-coldata
  if(is.factor(coldata) || is.character(coldata)){
    repval<-Mode(coldata, na.rm = TRUE)
    imputed_data <- as.factor(sapply(coldata, FUN = function(x) x = ifelse(is.na(x), repval, x)))
  }
  imputed_data_df[colname][[1]] <<- imputed_data
}

impute_dataset <- function(data_df){
  imputed_data_df <<- data_df
  
  ## Impute age column  
  impute_ageship(data_df)
  
  ## Impute numeric data
  imputed_data_df <<- impute_mean_if(imputed_data_df, .predicate = is.numeric)
  
  ## Impute other columns
  invisible(capture.output(map(names(data_df),data_imputation)))  
  
  return(imputed_data_df)
}

