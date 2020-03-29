#global variables
feature_list <- list()
cols_to_remove <- list()
mutated_cols_to_remove <- list()
suffix <- "(_s0|_s1|_s2)"
min_max_vals <<- list()
imputed_data_df <- NULL

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

# select features with suffix
select_features <- function(feat_name, suffix =  "(_s0|_s1|_s2)"){
  matched <- str_detect(feat_name, suffix)
  if(matched){
    feat_base_name <- str_replace(feat_name, suffix, "")
    feature <- list.match(feature_list, str_c("^",feat_base_name,"$"))
    feat_count <- 1
    if(length(feature) > 0)
    {
      feat_count <- feature_list[[feat_base_name]] + 1
    }
    feature_list[[feat_base_name]] <<- feat_count
  }
  else{
    cols_to_remove[[feat_name]] <<- 1
  }
}


# drop columns which does not match count = 3
drop_mutated_columns <- function(feat_name, suffix = "(_s0|_s1|_s2)"){
  feat_count <- feature_list[[feat_name]]
  if(feat_count != 3 && feat_name != "female"){ # skip female_s0 feature since it is required for gender analysis
    mutated_cols_to_remove[[feat_name]] <<- 1
  }
}


# remove cols from col_names
remove_cols <- function(feat_name, unwanted_cols){
  feature_name <- str_replace(feat_name, suffix, "")
  if(feature_name %in% unwanted_cols){
    cols_to_remove[[feat_name]] <<- 1 
  }
}

# extract all features that are present in all waves/suffix
extract_features_with_suffix <- function(data_df, wave_suffix){
  suffix <<- wave_suffix
  feature_list <<- list()
  cols_to_remove <<- list()
  mutated_cols_to_remove <<- list()
  col_names <- names(data_df)
  #select features with suffix
  map(col_names, select_features)
  # add columns that are not present in all three waves to unwanted columns list
  map(names(feature_list), drop_mutated_columns)
  # remove unwanted columns from dataframe
  map(col_names, remove_cols, names(mutated_cols_to_remove))
  data_df <- data_df[, !col_names %in% names(cols_to_remove)]
  return(data_df)
}

gender_group_compare <- function (data_df1, data_df2){
  col_names <- colnames(data_df1)
  cols_to_drop <- list()
  for(col in col_names){
    cloumn <- col
    male_vals <- as.vector(select(data_df1, col))
    female_vals <- as.vector(select(data_df2, col))
    if((all(is.na(male_vals)) && !all(is.na(female_vals))) || (!all(is.na(male_vals)) && all(is.na(female_vals)))){
      cols_to_drop <- list.append(cols_to_drop, col)
    }
  }
  return(cols_to_drop)
}

# extract min, max values from the data columns
extract_min_max_values <- function(data_df){
  min_max_vals <<- list()
  sample_df <- sample_df%>%
    mutate_at(vars(names(sample_df)[which(sapply(sample_df, is.numeric))])
              ,(function(x) return((x - min(x)) / (max(x) - min(x)))))
  numeric_cols <- names(data_df)[which(sapply(data_df, is.numeric))]
  lapply(numeric_cols, function(col){
    feature_minmax = list() 
    feature_minmax$min = min(data_df[, col])
    feature_minmax$max = max(data_df[, col])
    min_max_vals[[col]] <<- feature_minmax 
  })
  return(min_max_vals)
}

impute_ageship <- function(data_df){
  for(row in 1:nrow(data_df)){
    age_ship_s0 <- imputed_data_df[row, "age_ship_s0"]
    age_ship_s1 <- imputed_data_df[row, "age_ship_s1"]
    age_ship_s2 <- imputed_data_df[row, "age_ship_s2"]
    if(is.na(imputed_data_df[row, "age_ship_s1"])){
      imputed_data_df[row, "age_ship_s1"] <<- age_ship_s0 + 5
      age_ship_s1 <- age_ship_s0 + 5
    }
    if(is.na(data_df[row, "age_ship_s2"])){
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

## sampling of data using stratified k-fold cross validation
stratified_sample <- function(data_df, kfolds, cat_colname){
  set.seed(42)
  train <- fold(data_df, k = kfolds, cat_col = cat_colname)
  train_index <- list()
  folds <- c(1:kfolds)
  for(i in folds){
    train_index[[i]] <- which(train$.folds==i)
  }
  return(train_index)
}
