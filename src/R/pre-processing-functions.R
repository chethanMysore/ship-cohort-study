## global variables
feature_list <- list()
cols_to_remove <- list()
mutated_cols_to_remove <- list()
suffix <- "(_s0|_s1|_s2)"
min_max_vals <<- list()
imputed_data_df <- NULL

#' Extract all participants with continuous target variable value
#' 
#' @param data_df Data frame
#'
#' @return data frame with labels
#'
#' @export
#'
data_with_labels <- function(data_df){
  shipData_with_labels <- subset(data_df, data_df$liver_fat > 0)
  return(shipData_with_labels)
}

#' Compute class label
#'
#' Discretize target variable with threshold 0.1
#'
#' @param heaptic_steatosis liver fat level recorded in percentage
#'   
#' @return class label  
#'   
#' @export
#' 
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

#' Extract labels from dataset
#' 
#' @param data_df Optional data frame. If not specified, use the labels passed and return computed class labels
#' @param labels Optional list of liverfat levels. If not specified, use data frame passed to extract labels
#' 
#' @return list of class labels
#' 
#' @export
#'
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

#' Factor timestamp using POSIXct
#'
#' @param data_df data frame
#' @param col_name feature to factor
#' 
#' @return factored data frame
#' 
#' @export
#'
factor_timestamp <- function(data_df, col_name){
  data_df <- data_df %>%
    mutate_at(vars(contains(col_name)), (function(x) return(as.numeric(as.POSIXct(x)))))
  return(data_df)
}

#' Factor hms column
#'
#' @param data_df data frame
#' @param col_name feature to factor
#' 
#' @return factored data frame
#' 
#' @export
#'
factor_hms <- function(data_df, col_name){
  data_df <- data_df %>%
    mutate_at(vars(contains(col_name)), (function(x) return(as.numeric(as_hms(x)))))
  return(data_df)
}

factor_liver_fat <- function (data_df, col_name_2){
  sample_df <- data_df %>%
    mutate(factored_liver_fat=factor(liver_fat,levels = c("Yes","No"),labels = c(1,0)))
  return((sample_df))
}

#' Select features present in all waves
#'
#' @param feat_name feature name
#' @param suffix Optional wave suffix
#'
#' @export
#' 
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

#' Drop features that are not present in all 3 waves or is 'female_s0'
#' 
#' @param feat_name feature name
#' @param suffix wave suffix
#'
#' @export
#'
drop_mutated_columns <- function(feat_name, suffix = "(_s0|_s1|_s2)"){
  feat_count <- feature_list[[feat_name]]
  if(feat_count != 3 && feat_name != "female"){ # skip female_s0 feature since it is required for gender analysis
    mutated_cols_to_remove[[feat_name]] <<- 1
  }
}

#' Drop columns without wave suffix
#'
#' @param feat_name feature name
#' @param unwanted_cols features without wave suffix
#'
#' @export
#'
remove_cols <- function(feat_name, unwanted_cols){
  feature_name <- str_replace(feat_name, suffix, "")
  if(feature_name %in% unwanted_cols){
    cols_to_remove[[feat_name]] <<- 1 
  }
}

#' Extract all features present in all 3 waves
#' 
#' @param data_df data frame
#' @param wave_suffix wave suffix
#'
#' @return data frame with features in all 3 waves
#'
#' @export
#'
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

#' Group participants based on gender and compare
#' 
#' @param male_df data frame with male participants
#' @param female_df data frame with female participants
#'
#' @return list of gender specific features to drop
#'
#' @export
#'
gender_group_compare <- function (male_df, female_df){
  col_names <- colnames(male_df)
  cols_to_drop <- list()
  for(col in col_names){
    cloumn <- col
    male_vals <- as.vector(select(male_df, col))
    female_vals <- as.vector(select(female_df, col))
    if((all(is.na(male_vals)) && !all(is.na(female_vals))) || (!all(is.na(male_vals)) && all(is.na(female_vals)))){
      cols_to_drop <- list.append(cols_to_drop, col)
    }
  }
  return(cols_to_drop)
}

#' Extract minmax values of all participants
#'
#' @param data_df data frame
#'
#' @return named list of minmax values of each feature in the data frame
#'
#' @export
#'
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

#' Impute age_ship_s2 column
#' 
#' @param data_df data frame
#' 
#' @export
#'
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

#' Impute columns of data frame using mean value of the column for numeric features and mode for factors
#' 
#' @param colname feature name
#' @param data_df data frame
#'
#' @export
#' 
data_imputation <- function(colname, data_df){
  coldata<-as.vector(imputed_data_df[colname][[1]])
  imputed_data<-coldata
  if(is.factor(coldata) || is.character(coldata)){
    repval<-Mode(coldata, na.rm = TRUE)
    imputed_data <- as.factor(sapply(coldata, FUN = function(x) x = ifelse(is.na(x), repval, x)))
  }
  imputed_data_df[colname][[1]] <<- imputed_data
}

#' Impute columns of data frame using mean value of the column for numeric features and mode for factors
#' 
#' @param data_df data frame
#'
#' @return imputed data frame
#'
#' @export
#' 
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

#' Sample dataset using stratified k-fold cross validation
#' 
#' @param data_df data frame
#' @param kfold number of folds for cross validation
#' @param col_colname feature name
#'
#' @return list of fold indices for cross validation
#'
#' @export
#' 
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