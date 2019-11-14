#imports
library(tidyverse)
library(rlist)
library(hash)

#global variables
feature_list <- list()
cols_to_remove <- list()
mutated_cols_to_remove <- list()


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
  print(feat_name)
  feat_count <- feature_list[[feat_name]]
  if(feat_count != 3){
    print(grep(str_c("^", feat_name, suffix, "$"), "age_ship_s0"))
    #sample_ship_df <<- sample_ship_df[, -grep(str_c("^",feat_name,"(_s0|_s1|_s2)$"), col_names)]
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
extract_features_with_suffix <- function(data_df, suffix){
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