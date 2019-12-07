

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
  
  
  
  
  
  
  

  
