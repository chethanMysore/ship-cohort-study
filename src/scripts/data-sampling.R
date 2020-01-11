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