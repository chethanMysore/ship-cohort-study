rule_fit <- function(data_df, class_labels, train_index = NULL, cv_folds = 5){
  rulefit_result <- NULL;
  # set sampling method to cross validation with 5-folds
  train.control <- NULL
  if(is.null(train_index)){
    train.control <- trainControl(method = "cv", number = cv_folds)
  }
  else{
    train.control <- trainControl(index = train_index, method = "repeatedcv", number = cv_folds, repeats = 1)
  }
  set.seed(42)
  # build rule fit model
  rulefit_result <- train(x = data_df, y = class_labels, method = caret_pre_model, trControl = train.control, ntrees = 25L)
  rulefit_result.data <- data_df
  importance(rulefit_result$finalModel)
  return(rulefit_result)
}