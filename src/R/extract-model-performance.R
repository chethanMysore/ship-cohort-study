#' Extract Model Performance
#'
#' @param model rulefit model
#' @param train_set training set used during model building
#' @param validation_set validation set
#' 
#' @return dictionary of training performance and test performance of the model
#' 
#' @export
#'
extract_model_performance <- function(model, train_set, validation_set){
  ## Validate Model Prediction
  actual_labels <- validation_set$liver_fat
  validation_set <- validation_set[, !names(validation_set) %in% c("liver_fat")]
  model_predictions <- predict(model, validation_set)
  cmp_table <- table(factor(model_predictions, levels = levels(model_predictions)),
                     factor(actual_labels, levels = levels(actual_labels)))
  test_performance <- confusionMatrix(cmp_table)
  ## Model Prediction on training set
  actual_labels <- train_set$liver_fat
  train_set <- train_set[, !names(train_set) %in% c("liver_fat")]
  model_predictions <- predict(model, train_set)
  cmp_table <- table(factor(model_predictions, levels = levels(model_predictions)),
                     factor(actual_labels, levels = levels(actual_labels)))
  train_performance <- confusionMatrix(cmp_table)
  
  return(list(train_performance=train_performance, test_performance=test_performance))
}