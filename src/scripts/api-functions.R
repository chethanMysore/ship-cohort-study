cors <- function(req, res) {
  
  res$setHeader("Access-Control-Allow-Origin", "*")
  
  if (req$REQUEST_METHOD == "OPTIONS") {
    res$setHeader("Access-Control-Allow-Methods","*")
    res$setHeader("Access-Control-Allow-Headers", req$HTTP_ACCESS_CONTROL_REQUEST_HEADERS)
    res$status <- 200
    return(list())
  } else {
    plumber::forward()
  }
}

#* @serializer unboxedJSON
getFeatureImportance <- function(req,res) {
  feature_imp <- pre::importance(ship_study_results$model$finalModel)
  x <- feature_imp$varimps$imp
  y <- feature_imp$varimps$varname
  y_description <- c("Somatometric measurements of waist", 
                        "Glucose",
                        "Triglycerides during S2 wave",
                        "Patient's similarity with negative cohort patients",
                        "Patient's similarity with positive cohort patients",
                        "Time of lights off during sleep", 
                        "Serum Uric Acid",
                        "Characteristic changes in the patient",
                        "Representativeness of the patient in the cohort group",
                        "Somatometric measurements of Body Mass Index", 
                        "Diabetes Level",
                        "Triglycerides during S0 wave"
  )
  importance_data <- list(importance=x, features=y, featureDescription=y_description)
  list('response' = importance_data)
}

#* @serializer unboxedJSON
getIceCoords <- function(req, res){
tryCatch({
  feature_name <- fromJSON(req$postBody)[[1]]
  ice_coords <- get_ice_points(ship_study_results$model$finalModel$data, ship_study_results$model, feature_name = feature_name, frac_to_build = 0.27)
  list(status="SUCCESS", code="200", response=ice_coords)
}, error = function(e){
  list(status="ERROR", code="500", response=e)
})
}

#* @serializer unboxedJSON
getModelPerformance <- function(req, res){
  tryCatch({
    model_performance <- extract_model_performance(ship_study_results$model, ship_study_results$train_set, ship_study_results$validation_set)
    train_results <- model_performance$train_performance
    test_results <- model_performance$test_performance
    overall_train <- as.list(train_results$overall)
    byclass_train <- as.list(train_results$byClass)
    overall_test <- as.list(test_results$overall)
    byclass_test <- as.list(test_results$byClass)
    performance_results <- list(trainPerformance=data.frame("accuracy"=overall_train$Accuracy, 
                                                    "kappa"=overall_train$Kappa,
                                                    "positivePredictionValue"=byclass_train$`Pos Pred Value`,
                                                    "negativePredictionValue"=byclass_train$`Neg Pred Value`,
                                                    "sensitivity"=byclass_train$Sensitivity,
                                                    "specificity"=byclass_train$Specificity,
                                                    "precision"=byclass_train$Precision,
                                                    "f1"=byclass_train$F1
                                                    ),
                             testPerformance=data.frame("accuracy"=overall_test$Accuracy, 
                                                   "kappa"=overall_test$Kappa,
                                                   "positivePredictionValue"=byclass_test$`Pos Pred Value`,
                                                   "negativePredictionValue"=byclass_test$`Neg Pred Value`,
                                                   "sensitivity"=byclass_test$Sensitivity,
                                                   "specificity"=byclass_test$Specificity,
                                                   "precision"=byclass_test$Precision,
                                                   "f1"=byclass_test$F1
                             ))
    list(status="SUCCESS", code="200", response=performance_results)
  }, error = function(e){
    list(status="ERROR", code="500", response=e)
  })
}

#* @serializer unboxedJSON
getMinimalChange <- function(req, res){
  tryCatch({
    feature_imp <- importance(ship_study_results$model$finalModel)
    feat_imp <- feature_imp$varimps
    robustness <- get_minimal_change(ship_study_results$model, ship_study_results$validation_set, feat_imp$varname[1])
    list(status="SUCCESS", code="200", response=robustness)
  }, error = function(e){
    list(status="ERROR", code="500", response=e)
  })
}

