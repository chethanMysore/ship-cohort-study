#* @filter cors
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

test.data <- list(
  "cat",
  "dog",
  1,
  2,
  "butterfly"
)
# 
# #* @get /getFeatureImportance
# #* @serializer unboxedJSON
# getFeatureImportance <- function() {
#   imp <- print(ship_study_results$model$finalModel)
#   x <- imp$description
#   y <- imp$coefficient
#   importance_data <- list(x,y)
#   list('response' = importance_data)
# }


#* @get /getFeatureImportance
#* @serializer unboxedJSON
getFeatureImportance <- function() {
  feature_imp <- print(importance(ship_study_results$model$finalModel))
  x <- feature_imp$varimps$imp
  y <- feature_imp$varimps$varname
  y_description <- list("Somatometric measurements of waist", "Time of lights off during sleep", "Glucose", "Evolution feature", "Triglycerides during S2 wave", "Triglycerides during S2 wave", "Somatometric measurements of Body Mass Index", "Triglycerides during S1 wave", "Diabetes", "Serum Uric Acid", "Evolution feature")
  importance_data <- list(x,y,y_description)
  list('response' = importance_data)
}

