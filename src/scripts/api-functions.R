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
  feature_imp <- importance(ship_study_results$model$finalModel)
  x <- feature_imp$varimps$imp
  y <- feature_imp$varimps$varname
  y_description <- list("Somatometric measurements of waist", 
                        "Patient's similarity with positive cohort patients",
                        "Somatometric measurements of Body Mass Index", 
                        "Serum Uric Acid",
                        "Time of lights off during sleep", 
                        "Triglycerides during S2 wave",
                        "Triglycerides during S1 wave",
                        "Characteristic changes in the patient",
                        "Glucose", 
                        "Triglycerides during S0 wave",
                        "Diabetes")
  importance_data <- list(x,y,y_description)
  list('response' = importance_data)
}
