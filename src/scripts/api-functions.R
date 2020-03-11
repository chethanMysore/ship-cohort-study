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
  feature_imp <- importance(ship_study_results$model$finalModel)
  x <- feature_imp$varimps$imp
  y <- feature_imp$varimps$varname
  y_description <- list("Somatometric measurements of waist", 
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
  importance_data <- list(x,y,y_description)
  list('response' = importance_data)
}

#* @serializer unboxedJSON
getIceCoords <- function(req, res){
tryCatch({
  feature_name <- fromJSON(req$postBody)[[1]]
  ice_coords <- plot_ice(ship_study_results$model$finalModel, ship_study_results$model$finalModel$data, feature_name)
  ice_coords <- data.frame("x"=ice_coords[[1]], "y"=ice_coords$.y.hat)
  list(status="SUCCESS", code="200", response=ice_coords)
}, error = function(e){
  list(status="ERROR", code="500", response=e)
})
}

