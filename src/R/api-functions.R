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
getFeatureImportance <- function(req,res, ship_study_results) {
  importance_data = ship_study_results$get_feature_importance()
  list('response' = importance_data)
}

#* @serializer unboxedJSON
getIceCoords <- function(req, res, ship_study_results){
tryCatch({
  feature_name <- fromJSON(req$postBody)[[1]]
  ice_coords = ship_study_results$get_ice_coords(feature_name)
  list(status="SUCCESS", code="200", response=ice_coords)
}, error = function(e){
  list(status="ERROR", code="500", response=e)
})
}

#* @serializer unboxedJSON
getModelPerformance <- function(req, res, ship_study_results){
  tryCatch({
    performance_results = ship_study_results$get_model_performance()
    list(status="SUCCESS", code="200", response=performance_results)
  }, error = function(e){
    list(status="ERROR", code="500", response=e)
  })
}

#* @serializer unboxedJSON
getMinimalChange <- function(req, res, ship_study_results){
  tryCatch({
    participant_id <- fromJSON(req$postBody)[[1]]
    participant_changes = ship_study_results$get_minimal_change(participant_id)
    list(status="SUCCESS", code="200", response=participant_changes)
  }, error = function(e){
    list(status="ERROR", code="500", response=e)
  })
}

