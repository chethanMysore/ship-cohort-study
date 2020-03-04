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

#* @get /getFeatureImportance
#* @serializer unboxedJSON
getFeatureImportance <- function() {
  imp <- print(ship_study_results$model$finalModel)
  x <- imp$description
  y <- imp$coefficient
  importance_data <- list(x,y)
  list('response' = importance_data)
}