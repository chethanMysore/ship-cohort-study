plot_ice <- function(model, dataset, feature_name){
  ## TO create an object of the type Predictor using the function "
  model = Predictor$new(model = model, dataset)
  
  ##ICE plot
  effect <- FeatureEffect$new(predictor = model, feature = feature_name, method = "ice")
  ice_plot <- plot(effect)
  return(ice_plot$data)
}