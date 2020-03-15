# global variables
individual_ice_points <- list()

plot_ice <- function(model, dataset, feature_name){
  ## TO create an object of the type Predictor using the function "
  model = Predictor$new(model = model, dataset)
  
  ##ICE plot
  effect <- FeatureEffect$new(predictor = model, feature = feature_name, method = "ice")
  ice_plot <- plot(effect)
  return(ice_plot$data)
}

get_individual_ice_points <- function(individual_coords, x_coords){
  if(!is.null(individual_coords)){
    y_coords = as.vector(individual_coords)
    individual_ice = data.frame("x"=x_coords, "y"=y_coords)
    individual_ice_points <<- list.append(individual_ice_points, individual_ice)
  }
}

get_ice_points <- function(data, model, feature_name, frac_to_build = 0.2){
  individual_ice_points <<- list()
  pdp_points <- list()
  if(!is.null(data) && !is.null(model) && !is.null(feature_name) && !is.null(frac_to_build)){
    ship_train_data = data
    ship_ice_plot <- ice(model, X = ship_train_data, predictor=feature_name, frac_to_build = frac_to_build, predictfcn = function(object, newdata){
      predict(object, newdata, type = "prob")[,2]
    })
    ship_ice_curves = as.data.frame(ship_ice_plot$ice_curves)
    pdp_points <- as.data.frame(ship_ice_plot$pdp)
    apply(ship_ice_curves, 1, get_individual_ice_points, as.numeric(as.vector(colnames(ship_ice_curves))))
    pdp_points = data.frame("x"=as.numeric(rownames(pdp_points)), "y"=pdp_points$`ship_ice_plot$pdp`)
  }
  return(list(ice_points = individual_ice_points, pdp_points = pdp_points))
}

