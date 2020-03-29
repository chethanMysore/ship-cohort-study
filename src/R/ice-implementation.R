## global variables
individual_ice_points <- list()

#' Build ICE co-ordinates for the plot
#'
#' @param individual_coords individual co-ordinates of a single participant
#' @param x_coords x - coordinate values
#' 
#' @export
#'
build_individual_ice_points <- function(individual_coords, x_coords){
  if(!is.null(individual_coords)){
    y_coords = as.vector(individual_coords)
    individual_ice = data.frame("x"=x_coords, "y"=y_coords)
    individual_ice_points <<- list.append(individual_ice_points, individual_ice)
  }
}

#' Get ICE points for plot
#' 
#' @param data data frame
#' @param model rulefit model
#' @param feature_name feature name
#' @param frac_to_build Optional specifies the fraction of participants for which ICE needs to be plotted
#' 
#' @return dictionary of ice points and pdp points that can be used to plot the respective curves
#' 
#' @export
#'
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
    apply(ship_ice_curves, 1, build_individual_ice_points, as.numeric(as.vector(colnames(ship_ice_curves))))
    pdp_points = data.frame("x"=as.numeric(rownames(pdp_points)), "y"=pdp_points$`ship_ice_plot$pdp`)
  }
  return(list(icePoints = individual_ice_points, pdpPoints = pdp_points, featureName=feature_name))
}