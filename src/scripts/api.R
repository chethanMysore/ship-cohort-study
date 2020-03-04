library(plumber)

source('./scripts/api-functions.R')

#' Launch App in Dev Mode
#'
#' @export
launchAppDev <- function(port=3000) {
  
  ##browser()
  funcs <- file.path("scripts", "api-functions.R")
  
  r <- plumb(funcs)
  ## launch rserve and listen for requests
  r$run(port=port)
}

launchAppDev(3000)


