
## Install missing packages and import
dependencies_list <- c("evoxploit", "tidyverse", "checkmate", "rlist","hash", "hms", "ggplot2", "visdat","naniar","xlsx", "pre", "caret", "R6","shinydashboard", "DescTools", "groupdata2","devtools")
missing_packages_list <- dependencies_list[!(dependencies_list %in% installed.packages()[,"Package"])]
if(length(missing_packages_list)) install.packages(missing_packages_list)

library(evoxploit)
library(tidyverse)
library(checkmate)
library(rlist)
library(hash)
library(hms)
library(ggplot2)
library(visdat)
library(naniar)
library(pre)
library(caret)
library(R6)
library(xlsx)
library(DescTools)
library(shinydashboard)
library(groupdata2)
library(iml)
library(ICEbox)
library(devtools)
if(!c("plumber") %in% installed.packages()[,"Package"]){
  install_github("trestletech/plumber")
}
library(plumber)
library(jsonlite)
library(yaml)

source('./scripts/extract-features.R')
source('./scripts/factor-timestamp.R')
source('./scripts/data-with-labels.R')
source('./scripts/grouping-dataframes.R')
source('./scripts/rule-fit-implementation.R')
source('./scripts/ShipCohortStudy.R')
source('./scripts/data_imputation.R')
source('./scripts/data-sampling.R')
source('./scripts/ice-implementation.R')
source('./scripts/api-functions.R')
source('./scripts/extract-model-performance.R')
source('./scripts/robustness-computation.R')

  #' Launch App in Dev Mode
  #'
  #' @export
  # launchAppDev <- function(port=3000) {
  #   
  #   ##browser()
  #   funcs <- file.path("scripts", "api-functions.R")
  #   
  #   r <- plumb(funcs)
  #   ## launch rserve and listen for requests
  #   r$run(port=port)
  # }  
  
  launchAppDev <- function(port=3000){
    pr <- plumber$new()
    
    pr$filter("cors", function(req, res){
      cors(req,res)
      })
    
    pr$handle("GET", "/getFeatureImportance", function(req,res){
      getFeatureImportance(req,res)
      })
    
    pr$handle("GET", "/getModelPerformance", function(req,res){
      getModelPerformance(req,res)
    })
    
    pr$handle("POST", "/getIceCoords", function(req, res) {
      getIceCoords(req,res)
      })
    
    pr$handle("POST", "/getMinimalChange", function(req, res) {
      getMinimalChange(req,res)
    })
    
    pr$run(port=port, swagger=function(pr, spec, ...){
      spec <- yaml::read_yaml("./scripts/swagger.yaml")
      spec
    })
  }
  
  launchAppDev(3000)
                
  