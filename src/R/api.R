## Install missing packages and import
dependencies_list <- c("evoxploit", "tidyverse", "checkmate", "rlist","hash", "hms", "ggplot2", "visdat","naniar","xlsx", "pre", "caret", "R6", "DescTools", "groupdata2","devtools", "iml", "ICEbox", "jsonlite", "yaml")
missing_packages_list <- dependencies_list[!(dependencies_list %in% installed.packages()[,"Package"])]
if(length(missing_packages_list)) install.packages(missing_packages_list)

## Import required libraries
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

## Source required classes and functions
source('./R/pre-processing-functions.R')
source('./R/PreProcessing.R')
source('./R/rule-fit-implementation.R')
source('./R/ice-implementation.R')
source('./R/extract-model-performance.R')
source('./R/robustness-computation.R')
source('./R/ShipCohortStudy.R')
source('./R/api-functions.R')


  #' Launch App in Dev Mode
  #'
  #' @export
  launchAppDev <- function(port=3000, ship_study_results){
    pr <- plumber$new()
    
    pr$filter("cors", function(req, res){
      cors(req,res)
      })
    
    pr$handle("GET", "/getFeatureImportance", function(req,res){
      getFeatureImportance(req,res, ship_study_results)
      })
    
    pr$handle("GET", "/getModelPerformance", function(req,res){
      getModelPerformance(req,res, ship_study_results)
    })
    
    pr$handle("POST", "/getIceCoords", function(req, res) {
      getIceCoords(req,res, ship_study_results)
      })
    
    pr$handle("POST", "/getMinimalChange", function(req, res) {
      getMinimalChange(req,res, ship_study_results)
    })
    
    pr$run(port=port, swagger=function(pr, spec, ...){
      spec <- yaml::read_yaml("./R/swagger.yaml")
      spec
    })
  }