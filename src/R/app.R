#!/usr/bin/env Rscript
## Import dependencies
source('./src/R/api.R')

## command line arguments
args = commandArgs(trailingOnly = TRUE)

if(length(args) == 0){
  stop("Please specify input dataset file path", call.=FALSE)
}

## Import dataset into ship_dataset
tryCatch({
    sample_df <- readRDS(args[1])
  }, error = function(e){
    stop("Cannot open file")  
  }
)

## Build Rulefit Model
ship_study_results <- ShipCohortStudy$new(data_df = sample_df)
ship_study_results$summary()

## Launch the Ship_Study_Api
launchAppDev(port = 3000, ship_study_results)