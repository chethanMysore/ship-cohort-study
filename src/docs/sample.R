source('./R/api.R')

## Import dataset into ship_dataset
sample_df <- ship_dataset

## Build Rulefit Model
ship_study_results <- ShipCohortStudy$new(data_df = sample_df)
ship_study_results$summary()

## Launch the Ship_Study_Api
launchAppDev(port = 3000, ship_study_results)
