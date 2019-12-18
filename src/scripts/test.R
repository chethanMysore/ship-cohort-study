## Install missing packages and import
dependencies_list <- c("evoxploit", "tidyverse", "checkmate", "rlist","hash", "hms", "ggplot2", "visdat","naniar","xlsx", "pre", "caret", "R6")
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
source('./scripts/extract-features.R')
source('./scripts/factor-timestamp.R')
source('./scripts/data-with-labels.R')
source('./scripts/grouping-dataframes.R')
source('./scripts/rule-fit-implementation.R')
source('./scripts/ShipCohortStudy.R')

## Take sample of ship_data dataset
sample_df <- ship_dataset

## Removing data without labels
sample_df <- data_with_labels(sample_df)

## Extraction of labels
sample_df_label_column <- only_labels(data_df = sample_df)

## Plot Missing Values for wave s0
wave_s0_df <- select(sample_df, ends_with("_s0"))
gg_miss_var(wave_s0_df, show_pct = TRUE)  #shows percentage of missing values in the column
gg_miss_var(wave_s0_df, show_pct = FALSE)  #shows number of missing values in the column
vis_miss(sample_df)  #visualize missing values

## Build Rule Fit Model
ship_study_results <- ShipCohortStudy$new(data_df = sample_df, labels = sample_df_label_column[[1]])
ship_study_results$summary()

## Validate Model Prediction
validation_set <- ship_study_results$validation_set
actual_labels <- validation_set$liver_fat
validation_set <- validation_set[, !names(validation_set) %in% c("liver_fat")]
model_predictions <- predict(ship_study_results$model, validation_set[1:50,])
actual_labels <- actual_labels[1:50]
cmp_table <- table(factor(model_predictions, levels = levels(model_predictions)),
                   factor(actual_labels, levels = levels(actual_labels)))
confusionMatrix(cmp_table)

## Scaling evolution_features for all waves
#stand_sample_df_for_evo <- sample_df_for_evo%>%
  #mutate_at(vars(names(sample_df_for_evo)[which(sapply(sample_df_for_evo, is.numeric))])                                         
         #   ,(function(x) return((x - min(x)) / (max(x) - min(x)))))
#str(sample_df_for_evo$smoking_s0)

## Exporting to Excel
#output_file_path <- getwd()
#output_file_name <- "sample_df_report.xlsx"
#dataframe_to_write <- sample_df
#write.xlsx(dataframe_to_write, str_c(output_file_path, "/visualization/", output_file_name))
