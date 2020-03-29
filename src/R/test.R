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

source('./R/rule-fit-implementation.R')
source('./R/ShipCohortStudy.R')
source('./R/ice-implementation.R')
source('./R/extract-model-performance.R')
source('./R/api-functions.R')
source('./R/PreProcessing.R')
source('./R/pre-processing-functions.R')
source('./R/robustness-computation.R')

## Take sample of ship_data dataset
sample_df <- ship_dataset

preprocessing_result <- PreProcessing$new(sample_df)

##Remove rows with non-missing values for age_ship_s2
sample_df <- sample_df[!is.na(sample_df$age_ship_s2), ]

#impute_mean(as.vector(sample_df$age_ship_s1))
## Removing data without labels
sample_df <- data_with_labels(sample_df)

## Extraction of labels
sample_labels <- only_labels(data_df = sample_df)
sample_df$liver_fat <- sample_labels


##Plot missingness graphs
zero_t0_6_missing <- sample_df[, which(colMeans(is.na(sample_df)) >= 0 & colMeans(is.na(sample_df)) < 0.06)]
six_to_10_missing <- sample_df[, which(colMeans(is.na(sample_df)) >= 0.06 & colMeans(is.na(sample_df)) < 0.10)]
ten_to_20_missing <- sample_df[, which(colMeans(is.na(sample_df)) >= 0.10 & colMeans(is.na(sample_df)) < 0.20)]
greater_than_20_missing <- sample_df[, which(colMeans(is.na(sample_df1)) >= 0.20)]

wave_s0_df <- zero_t0_6_missing
gg_miss_var(wave_s0_df, show_pct = TRUE)  #shows percentage of missing values in the column

wave_s0_df <- six_to_10_missing
gg_miss_var(wave_s0_df, show_pct = TRUE)  #shows percentage of missing values in the column

wave_s0_df <- ten_to_20_missing
gg_miss_var(wave_s0_df, show_pct = TRUE)  #shows percentage of missing values in the column

wave_s0_df <- greater_than_20_missing
gg_miss_var(wave_s0_df, show_pct = TRUE)  #shows percentage of missing values in the column


## Extract features present in all 3 waves
sample_df <- extract_features_with_suffix(sample_df, "(_s0|_s1|_s2)")

## Factor Features
sample_df <- factor_timestamp(sample_df, "exdate_ship")
sample_df <- factor_hms(sample_df, "blt_beg")

##Remove columns having 5% or more than 5% of missing values(NA)
sample_df <- sample_df[, -which(colMeans(is.na(sample_df)) > 0.06)]

## Extracting evolution features
evo_extraction_result <- Evoxploit$new(sample_df, sample_labels[[1]], wave_suffix = "_s")
sample_df <- evo_extraction_result$all_features

## Extracting evolution_features for all waves
## After this step, private$..data_df has all original and evo_features available in all waves
sample_df <- extract_features_with_suffix(sample_df, "(_s0|_s1|_s2)")

## Remove gender specific features
group_by_male <- subset(sample_df, female_s0==0)
group_by_female <- subset(sample_df, female_s0==1)
cols_to_remove <- gender_group_compare (group_by_male, group_by_female)
cols_to_remove <- list.append(cols_to_remove, "female_s0")
sample_df <- sample_df[,!names(sample_df) %in% cols_to_remove]

##Remove columns having 5% or more than 5% of missing values(NA)
sample_df <- sample_df[, -which(colMeans(is.na(sample_df)) > 0.06)]

## Impute Data
sample_df <- impute_dataset(sample_df)

## Get minmax values of each feature
minmax_vals <- extract_min_max_values(sample_df)

## Scaling features for all waves
sample_df <- sample_df%>%
  mutate_at(vars(names(sample_df)[which(sapply(sample_df, is.numeric))])
            ,(function(x) return((x - min(x)) / (max(x) - min(x)))))

sample_df$liver_fat <- sample_labels[[1]]


## Build Rule Fit Model
ship_study_results <- ShipCohortStudy$new(data_df = sample_df, labels = sample_df$liver_fat, cv_folds = 5)
ship_study_results$summary()

## Validate Model Prediction
validation_set <- ship_study_results$validation_set
actual_labels <- validation_set$liver_fat
validation_set <- validation_set[, !names(validation_set) %in% c("liver_fat")]
model_predictions <- predict(ship_study_results$model, validation_set)
cmp_table <- table(factor(model_predictions, levels = levels(model_predictions)),
                   factor(actual_labels, levels = levels(actual_labels)))
confusionMatrix(cmp_table)

## Variable importance
# Coefficients for final linear regression model
feature_imp <- importance(ship_study_results$model$finalModel)

## Dataframe for model results
rules_coeff <- select(feature_imp$baseimps, c("rule", "description", "coefficient"))
check <- print(ship_study_results$model$finalModel)

##feature imp plot
ggplot(data = check, aes(x = check$description, y = check$coefficient, fill = check$coefficient > 0)) + 
  geom_bar(stat = "identity") +
  scale_fill_manual(name = "Coefficients > 0", labels = c("Negative Values", "Positive Values"), values = c("FALSE"="#d43943", "TRUE"="#29ab9c")) + 
  labs(x= "Features", y="Importance") + 
  coord_flip()

## Plot Missing Values for wave s0
#wave_s0_df <- select(sample_df, ends_with("_s2"))
gg_miss_var(wave_s0_df, show_pct = TRUE)  #shows percentage of missing values in the column
gg_miss_var(wave_s0_df, show_pct = FALSE)  #shows number of missing values in the column
vis_miss(sample_df)  #visualize missing values

## Exporting to Excel
#output_file_path <- getwd()
#output_file_name <- "sample_df_report.xlsx"
#dataframe_to_write <- sample_df
#write.xlsx(dataframe_to_write, str_c(output_file_path, "/visualization/", output_file_name))
