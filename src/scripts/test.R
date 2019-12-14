#install missing packages and import
package_list <- c("evoxploit", "tidyverse", "checkmate", "rlist","hash", "hms", "ggplot2", "visdat","naniar","xlsx")
package_list <- package_list[!(package_list %in% installed.packages()[,"Package"])]
if(length(package_list)) install.packages(package_list)

library(evoxploit)
library(tidyverse)
library(checkmate)
library(rlist)
library(hash)
library(hms)
library(ggplot2)
library(visdat)
library(naniar)
library("xlsx")
source('./scripts/extract-features.R')
source('./scripts/factor-timestamp.R')
source('./scripts/data-with-labels.R')

# wave suffix
suffix <- "(_s0|_s1|_s2)"

# take sample of ship_data dataset
sample_df <- ship_dataset

#removing data without labels
sample_df <- data_with_labels(sample_df)

#Extraction of labels
sample_df_label_column <- only_labels(sample_df)

# extract features in all waves
sample_df <- extract_features_with_suffix(sample_df, suffix)

# Factor Features
sample_df <- factor_timestamp(sample_df, "exdate_ship")
sample_df <- factor_hms (sample_df, "blt_beg")

#Plot Missing Values for wave s0 
wave_s0_df <- select(sample_df, ends_with("_s0"))
gg_miss_var(wave_s0_df, show_pct = TRUE)  #shows percentage of missing values in the column 
gg_miss_var(wave_s0_df, show_pct = FALSE)  #shows number of missing values in the column
vis_miss(sample_df)  #visualize missing values

#Extracting evolution features
evo_features <- Evoxploit$new(sample_df, sample_df_label_column[[1]], wave_suffix = "_s")
evo_all_features <- evo_features$all_features

#extracting evolution_features for all waves
sample_df_for_evo <- extract_features_with_suffix(evo_all_features, suffix)

#Grouping by gender
group_by_male <- subset(sample_df_for_evo, female_s0==0)
group_by_female <- subset(sample_df_for_evo, female_s0==1)
sample_df_for_evo_compare <- gender_group_compare (group_by_male, group_by_female)

#appending output 
sample_df_for_evo_withcL <- cbind(sample_df_for_evo,sample_df_label_column)

#factoring liver_fat column
sample_df_for_evo_withcL_factored <- factor_liver_fat (sample_df_for_evo_withcL, "liver_fat")  

#scaling evolution_features for all waves
#stand_sample_df_for_evo <- sample_df_for_evo%>%
  #mutate_at(vars(names(sample_df_for_evo)[which(sapply(sample_df_for_evo, is.numeric))])                                         
         #   ,(function(x) return((x - min(x)) / (max(x) - min(x)))))
#str(sample_df_for_evo$smoking_s0)

##Exporting to Excel
#output_file_path <- getwd()
#output_file_name <- "sample_df_report.xlsx"
#dataframe_to_write <- sample_df
#write.xlsx(dataframe_to_write, str_c(output_file_path, "/visualization/", output_file_name))
