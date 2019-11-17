#imports
library(evoxploit)
library(tidyverse)
library(checkmate)
library(rlist)
library(hash)
library(hms)

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

# factor timestamp column for exdate_ship
col_name = "exdate_ship"
col_name_1 = "blt_beg"
sample_df <- factor_timestamp(sample_df, col_name)
sample_df <- factor_hms (sample_df, col_name_1)

#Extracting evolution features
evo_features <- Evoxploit$new(sample_df, sample_df_label_column[[1]], wave_suffix = "_s")

summary(evo_features)
evo_all_features <- evo_features$all_features

#extracting evolution_features for all waves
sample_df_for_evo <- extract_features_with_suffix(evo_all_features, suffix)
