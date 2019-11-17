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
sample_df_with_labels <- data_with_labels(sample_df)

# extract features in all waves
sample_df <- extract_features_with_suffix(sample_df_with_labels, suffix)

str(sample_df)
# factor timestamp column for exdate_ship
col_name = "exdate_ship"
col_name_1 = "blt_beg"
sample_df <- factor_timestamp(sample_df, col_name)
sample_df <- factor_hms (sample_df, col_name_1)

