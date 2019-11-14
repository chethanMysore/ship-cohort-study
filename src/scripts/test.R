#imports
library(evoxploit)
library(tidyverse)
library(checkmate)
library(rlist)
library(hash)

source('~/Documents/DM-Project/ship-cohort-study/src/scripts/extract-features.R')
source('~/Documents/DM-Project/ship-cohort-study/src/scripts/factor-timestamp.R')

# wave suffix
suffix <- "(_s0|_s1|_s2)"

# take sample of ship_data dataset
sample_df <- ship_dataset

# extract features in all waves
sample_df <- extract_features_with_suffix(sample_df, suffix)

# factor timestamp column for exdate_ship
col_name = "exdate_ship"
sample_df <- factor_timestamp(sample_df, col_name)

