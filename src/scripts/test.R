#imports
library(evoxploit)
library(tidyverse)
library(checkmate)
library(rlist)
library(hash)
library(hms)
library(ggplot2)
library(visdat)
library(naniar)
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
col_name_2="liver_fat"
sample_df <- factor_timestamp(sample_df, col_name)
sample_df <- factor_hms (sample_df, col_name_1)

summary(sample_df)
gg_miss_var(wave_s0,show_pct= TRUE)


wave_s0 <- select(sample_df, ends_with("_s0"))
wave_s_s0 <- select(wave_s0, ends_with("_s_s0"))

gg_miss_var(wave_s0, show_pct = FALSE)

help(gg_miss_var)
vis_miss(sample_df)

#Extracting evolution features
evo_features <- Evoxploit$new(sample_df, sample_df_label_column[[1]], wave_suffix = "_s")

summary(evo_features)
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
sample_df_for_evo_withcL_factored <- factor_liver_fat (sample_df_for_evo_withcL, col_name_2)  







#scaling evolution_features for all waves
#stand_sample_df_for_evo <- sample_df_for_evo%>%
  #mutate_at(vars(names(sample_df_for_evo)[which(sapply(sample_df_for_evo, is.numeric))])                                         
         #   ,(function(x) return((x - min(x)) / (max(x) - min(x)))))

#str(sample_df_for_evo$smoking_s0)

               

#exportingexcel
install.packages("xlsx")
library("xlsx")
write.xlsx(dat,"C:/Users/Mohit/Desktop/DE_WiSe2019/Project DM and VA/rawdata.xlsx")
