PreProcessing <- R6::R6Class("PreProcessing", private = list(
    ..data_df = NULL,
    ..raw_data = NULL,
    ..labels = NULL,
    ..evo_result = NULL,
    ..group_by_male = NULL,
    ..group_by_female = NULL,
    ..wave_suffix = "(_s0|_s1|_s2)",
    ..missingness_threshold = 0.06,
    ..minmax_vals = NULL,
    ..obs_after_ageship = NULL,
    ..obs_after_missing = NULL
  ),
  public = list(
    initialize = function(data_df){
      checkmate::assert_data_frame(data_df)
      private$..raw_data <- data_df
      private$..data_df <- data_df
      
      ##Remove rows with non-missing values for age_ship_s2
      cat("Removing rows with non-missing values for age_ship_s2...\n")
      private$..obs_after_ageship <- nrow(private$..data_df)
      private$..data_df <- private$..data_df[!is.na(private$..data_df$age_ship_s2), ]
      private$..obs_after_ageship <- (private$..obs_after_ageship - nrow(private$..data_df))

      ## Removing data without labels
      cat("Removing data without labels...\n")
      private$..data_df <- data_with_labels(private$..data_df)
      
      ## Extraction of labels
      cat("Extracting labels...\n")
      private$..labels <- only_labels(data_df = private$..data_df)
      private$..data_df$liver_fat <- private$..labels
      
      ## Extract features present in all 3 waves
      cat("Extracting features present in all 3 waves...\n")
      private$..data_df <- extract_features_with_suffix(private$..data_df, private$..wave_suffix)
      
      ## Factor Features
      cat("Factoring Features...\n")
      private$..data_df <- factor_timestamp(private$..data_df, "exdate_ship") # TODO: add checks for column availability
      private$..data_df <- factor_hms(private$..data_df, "blt_beg") # TODO: add checks for column availability
      
      ##Remove columns having 5% or more than 5% of missing values(NA)
      cat("Removing columns having 5% or more than 5% of missing values(NA)...\n")
      private$..data_df <- private$..data_df[, -which(colMeans(is.na(private$..data_df)) > private$..missingness_threshold)]
      
      ## Extracting evolution features
      cat("Extracting evolution features...\n")
      private$..evo_result = Evoxploit$new(private$..data_df, private$..labels[[1]], wave_suffix = "_s")
      private$..data_df <- private$..evo_result$all_features
      
      ## Extracting evolution_features for all waves
      ## After this step, private$..data_df has all original and evo_features available in all waves
      cat("Extracting evolution_features for all waves...\n")
      private$..data_df <- extract_features_with_suffix(private$..data_df, private$..wave_suffix)
      
      ## Remove gender specific features
      cat("Removing gender specific features...\n")
      private$..group_by_male = subset(private$..data_df, female_s0==0)
      private$..group_by_female = subset(private$..data_df, female_s0==1)
      cols_to_remove = NULL
      cols_to_remove = gender_group_compare (private$..group_by_male, private$..group_by_female)
      cols_to_remove = list.append(cols_to_remove, "female_s0") # TODO: add checks for column availability
      private$..data_df <- private$..data_df[,!names(private$..data_df) %in% cols_to_remove]
      
      ##Remove columns having 5% or more than 5% of missing values(NA)
      cat("Removing columns having 5% or more than 5% of missing values(NA)...\n")
      private$..obs_after_missing <- nrow(private$..data_df)
      private$..data_df <- private$..data_df[, -which(colMeans(is.na(private$..data_df)) > private$..missingness_threshold)]
      private$..obs_after_missing <- (private$..obs_after_missing - nrow(private$..data_df))
      
      ## Impute Data
      cat("Imputing Data...\n")
      private$..data_df <- impute_dataset(private$..data_df)
      
      ## Get minmax values of each feature
      cat("Extracting minmax values of each feature...\n")
      private$..minmax_vals <- extract_min_max_values(private$..data_df)
      
      ## Scaling features for all waves
      cat("Scaling features for all waves...\n")
      private$..data_df <- private$..data_df%>%
        mutate_at(vars(names(private$..data_df)[which(sapply(private$..data_df, is.numeric))])
                  ,(function(x) return((x - min(x)) / (max(x) - min(x)))))
      
      ## Add labels back to reformed dataset
      cat("Adding labels back to reformed dataset...\n")
      private$..data_df$liver_fat <- private$..labels[[1]]
    },
    summary = function(){
      cat("\n---------------------Data Preprocesing Summary-----------------------\n")
      cat(nrow(private$..raw_data), " instances were observed.\n")
      cat("After removing records with missing values in 'age_ship_s2', ", private$..obs_after_ageship, " instances were observed.\n")
      cat("Total number of evolutionary features extracted: ", ncol(private$..evo_result$evo_features), ".\n")
      cat("After removing records with missingness threshold > ", (private$..missingness_threshold * 100), "%, ", private$..obs_after_missing, " instances were observed.\n")
      cat("Final dataset has ", nrow(private$..data_df), " observations and ", ncol(private$..data_df), " features.\n")
    }
  ),
  active = list(
    data_df = function(value){
      if(missing(value)){
        private$..data_df
      }
      else{
        stop("$..data_df is read only", call. = FALSE)
      }
    },
    labels = function(value){
      if(missing(value)){
        private$..labels
      }
      else{
        stop("$..labels is read only", call. = FALSE)
      }
    },
    evo_result = function(value){
      if(missing(value)){
        private$..evo_result
      }
      else{
        stop("$..evo_result is read only", call. = FALSE)
      }
    },
    group_by_male = function(value){
      if(missing(value)){
        private$..group_by_male
      }
      else{
        stop("$..group_by_male is read only", call. = FALSE)
      }
    },
    group_by_female = function(value){
      if(missing(value)){
        private$..group_by_female
      }
      else{
        stop("$..group_by_female is read only", call. = FALSE)
      }
    },
    missingness_threshold = function(value){
      if(missing(value)){
        private$..missingness_threshold
      }
      else{
        stop("$..missingness_threshold is read only", call. = FALSE)
      }
    },
    minmax_vals = function(value){
      if(missing(value)){
        private$..minmax_vals
      }
      else{
        stop("$..minmax_vals is read only", call. = FALSE)
      }
    }
  )
)
