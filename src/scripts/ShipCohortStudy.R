ShipCohortStudy <- R6::R6Class("ShipCohortStudy", private = list(
  ..wave_suffix = "(_s0|_s1|_s2)",
  ..data_df = NULL,
  ..labels = NULL,
  ..evo_extraction_result = NULL,
  ..evo_features = NULL,
  ..data_df_with_evo = NULL,
  ..rule_fit_model = NULL,
  ..model_accuracy = NULL,
  ..model_kappa = NULL,
  ..model_learnrate = NULL,
  ..best_tuning_params = NULL,
  ..train_set = NULL,
  ..validation_set = NULL,
  ..cv_folds = NULL,
  ..model_validation_result = NULL,
  ..importance_plotname = "feature-importance.png",
  ..vis_dir = "../visualization"
  ),
  public = list(
    initialize = function(data_df, labels, cv_folds = 5){
      #checkmate::assert(data_df)
      #checkmate::assert(labels)
      checkmate::assert_data_frame(data_df)
      checkmate::assert_true(nrow(data_df) == length(labels))
      checkmate::assert_false(any(is.na(labels)))
      
      private$..data_df <- data_df
      private$..labels <- labels
      private$..cv_folds <- cv_folds
      
      if(all(purrr::map_lgl(labels, ~ checkmate::test_numeric(.x)))){  # if the labels provided is a numeric array then convert it to factor
        labels <- only_labels(labels = labels)
        labels <- labels$liver_fat
      }
      
      ## Extract features present in all 3 waves
      data_df <- extract_features_with_suffix(data_df, private$..wave_suffix)
      
      col_names <- names(data_df)
      ## Factor Features
      if(length(grep("exdate_ship", col_names, value = TRUE)) > 0){
        data_df <- factor_timestamp(data_df, "exdate_ship")
      }
      if(length(grep("blt_beg", col_names, value = TRUE)) > 0){
        data_df <- factor_hms(data_df, "blt_beg")
      }
      
      ## Extracting evolution features
      private$..evo_extraction_result <- Evoxploit$new(data_df, labels, wave_suffix = "_s")
      private$..evo_features <- private$..evo_extraction_result$evo_features
      
      ## Extracting evolution_features for all waves
      ## After this step, private$..data_df has all original and evo_features available in all waves
      private$..data_df_with_evo <- extract_features_with_suffix(private$..evo_extraction_result$all_features, private$..wave_suffix)
      
      ## Remove gender specific features
      if(length(grep("female_s0", names(private$..data_df_with_evo), value = TRUE)) > 0){
        group_by_male <- subset(private$..data_df_with_evo, female_s0==0)
        group_by_female <- subset(private$..data_df_with_evo, female_s0==1)
        cols_to_remove <- gender_group_compare (group_by_male, group_by_female)
        cols_to_remove <- list.append(cols_to_remove, "female_s0")
        private$..data_df_with_evo <- private$..data_df_with_evo[,!names(private$..data_df_with_evo) %in% cols_to_remove]
      }
      
      ## Build Rule Fit Model
      ## Import final_evo_dataset_imputed csv into evo_dataset_imputed variable. ToDo: Code this 
      evo_imputed_sample <- evo_dataset_imputed %>%
        mutate(liver_fat = factor(liver_fat, levels = c(1, 0), labels = c(1, 0)))
      private$..data_df_with_evo <- evo_imputed_sample
      private$..data_df_with_evo <- private$..data_df_with_evo[, !names(private$..data_df_with_evo) %in% c("female_s0")]
      
      ## Sample data into train and validation sets
      set.seed(42)
      train <- sample(1:nrow(evo_imputed_sample), 2 * nrow(evo_imputed_sample)/3)
      test <- (-train)
      private$..train_set <- evo_imputed_sample[train,]
      private$..validation_set <- evo_imputed_sample[test,]
      sample_class_labels <- private$..train_set$liver_fat
      train_set <- private$..train_set[, !names(private$..train_set) %in% c("liver_fat")]
      
      private$..rule_fit_model <- rule_fit(train_set, sample_class_labels, private$..cv_folds)
      
      ## Store Results
      if(!is.null(private$..rule_fit_model)){
        private$..model_accuracy <- mean(as.vector(private$..rule_fit_model$results$Accuracy))
        private$..model_kappa <- mean(as.vector(private$..rule_fit_model$results$Kappa))
        private$..best_tuning_params <- private$..rule_fit_model$bestTune
        private$..best_tuning_params$cv_folds <- private$..cv_folds
      }  
    },
    summary = function(){
      cat("\n---------------------Summary-----------------------\n")
      cat(nrow(private$..data_df), " instances were observed.\n")
      if(is.null(private$..rule_fit_model)){
        cat("Model build failed. Results are unavailable")
      }
      else{
        cat("Average accuracy of the model: ", private$..model_accuracy, "\n")
        cat("Average interrater reliability (kappa): ", private$..model_kappa, "\n")
        cat("Best tuning parameters observed: \n")
        if(is.null(private$..best_tuning_params)){
          cat("NULL\n")
        }
        else{
          cat("\tSampfrac: ", private$..best_tuning_params$sampfrac, "\n")
          cat("\tmaxdepth: ", private$..best_tuning_params$maxdepth, "\n")
          cat("\tlearnrate: ", private$..best_tuning_params$learnrate, "\n")
          cat("\tmtry: ", private$..best_tuning_params$mtry, "\n")
          cat("\tuse.grad: ", private$..best_tuning_params$use.grad, "\n")
          cat("\tpenalty.par.value: ", private$..best_tuning_params$penalty.par.value, "\n")
          cat("\tcv_folds: ", private$..best_tuning_params$cv_folds, "\n")
        }
        file_path = str_c(private$..vis_dir, "/", private$..importance_plotname)
        png(filename = file_path)
        importance(private$..rule_fit_model$finalModel)
      }
      invisible(self)
    }
  ),
  active = list(
    data = function(value){
      if(missing(value)){
        private$..data_df
      }
      else{
        stop("$data is read only", call. = FALSE)
      }
    },
    data_with_evo = function(value){
      if(missing(value)){
        private$..data_df_with_evo
      }
      else{
        stop("$data_with_evo is read only", call. = FALSE)
      }
    },
    evo_features = function(value){
      if(missing(value)){
        private$..evo_features
      }
      else{
        stop("$evo_features is read only", call. = FALSE)
      }
    },
    model = function(value){
      if(missing(value)){
        private$..rule_fit_model
      }
      else{
        stop("$model is read only", call. = FALSE)
      }
    },
    accuracy = function(value){
      if(missing(value)){
        private$..model_accuracy
      }
      else{
        stop("$accuracy is read only", call. = FALSE)
      }
    },
    kappa = function(value){
      if(missing(value)){
        private$..model_kappa
      }
      else{
        stop("$kappa is read only", call. = FALSE)
      }
    },
    learnrate = function(value){
      if(missing(value)){
        private$..model_learnrate
      }
      else{
        stop("$learnrate is read only", call. = FALSE)
      }
    },
    hyper_params = function(value){
      if(missing(value)){
        private$..best_tuning_params
      }
      else{
        stop("$hyper_params is read only", call. = FALSE)
      }
    },
    validation_result = function(value){
      if(missing(value)){
        private$..model_validation_results
      }
      else{
        stop("$validation_results is read only", call. = FALSE)
      }
    },
    train_set = function(value){
      if(missing(value)){
        private$..train_set
      }
      else{
        stop("$train_set is read only", call. = FALSE)
      }
    },
    validation_set = function(value){
      if(missing(value)){
        private$..validation_set
      }
      else{
        stop("$validation_set is read only", call. = FALSE)
      }
    }
  )
)
