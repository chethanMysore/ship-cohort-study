ShipCohortStudy <- R6::R6Class("ShipCohortStudy", private = list(
  ..wave_suffix = "(_s0|_s1|_s2)",
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
  ..vis_dir = "/visualization"
  ),
  public = list(
    initialize = function(data_df, labels, cv_folds = 5){
      #checkmate::assert(data_df)
      #checkmate::assert(labels)
      checkmate::assert_data_frame(data_df)
      checkmate::assert_true(nrow(data_df) == length(labels))
      checkmate::assert_false(any(is.na(labels)))
      
      private$..labels <- labels
      private$..cv_folds <- cv_folds
      private$..data_df_with_evo <- data_df
      
      if(all(purrr::map_lgl(labels, ~ checkmate::test_numeric(.x)))){  # if the labels provided is a numeric array then convert it to factor
        labels <- only_labels(labels = labels)
        labels <- labels$liver_fat
      }

      ## Sample data into train and validation sets
      set.seed(42)
      train <- sample(1:nrow(private$..data_df_with_evo), 2 * nrow(private$..data_df_with_evo)/3)
      test <- (-train)
      private$..train_set <- private$..data_df_with_evo[train,]
      private$..validation_set <- private$..data_df_with_evo[test,]
      sample_class_labels <- private$..train_set$liver_fat[[1]]
      train_set <- private$..train_set[, !names(private$..train_set) %in% c("liver_fat")]
      
      ## Build Rule Fit Model
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
        file_path = str_c(getwd(), private$..vis_dir, "/", private$..importance_plotname)
        png(filename = file_path)
        importance(private$..rule_fit_model$finalModel)
        dev.off()
      }
      invisible(self)
    }
  ),
  active = list(
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
