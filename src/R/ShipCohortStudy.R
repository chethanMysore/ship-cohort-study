#' ShipCohortStudy encapsulates the model building task, model analysis tasks and participant analysis tasks achieved on SHIP dataset
#' 
#' Object includes evolutionary features extracted using evoxploit, training set, validation set, original dataset passed, rulefit model built and preprocessing result.
#' Object also exposes several methods for performing various analysis tasks on the participant data using the built model
#' 
#' \code{ShipCohortStudy} builds rulefit model on dataset and returns model results
#' 
#' @format \code{\link{R6Class}} object.
#' 
#' @section Usage:
#' \preformatted{
#' ship_study_results <- ShipCohortStudy$new(data_df)
#' ship_study_results$summary()
#' }
#'
#' @section Arguments:
#' 
#' For ShipCohortStudy$new(): 
#' \describe{
#' \item{data_df: }{('data.frame')\cr
#' The dataset with discrete class labels, without null values(NAs), scaled feature values 
#' }
#' \item{cv_folds: }{('num')\cr
#' (optional) The number of folds for cross validation
#' }
#' }
#'
#' @export
#'
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
  ..preprocessing_result = NULL
  ),
  public = list(
    initialize = function(data_df, cv_folds = 5){
      ## Assert if data_df is not NULL
      checkmate::assert_data_frame(data_df)
      
      ## Preprocess the data 
      cat("Preprocessing the data...\n")
      private$..preprocessing_result <- PreProcessing$new(data_df = data_df)
      private$..preprocessing_result$summary()
      data_df = private$..preprocessing_result$data_df
      labels = data_df$liver_fat
      
      ## Assert that labels does not contain 'NA's
      checkmate::assert_true(nrow(data_df) == length(labels))
      checkmate::assert_false(any(is.na(labels)))
      
      private$..labels <- labels
      private$..cv_folds <- cv_folds
      private$..data_df_with_evo <- data_df
      
      ## if the labels provided is a numeric array then convert it to factor
      if(all(purrr::map_lgl(private$..labels, ~ checkmate::test_numeric(.x)))){ 
        private$..labels <- only_labels(labels = private$..labels)
        private$..labels <- private$..labels$liver_fat
      }

      ## sampling of data using stratified k-fold cross validation
      cat("Sampling data using stratified ", cv_folds, "-fold cross validation...\n")
      train_index = stratified_sample(data_df = private$..data_df_with_evo, kfolds = private$..cv_folds, cat_colname = "liver_fat")
      
      ## validation set
      set.seed(42)
      validation_index = sample(train_index, 1)
      private$..validation_set <- private$..data_df_with_evo[validation_index[[1]],]
      
      ## Training set
      private$..train_set <-  private$..data_df_with_evo[-validation_index[[1]],]
      train_index = train_index[-match(validation_index, train_index)]
      train_labels = private$..data_df_with_evo$liver_fat
      private$..data_df_with_evo <- private$..data_df_with_evo[, !names(private$..data_df_with_evo) %in% c("liver_fat")]
      
      ## Build Rule Fit Model
      cat("Building RuleFit model...\n")
      private$..rule_fit_model <- rule_fit(data_df = private$..data_df_with_evo, class_labels = train_labels, cv_folds = (private$..cv_folds - 1), train_index = train_index)
      
      ## Store Results
      if(!is.null(private$..rule_fit_model)){
        private$..model_accuracy <- mean(as.vector(private$..rule_fit_model$results$Accuracy))
        private$..model_kappa <- mean(as.vector(private$..rule_fit_model$results$Kappa))
        private$..best_tuning_params <- private$..rule_fit_model$bestTune
        private$..best_tuning_params$cv_folds <- private$..cv_folds
      }  
    },
    
    #' Prints summary of the rulefit model
    #'
    #' Prints model performance evaluated against various metrics and best tuning parameters observed
    #' 
    #' @export
    #'
    summary = function(){
      cat("\n---------------------RuleFit Model Summary-----------------------\n")
      cat(nrow(private$..data_df_with_evo), " instances were observed.\n")
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
        # importance(private$..rule_fit_model$finalModel)
      }
    },
    
    #' Get co-ordinates for ICE plot
    #'
    #' @param feature_name feature name
    #' 
    #' @returns named list of ice points and pdp points for the plot
    #' 
    #' @export
    #'
    get_ice_coords = function(feature_name){
      ice_coords = get_ice_points(private$..rule_fit_model$finalModel$data, private$..rule_fit_model, feature_name = feature_name, frac_to_build = 0.27)
      return(ice_coords)
    },
    
    #' Get points for feature importance plot
    #'
    #' @return Returns critical set of features and their corresponding importance values observed from the rulefit model
    #' 
    #' @export
    #'
    get_feature_importance = function(){
      feature_imp = pre::importance(private$..rule_fit_model$finalModel)
      x = feature_imp$varimps$imp
      y = feature_imp$varimps$varname
      y_description = c("Somatometric measurements of waist", 
                         "Glucose",
                         "Triglycerides during S2 wave",
                         "Patient's similarity with negative cohort patients",
                         "Patient's similarity with positive cohort patients",
                         "Time of lights off during sleep", 
                         "Serum Uric Acid",
                         "Characteristic changes in the patient",
                         "Representativeness of the patient in the cohort group",
                         "Somatometric measurements of Body Mass Index", 
                         "Diabetes Level",
                         "Triglycerides during S0 wave"
      )
      feat_type = c("original", "original", "original", "evolutionary", "evolutionary", "original", "original", "evolutionary", "evolutionary", "original", "original", "original")
      importance_data = list(importance=x, features=y, featureDescription=y_description, featType=feat_type)
      return(importance_data)
    },
    
    #' Get model performance results
    #' 
    #' @return Returns named list of performance results of the model on test and training sets
    #' 
    #' @export
    #' 
    get_model_performance = function(){
      model_performance = extract_model_performance(private$..rule_fit_model, private$..train_set, private$..validation_set)
      train_results = model_performance$train_performance
      test_results = model_performance$test_performance
      overall_train = as.list(train_results$overall)
      byclass_train = as.list(train_results$byClass)
      overall_test = as.list(test_results$overall)
      byclass_test = as.list(test_results$byClass)
      performance_results = list(trainPerformance=data.frame("accuracy"=overall_train$Accuracy, 
                                                              "kappa"=overall_train$Kappa,
                                                              "positivePredictionValue"=byclass_train$`Pos Pred Value`,
                                                              "negativePredictionValue"=byclass_train$`Neg Pred Value`,
                                                              "sensitivity"=byclass_train$Sensitivity,
                                                              "specificity"=byclass_train$Specificity,
                                                              "precision"=byclass_train$Precision,
                                                              "f1"=byclass_train$F1
      ),
      testPerformance = data.frame("accuracy"=overall_test$Accuracy, 
                                 "kappa"=overall_test$Kappa,
                                 "positivePredictionValue"=byclass_test$`Pos Pred Value`,
                                 "negativePredictionValue"=byclass_test$`Neg Pred Value`,
                                 "sensitivity"=byclass_test$Sensitivity,
                                 "specificity"=byclass_test$Specificity,
                                 "precision"=byclass_test$Precision,
                                 "f1"=byclass_test$F1
      ))
      return(performance_results)
    },
    
    #' Get minimal change in a participant so that the class prediction changes
    #' 
    #' @param participant_id row number of the participant in the training set used to build the model
    #' 
    #' @return dictionary of rules and the feature changes associated with it so that the participant's prediction changes
    #'
    #' @export
    #'
    get_minimal_change = function(participant_id){
      feature_imp = importance(private$..rule_fit_model$finalModel)
      rules_coeff = select(feature_imp$baseimps, c("rule", "description", "coefficient"))
      feat_imp = feature_imp$varimps
      participant_changes = get_minimal_change(rules_coeff, participant_id, private$..train_set, feat_imp$varname, private$..preprocessing_result$minmax_vals)
      return(participant_changes)
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
