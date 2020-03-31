## Global Variables
minimal_change <- list()
feat_change <- list()
y_value <- 0
robust_rules <- list()
stop <- FALSE
feat_values <- list()
feature_values <- list()
res <- TRUE
prediction_value <- 0
isSatisfied <- list()
rule_pattern <- "([a-zA-Z_0-9]*)\\s(<|<=|>|>=|=|.in.)\\s([0-9].[0-9]*|c\\(\\\"[0-1]\\\"\\))"
feat_pattern <- "([a-zA-Z_0-9]*(?!%in%))"
value_pattern <- "(\\s[0-9]\\.*[0-9]*|c\\(\\\"[0-1]\\\"\\))"
operator_pattern <- "(<=|<|>=|>|=|%in%)"
reformed_rule <- ""
transformed_rules <- list()

#' Get class label from continuous prediction using a threshold 0.1
#'
#' @param prediction continuous y-prediction value
#'
#' @return 1 if prediction > 0.1 and 0 otherwise
#' 
#' @export
#'
get_class <- function(prediction){
  if(prediction > 0.1){
    return(1)
  }
  else{
    return(0)
  }
}

#' Compute Minimal Changes in feature values based on rule changes
#'
#' @param robust_rules rules that needs to be changed in order ti change the model prediction. i.e., if the rule is satisfied, it needs to be dissatisfied and vice versa
#' @param participant_data data frame with participant's original feature values
#' @param predicted_value predicted y-value of the model for the participant
#' 
#' @return named list of minmal changes with rule, changes in each feature associated with the rule and whether the rule was initially satisfied by the participant
#'
#' @export
compute_minimal_change <- function(robust_rules, participant_data, predicted_value){
  minimal_change <<- list()
  minimal_change$rule_change <<- list()
  map(robust_rules$rules, function(rule){
    feat_change <<- list()
    map(rule$feat_values[[1]], function(feat){
      actual_value = participant_data[1, feat$feature][[1]]
      change = 0
      if(is.factor(actual_value)){
        actual_value = as.numeric(levels(actual_value)[actual_value])
        if(feat$value == "c(\"0\")"){
          change = abs(actual_value - 1)
        }
        else if(feat$value == "c(\"1\")"){
          change = abs(actual_value - 1)
        }
      }
      else{
        change = feat$value - actual_value
      }
      feat_change <<- list.append(feat_change, list("feature"=feat$feature, "value"=change))
    })
    minimal_change$rule_change <<- list.append(minimal_change$rule_change, list("rule" = rule$description, "minimalChange" = feat_change,"isSatisfied"=rule$isSatisfied))
  })
  
  return(minimal_change)
}

#' Find robust rules i.e., the rules that needs to be changed for the model prediction for the participant to change
#'
#' @param rules data frame of rules, their description and coefficient observed in the rulefit model
#' @param predicted_value y-prediction of the model for the participant
#' @param participant_data data frame with participant's original feature values
#' 
#' @return named list of minmal changes with rule, changes in each feature associated with the rule and whether the rule was initially satisfied by the participant
#' 
#' @export
#' 
find_robust_rules <- function(rules, predicted_value, participant_data){
  y_value <<- predicted_value
  robust_rules <<- list()
  robust_rules$rules <<- list()
  by(rules, 1:nrow(rules), function(rule){
    
    if(!stop){
      if(predicted_value > 0.1){
        if((rule$coefficient < 0 && !rule$isSatisfied[[1]]) || (rule$coefficient > 0 && rule$isSatisfied[[1]])){
          robust_rules$rules <<- list.append(robust_rules$rules, rule)
          y_value <<- predicted_value + rule$coefficient
        }
      }
      else{
        if((rule$coefficient < 0 && rule$isSatisfied[[1]]) || (rule$coefficient > 0 && !rule$isSatisfied[[1]])){
          robust_rules$rules <<- list.append(robust_rules$rules, rule)
          y_value <<- predicted_value + rule$coefficient
        }
      }
      if(get_class(y_value) !=  get_class(predicted_value)){
        stop <<- TRUE
      }
    }
  })
  change = compute_minimal_change(robust_rules, participant_data, predicted_value)
  return(change)
}

#' Parse Rules to replace scaled values of features with original values and extract operators and features for comparison with participant data
#' 
#' @param rules_df data frame of rules, their description and coefficient observed in the rulefit model
#' @param minmax_vals minmax values for each column, used to find the original value of the feature from scaled value
#' 
#' @return data frame with parsed rules 
#' 
#' @export
#' 
parse_rules <- function(rules_df, minmax_vals){
  transformed_rules <<- rules_df
  rules = rules_df
  feature_values <<- list()

  by(rules_df, 1:nrow(rules_df), function(rule){
    feat_values <<- list()
    description = rule$description
    extracted_rules <- str_extract_all(description, rule_pattern)
    reformed_rule <<- ""
    map(extracted_rules[[1]], function(constraint){
      feature = str_extract(constraint, feat_pattern)
      feat_value = str_extract(constraint, value_pattern)
      feat_value = str_trim(feat_value, side = c("left"))
      if(feat_value != "c(\"0\")" && feat_value != "c(\"1\")"){
        feat_value = as.numeric(feat_value)
        # get original value of the feature using minmax_vals
        feat_value = feat_value * (minmax_vals[[feature]]$max - minmax_vals[[feature]]$min) + minmax_vals[[feature]]$min
      }
      operator = str_extract(constraint, operator_pattern)
      reformed_rule <<- str_c(reformed_rule, " ", feature, " ", operator, " ", feat_value, " ", "&")
      feat_values <<- list.append(feat_values, list("feature"=feature, "value"=feat_value, "operator"=operator))
    })
    reformed_rule <<- str_remove_all(reformed_rule, "(^\\s)|(\\s\\&$)")
    transformed_rules[transformed_rules$description == rule$description, "description"] <<- reformed_rule
    feature_values <<- list.append(feature_values, feat_values)
  })
  rules$feat_values = feature_values
  rules$description = transformed_rules$description
  return(rules)
}

#' Perform operation to compare the rule chunk and the participant data
#'
#' @param part_value participant feature value
#' @param rule_value feature value in rule chunk
#' 
#' @export
#' 
perform_operation <- function(part_value, rule_value, operator){
  if(rule_value == "c(\"0\")"){
    rule_value = c(0)
  }
  else if(rule_value == "c(\"1\")"){
    rule_value = c(1)
  }
  switch (operator,
    "<" = return(part_value < rule_value),
    ">" = return(part_value > rule_value),
    "<=" = return(part_value <= rule_value),
    ">=" = return(part_value >= rule_value),
    "%in%" = return(part_value %in% rule_value),
    "_in_" = return(part_value %in% rule_value)
  )
}

#' Check whether the rulw is satisfied by the participant
#' 
#' @param rule rule to compare
#' @param participant_data data frame with participant's original feature values
#' 
#' @return TRUE if satisfied, FALSE otherwise
#' 
#' @export
#'
check_rule <- function(rule, participant_data){
  res <<- TRUE
  map(rule$feat_values[[1]], function(feat){
    result = perform_operation(participant_data[1, feat$feature][[1]], feat$value, feat$operator) 
    res <<- res && result
  })
  return(res)
}

#' Compute Predicted y-value from the rulefit model
#'
#' @param rules_df data frame of rules, their description and coefficient observed in the rulefit model
#'
#' @return continuous y-prediction value
#'   
#' @export
#'
compute_predicted_value <- function(rules){
  prediction_value <<- 0.84 #y-intercept
  by(rules, 1:nrow(rules), function(rule){
    if(rule$isSatisfied[[1]]){
      prediction_value <<- prediction_value + rule$coefficient[[1]]
    }
  })
  return(prediction_value)
}

#' Get minimal change for each feature value of the participant given the model and set of rules derived from the model
#'
#' @param rules_df data frame of rules, their description and coefficient observed in the rulefit model
#' @param participant_index row number of the participant for whom the minimal change needs to be calculated
#' @param train_set training set used in model building
#' @param minimal_feature_set list of critical features derived from the rulefit model
#' @param minimax_vals named list of minmax values for each feature in the dataset
#' 
#' @return dictionary of particpant changes including rules derived from the model
#' 
#' @export
#'
get_minimal_change <- function(rules_df, participant_index, train_set, minimal_feature_set, minmax_vals){
  participant_data <- train_set[participant_index, ]
  participant_data$liver_fat <- NULL
  part_prob_pred <- predict(ship_study_results$model, participant_data[1,], type="prob")
  if(part_prob_pred[1, "1"] >= 0.5){
    part_prob_pred <- part_prob_pred[1, "1"]
  }
  else{
    part_prob_pred <- part_prob_pred[1, "0"]
  }
  participant_data <- participant_data[1, colnames(participant_data) %in% minimal_feature_set]
  lapply(minimal_feature_set, function(feature){
    feat_value = participant_data[[feature]]
    if(is.numeric(feat_value)){
      participant_data[[feature]] <<- feat_value * (minmax_vals[[feature]]$max - minmax_vals[[feature]]$min) + minmax_vals[[feature]]$min
    }
  })
  actual_label <- train_set[participant_index, "liver_fat"];
  rules = parse_rules(rules_df, minmax_vals)
  isSatisfied <<- list()
  by(rules, 1:nrow(rules), function(rule){
    isSatisfied <<- list.append(isSatisfied, check_rule(rule, participant_data))
  })
  rules$isSatisfied = isSatisfied
  predicted_value = compute_predicted_value(rules)
  rules = rules[order(-abs(rules$coefficient)), ]
  change = find_robust_rules(rules, predicted_value, participant_data)
  participant_changes = list("changes"=change$rule_change, "rulesSet"=rules, "prediction"=actual_label$liver_fat[[1]], "participantId"=participant_index, "yPrediction"=predicted_value, "predictedProb"=part_prob_pred)
  stop <<- FALSE
  return(participant_changes)
}