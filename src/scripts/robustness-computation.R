#Global Variables
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

get_class <- function(prediction){
  if(prediction > 0.1){
    return(1)
  }
  else{
    return(0)
  }
}

compute_minimal_change <- function(robust_rules, participant_data, predicted_value){
  minimal_change <<- list()
  minimal_change$y_change <<- abs(robust_rules$y_change - predicted_value)
  minimal_change$rule_change <<- list()
  map(robust_rules$rules, function(rule){
    feat_change <<- list()
    map(rule$feat_values[[1]], function(feat){
      actual_value = participant_data[1, feat$feature][[1]]
      change = 0
      if(is.factor(actual_value)){
        actual_value = as.numeric(levels(actual_value)[actual_value])
        if(feat$value == "c(\"0\")"){
          change = abs(actual_value - 0)
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
    minimal_change$rule_change <<- list.append(minimal_change$rule_change, list("rule" = rule$description, "minimalChange" = feat_change))
  })
  
  return(minimal_change)
}


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
        robust_rules$y_change <<- y_value
        stop <<- TRUE
      }
    }
  })
  change = compute_minimal_change(robust_rules, participant_data, predicted_value)
  return(change)
}


parse_rules <- function(rules_df){
  rules = rules_df
  feature_values <<- list()
  rule_pattern <- "([a-zA-Z_0-9]*)\\s(<|<=|>|>=|=|.in.)\\s([0-9].[0-9]*|c\\(\\\"[0-1]\\\"\\))"
  feat_pattern <- "([a-zA-Z_0-9]*(?!%in%))"
  value_pattern <- "([0-9]\\.[0-9]*|c\\(\\\"[0-1]\\\"\\))"
  operator_pattern <- "(<|<=|>|>=|=|%in%)"
  by(rules_df, 1:nrow(rules_df), function(rule){
    feat_values <<- list()
    description = rule$description
    extracted_rules <- str_extract_all(description, rule_pattern)
    map(extracted_rules[[1]], function(rule){
      feature = str_extract(rule, feat_pattern)
      value = str_extract(rule, value_pattern)
      if(value != "c(\"0\")" && value != "c(\"1\")"){
        value = as.numeric(value)
      }
      operator = str_extract(rule, operator_pattern)
      feat_values <<- list.append(feat_values, list("feature"=feature, "value"=value, "operator"=operator))
    })
    feature_values <<- list.append(feature_values, feat_values)
  })
  rules$feat_values = feature_values
  return(rules)
}

perform_operation <- function(val1, val2, operator){
  if(val2 == "c(\"0\")"){
    val2 = c(0)
  }
  else if(val2 == "c(\"1\")"){
    val2 = c(1)
  }
  switch (operator,
    "<" = return(val1 < val2),
    ">" = return(val1 > val2),
    "<=" = return(val1 <= val2),
    ">=" = return(val1 >= val2),
    "%in%" = return(val1 %in% val2),
    "_in_" = return(val1 %in% val2)
  )
}


check_rule <- function(rule, participant_data){
  res <<- TRUE
  map(rule$feat_values[[1]], function(feat){
    result = perform_operation(participant_data[1, feat$feature][[1]], feat$value, feat$operator) 
    res <<- res && result
  })
  return(res)
}


compute_predicted_value <- function(rules){
  prediction_value <<- 0
  by(rules, 1:nrow(rules), function(rule){
    if(rule$isSatisfied[[1]]){
      prediction_value <<- prediction_value + rule$coefficient[[1]]
    }
  })
  return(prediction_value)
}


get_minimal_change <- function(rules_df, participant_index, train_set, minimal_feature_set){
  participant_data <- train_set[participant_index, colnames(train_set) %in% minimal_feature_set]
  actual_label <- train_set[participant_index, "liver_fat"];
  rules = parse_rules(rules_df)
  isSatisfied <<- list()
  by(rules, 1:nrow(rules), function(rule){
    isSatisfied <<- list.append(isSatisfied, check_rule(rule, participant_data))
  })
  rules$isSatisfied = isSatisfied
  predicted_value = compute_predicted_value(rules)
  rules = rules[order(-abs(rules$coefficient)), ]
  change = find_robust_rules(rules, predicted_value, participant_data)
  participant_changes = list("changes"=change$rule_change, "rulesSet"=rules_df, "prediction"=actual_label$liver_fat[[1]], "participantId"=participant_index, "yPrediction"=predicted_value, "yChange"=change$y_change)
  stop <<- FALSE
  return(participant_changes)
}
