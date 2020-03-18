#Global Variables
participant_variants <- list()
minimal_change <- list()
participant_minimal_change <- list()
min_change <- c()

compute_robustness <- function(model, data_variants, participant_data, feature_name){
  actual_label = participant_data[1, "liver_fat"]
  actual_feature_value = participant_data[1, feature_name]
  participant_variants <<- list()
  by(data_variants, 1:nrow(data_variants), function(variant) {
    new_variant = participant_data
    if(new_variant[1, feature_name] != variant){
      new_variant[1, feature_name] = variant
      pred = predict(model, new_variant)
      if(pred != actual_label[[1]]){
        participant_variants <<- list.append(participant_variants, new_variant[1, feature_name][[1]])
      }
    }
  })
  
  if(length(participant_variants) > 0){
    map(participant_variants, function(variant) {
      min_change <<- append(min_change, (variant - actual_feature_value[[1]]))
    })
  }
  min_change <<- min_change[which.min(abs(min_change))]
  return(min_change)
}

enumerate_features <- function(model, train_set, participant_data, features){
  minimal_change <<- list()
  map(features, function(feature){
    feat_change = compute_robustness(model, train_set[, feature], participant_data, feature)
    minimal_change[[feature]] <<- feat_change
  })
  return(minimal_change)
}

get_minimal_change <- function(model, train_set, minimal_feature_set){
  participant_minimal_change <<- list()
  set.seed(42)
  rep_participants <- dplyr::sample_n(train_set, 1)
  by(rep_participants, 1:nrow(rep_participants), function(participant){
    part_minimal_change = enumerate_features(model, train_set, participant, minimal_feature_set)
    participant_minimal_change <<- list.append(participant_minimal_change, list("participant"=participant, "minimal_change"=part_minimal_change))
  })
  return(participant_minimal_change);
}

