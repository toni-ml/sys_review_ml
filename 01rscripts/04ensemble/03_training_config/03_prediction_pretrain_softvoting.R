#*******************************************************************************
#*******************************************************************************
#######################    Model prediction   ##################################
#*******************************************************************************
#*******************************************************************************

# predict on full training data set and test data

#*******************************************************************************

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# pedict on train data
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#+++++++++++++++++++++++
tic("prediction on train data")
#+++++++++++++++++++++++

soft_voting_train <- vector(mode = "list", length = length(ml_baselearner))

for (i in 1:length(ml_baselearner)){
  soft_voting_train[[i]] <- get(paste0("baselearner", formatC(i,width = 2, flag = 0)))$train_pred_prob$inclusion
}

names(soft_voting_train) <- ml_baselearner

ls_model$soft_voting_train <- bind_cols(soft_voting_train) %>% 
  mutate(vote_prob = rowMeans(.),
         vote_class = factor(ifelse(vote_prob > trehshold, "inclusion", "exclusion"), levels = c("inclusion", "exclusion")))

# predict raw classes
ls_model$train_pred_raw <- ls_model$soft_voting_train %>% pull(vote_class)

# predict probs
ls_model$train_pred_prob <- ls_model$soft_voting_train %>% pull(vote_prob)

# y_label reference 
ls_model$train_y_ref <- ls_split$train_dtm$y_label

# confusion matrix
ls_model$train_cm <- caret::confusionMatrix(
  data = ls_model$train_pred_raw,
  reference = ls_model$train_y_ref,
  mode = "everything",
  positive = "inclusion"
)

#+++++++++++++++++++++++
time_pred_train <- toc()
#+++++++++++++++++++++++

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# pedict on test data
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#+++++++++++++++++++++++
tic("prediction on test data")
#+++++++++++++++++++++++

soft_voting_test <- vector(mode = "list", length = length(ml_baselearner))

for (i in 1:length(ml_baselearner)){
  soft_voting_test[[i]] <- get(paste0("baselearner", formatC(i,width = 2, flag = 0)))$test_pred_prob$inclusion
}

names(soft_voting_test) <- ml_baselearner

ls_model$soft_voting_test <- bind_cols(soft_voting_test) %>% 
  mutate(vote_prob = rowMeans(.),
         vote_class = factor(ifelse(vote_prob > trehshold, "inclusion", "exclusion"), levels = c("inclusion", "exclusion")))

# predict raw classes
ls_model$test_pred_raw <- ls_model$soft_voting_test %>% pull(vote_class)

# predict probs
ls_model$test_pred_prob <- ls_model$soft_voting_test %>% pull(vote_prob)

# y_label reference 
ls_model$test_y_ref <- ls_split$test_dtm$y_label

# confusion matrix
ls_model$test_cm <- caret::confusionMatrix(
  data = ls_model$test_pred_raw,
  reference = ls_model$test_y_ref,
  mode = "everything",
  positive = "inclusion"
)

#+++++++++++++++++++++++
time_pred_test <- toc()
#+++++++++++++++++++++++
