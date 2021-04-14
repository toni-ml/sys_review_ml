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

# predict raw classes
ls_model$train_pred_raw <- predict(
  object = ls_model$fit,
  newdata = select(ls_split$train_dtm, 
                   -c("pub_id", "y_label")),
  type = "raw"
)

# predict probs
ls_model$train_pred_prob <- predict(
  object = ls_model$fit,
  newdata = select(ls_split$train_dtm, 
                   -c("pub_id", "y_label")),
  type = "prob"
)

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
toc()
#+++++++++++++++++++++++

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# pedict on test data
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#+++++++++++++++++++++++
tic("prediction on test data")
#+++++++++++++++++++++++

# predict raw classes
ls_model$test_pred_raw <- predict(
  object = ls_model$fit,
  newdata = select(ls_split$test_dtm, 
                   -c("pub_id", "y_label")),
  type = "raw"
)

# predict probs
ls_model$test_pred_prob <- predict(
  object = ls_model$fit,
  newdata = select(ls_split$test_dtm, 
                   -c("pub_id", "y_label")),
  type = "prob"
)

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
toc()
#+++++++++++++++++++++++
