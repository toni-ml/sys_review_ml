#*******************************************************************************
#*******************************************************************************
#######################    Model tuning   ######################################
#*******************************************************************************
#*******************************************************************************

# train the model

#*******************************************************************************

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# train and tune model
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#+++++++++++++++++++++++
tic("model fit")
#+++++++++++++++++++++++

if(ctrl$allowParallel){
  # register Cluster
  cores <- tidy_cores()
  cl <- makeCluster(cores, outfile = "", type='SOCK')
  registerDoSNOW(cl)
}

set.seed(42)
ls_model$fit <-
  caret::train(y_label ~ .,
               data = select(ls_split$train_dtm, -c("pub_id")),
               method = ml_method,
               trControl = ctrl,
               tuneGrid = tune_grid,
               metric = "brier_score", 
               maximize = FALSE # to choose lowest brier_score
  )

if(ctrl$allowParallel){
  stopCluster(cl)
}

#+++++++++++++++++++++++
time_fit <- toc() 
#+++++++++++++++++++++++
