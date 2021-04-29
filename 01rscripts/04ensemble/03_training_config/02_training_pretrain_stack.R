#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# train and tune model
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#+++++++++++++++++++++++
tic("model fit")
#+++++++++++++++++++++++


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# load Baselearner 1 - 3
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


for (i in 1:length(ml_baselearner)){
fit_name_baselearner <- paste0("00data/rdata/",ml_baselearner_path[i], ml_baselearner[i], 
                                 "_", grid_f$data_id_config[j], ".RDS")
assign(paste0("baselearner", formatC(i,width = 2, flag = 0)), value = readRDS(fit_name_baselearner))
}





#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# build ensemble of pre-trained models
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

model_list <- vector(mode = "list", length = length(ml_baselearner))

for (i in 1:length(ml_baselearner)){
model_list[[i]] <- get(paste0("baselearner", formatC(i,width = 2, flag = 0)))$fit
}

class(model_list) <- "caretList"

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Ensemble
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

if(ctrl$allowParallel){
  # register Cluster
  cores <- tidy_cores()
  cl <- makeCluster(cores, outfile = "", type='SOCK')
  registerDoSNOW(cl)
}

set.seed(42)
ls_model$fit <- caretStack(all.models = model_list,
                           method = "rf",
                           trControl = ctrl,
                           metric = "brier_score",
                           maximize = FALSE)

if(ctrl$allowParallel){
  stopCluster(cl)
}

#+++++++++++++++++++++++
time_fit <- toc() 
#+++++++++++++++++++++++
