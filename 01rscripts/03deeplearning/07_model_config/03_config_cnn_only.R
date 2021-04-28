#*******************************************************************************
#*******************************************************************************
#######################    DNN on BoW                  #########################
#*******************************************************************************
#*******************************************************************************

# configure the model runs

#*******************************************************************************
fd_data <- "00data/rdata/03deeplearning/"
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# model
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


source(paste0("01rscripts/03deeplearning/05_model_info/00_dl_cnn_only_info.R"))
ml_method <- cnn_only
ml_model_name <- "CNN"

# tuning parameter
# modelLookup(ml_method)

# naming convention vor data output
ml_model <- "cnn_only"
ml_model_path <- "03_cnn_only"



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# define a grid of parameters
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

tune_grid <- expand.grid(
  filters_conv_1 = c(150),
  filters_size_conv_1 = c(3),
  batch_size = c(20),
  epochs = c(20),
  patience = c(10),
  lr = c(0.01),
  class_weights = c(1),
  activation = "relu"
)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# select parameter for scenarios
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

rev_id <- length(table(df_data$rev_id)) # or choose a selection rev_id <- 1:1
input <- c("tiab") # filter 
f <- !paste0(ml_model,"_", grid_sl$data_id_config, ".RDS") %in% 
  list.files(path = paste0("00data/rdata/03deeplearning/",ml_model_path,"/"))
grid_f <- grid_sl[f,]
grid_f <- grid_f %>% filter(rev_id_config %in% rev_id & grid %in% input)
if(debug == TRUE){
  grid_f %<>% slice(1:2) # for debug
}


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# source model training and prediction for each data szenario
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

source(paste0("01rscripts/03deeplearning/06_training_config/00_runs.R"))

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# source prepare evaluation
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

source(paste0("01rscripts/03deeplearning/06_training_config/04_prepare_eval.R"))
