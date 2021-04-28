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


source(paste0("01rscripts/03deeplearning/05_model_info/00_dl_mlp_bow_info.R"))
ml_method <- mlp_bow
ml_model_name <- "Multilayer Perceptron"

# tuning parameter
# modelLookup(ml_method)

# naming convention vor data output
ml_model <- "mlp_bow"
ml_model_path <- "01_mlp_bow"



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# define a grid of parameters
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

tune_grid <- expand.grid(
  units_1 = c(512),
  dropout_1 = c(0.2),
  units_2 = c(256),
  dropout_2 = c(0.2),
  units_3 = c(64),
  batch_size = c(20),
  epochs = c(20), 
  patience = c(10),
  lr = 0.01,
  decay = 0,
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
