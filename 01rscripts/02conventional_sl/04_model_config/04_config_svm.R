#*******************************************************************************
#*******************************************************************************
#######################    logistic regression  runs   #########################
#*******************************************************************************
#*******************************************************************************

# configure the model runs

#*******************************************************************************
fd_data <- "00data/rdata/02conventional_sl/"
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# model
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# ml_method <- "svmLinearWeights2" # hat kein probModel - kein sensi training
ml_method <- "svmLinearWeights" # hat kein L2 regularisierung
# ml_method <- "svmLinear3" # hat kein probModel - kein sensi training
# ml_method <- "svmRadialWeights" # hat kein L2 regularisierung

# tuning parameter
modelLookup(ml_method)

# naming convention vor data output
ml_model <- "svm"
ml_model_path <- "04_svm"

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# define a grid of parameters
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

tune_grid <- expand.grid(
  cost = c(0.2, 0.5, 1, 5, 10, 30, 60),
  # Loss = c("L2"),
  weight = c(seq(0.5, 1.5, 0.1))
)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# select parameter for scenarios
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

rev_id <- length(table(df_data$rev_id)) # or choose a selection rev_id <- 1:1
input <- c("tiab") # filter 
f <- !paste0(ml_model,"_", grid_sl$data_id_config, ".RDS") %in% 
  list.files(path = paste0(fd_data,ml_model_path,"/"))
grid_f <- grid_sl[f,]
grid_f <- grid_f %>% filter(rev_id_config %in% rev_id & grid %in% input)
if(debug == TRUE){
grid_f %<>% slice(1:2) # for debug
}


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# source model training and prediction for each data szenario
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

source(paste0("01rscripts/02conventional_sl/03_training_config/00_runs.R"))

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# source prepare evaluation
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

source(paste0("01rscripts/02conventional_sl/03_training_config/04_prepare_eval.R"))
