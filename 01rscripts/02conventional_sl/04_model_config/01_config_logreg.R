#*******************************************************************************
#*******************************************************************************
#######################    logistic regression  runs   #########################
#*******************************************************************************
#*******************************************************************************

# configure the model runs

#*******************************************************************************

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# model
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

ml_method <- "glmnet"

# tuning parameter
modelLookup(ml_method)

# naming convention vor data output
ml_model <- "logreg"
ml_model_path <- "01_logreg"

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# define a grid of parameters
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

tune_grid <- expand.grid(
  alpha = seq(from = 0, to = 1, length.out = 10),
  lambda = 10^seq(from = 3, to = -3, length.out = 100)
)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# select parameter for scenarios
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

rev_id <- length(table(df_data$rev_id)) # or choose a selection rev_id <- 1:1
input <- c("tiab") # filter 
f <- !paste0(ml_model,"_", grid_sl$data_id_config, ".RDS") %in% 
  list.files(path = paste0("00data/rdata/02conventional_sl/",ml_model_path,"/"))
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
