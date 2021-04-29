#*******************************************************************************
#*******************************************************************************
#######################    ensemble pre trained caretlist    ###################
#*******************************************************************************
#*******************************************************************************

# configure the model runs

#*******************************************************************************
fd_data <- "00data/rdata/04ensemble/"
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# model
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

ml_method <- "ensemble"

# naming convention vor data output
ml_model <- "pre_caretlist"
ml_model_path <- "01_pretrained_caretlist"

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# define pretrained base-learner
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# # Baselearner
ml_baselearner <- c("logreg", "rf", "svm")
ml_baselearner_path <- c("02conventional_sl/01_logreg/", 
                         "02conventional_sl/03_rf/",
                         "02conventional_sl/04_svm/")


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

source(paste0("01rscripts/04ensemble/03_training_config/00_runs_pretrain.R"))

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# source prepare evaluation
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

source(paste0("01rscripts/04ensemble/03_training_config/04_prepare_eval_pretrain.R"))







