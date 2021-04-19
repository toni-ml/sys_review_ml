#*******************************************************************************
#*******************************************************************************
#################    logistische Regression  eval   ############################
#*******************************************************************************
#*******************************************************************************

# prepare eval for each data szenario

#*******************************************************************************

#+++++++++++++++++++++++
tic("total script: 04_prepare_eval.R")
#+++++++++++++++++++++++

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# choose parameter for scenarios
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

rev_id <- length(table(df_data$rev_id)) # or choose a selection rev_id <- 1:1
input <- c("tiab") # filter 
f <- paste0(ml_model,"_", grid_sl$data_id_config, ".RDS") %in% 
  list.files(path = paste0(fd_data,ml_model_path,"/"))
grid_f <- grid_sl[f,]
grid_f <- grid_f %>% filter(rev_id_config %in% rev_id & grid %in% input)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Time
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

times <- read_delim(paste0(fd_data,ml_model_path, "/time.csv"), 
                    ";", escape_double = FALSE, col_names = TRUE, trim_ws = TRUE, col_types = cols(
                      token = col_character(),
                      cores = col_double(),
                      time_run = col_time(format = ""),
                      time_load = col_time(format = ""),
                      time_fit = col_time(format = ""),
                      time_pred_train = col_time(format = ""),
                      time_pred_test = col_time(format = ""),
                      time_save = col_time(format = "")
                    ))


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# fill data frame with outcomes of model training and predication
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#+++++++++++++++++++++++
tic("store eval prep")
#+++++++++++++++++++++++

cores <- tidy_cores()
cl <- makeCluster(cores, outfile = "", type='SOCK')
registerDoSNOW(cl)

pb <- txtProgressBar(max = nrow(grid_f), style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

eval_list <- foreach(i = 1:nrow(grid_f), 
                     .options.snow = opts, 
                     .packages = c("tidyverse", "ModelMetrics", "MLmetrics", "hms"),
                     .export = c("times")) %dopar% {
                       if(ml_model == "null_model"){
                         store_eval_dat_null_modell(i)
                       } else {
                         store_eval_dat(i)
                       }
                       
                     }
stopCluster(cl)
close(pb)

eval_dat <- bind_rows(eval_list)

#+++++++++++++++++++++++
toc() 
#+++++++++++++++++++++++

#*******************************************************************************
################################      save    ##################################
#*******************************************************************************

#+++++++++++++++++++++++
tic("save data")
#+++++++++++++++++++++++

saveRDS(eval_dat %>% as_tibble(), paste0("02outputs/02conventional_sl/",ml_model_path, "/eval_",ml_model, ".RDS"))

#+++++++++++++++++++++++
toc()
toc()
#+++++++++++++++++++++++
