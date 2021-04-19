#*******************************************************************************
#*******************************************************************************
#######################    Run model tuning    #################################
#*******************************************************************************
#*******************************************************************************

# configure the model runs

#*******************************************************************************

#+++++++++++++++++++++++
tic("total script: 01_runs.R")
#+++++++++++++++++++++++

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# source train control
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
source(paste0("01rscripts/02conventional_sl/03_training_config/01_train_control.R"))


#*******************************************************************************
####################           runs              ###############################
#*******************************************************************************

sink(paste0(fd_data, ml_model_path, "/", ml_model, "_runs.txt"), append = TRUE)
pb <- txtProgressBar(max = nrow(grid_f), style = 3, width = 100)
sink()
for (j in 1:nrow(grid_f)) {
  #+++++++++++++++++++++++
  tic(paste("run:", j))
  #+++++++++++++++++++++++
  
  #+++++++++++++++++++++++
  # sink I
  #+++++++++++++++++++++++
  sink(
    paste0(
      fd_data, ml_model_path, "/",
      ml_model, "_tuning_rev_", formatC(grid_f$rev_id_config[j],width = 2, flag = 0), ".txt"
    ), append = TRUE)
  
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # define names
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  input_name <- paste0("ls_split_", grid_f$data_id_config[j], ".RDS")
  output_name <- paste0(ml_model, "_", grid_f$data_id_config[j], ".RDS")
  cat(paste("\n -->", output_name, "\n\n"))
  
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # load data
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  #+++++++++++++++++++++++
  tic("load data")
  #+++++++++++++++++++++++
  
  ls_split <- readRDS(paste0("00data/rdata/01preprocessing/12_split_bow/",input_name))
  
  #+++++++++++++++++++++++
  time_load <- toc() 
  #+++++++++++++++++++++++
  
  if(ml_model == "null_model"){
  # reduce size to speed up runs
  ls_split$data_dtm %<>% select(1:3) 
  ls_split$train_dtm %<>% select(1:3) 
  ls_split$test_dtm %<>% select(1:3) 
  }
  
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # dtm manipulation for correct test data dtm in case of downsampling and token reduction
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  if(ml_model != "null_model"){
    # Make sure that all the tokens from the training dtm are also in the test dtm
    if(str_detect(grid_f$data_id_config[j],"u02|u03|u04|u05") | str_detect(grid_f$data_id_config[j], "05h|01k|05k|10k|15k")){
      
      # original test data
      new <- str_replace(grid_f$data_id_config[j], "u02|u03|u04|u05", "org")
      new <- str_replace(new, "05h|01k|05k|10k|15k", "org")
      new <- str_replace(string = new, pattern = substring(text = new, first = 1, last = 4), replacement = "")
      files <- list.files(path = paste0("00data/rdata/01preprocessing/12_split/"))
      
      ls_split_org <- readRDS(paste0("00data/rdata/01preprocessing/12_split/",files[str_detect(string = files, pattern = new)]))
      
      tokens_lost <- names(ls_split$train_dtm)[!names(ls_split$train_dtm) %in% names(ls_split_org$test_dtm)]
      to_add_test <- as.data.frame(matrix(data = 0, nrow(ls_split_org$test_dtm), ncol = length(tokens_lost)))
      names(to_add_test) <- tokens_lost
      
      ls_split$test_dtm <- bind_cols(ls_split_org$test_dtm, to_add_test)
      rm(ls_split_org)
    }
  }  
  
  if(ml_model == "c50"){
  # necessary step, because C5.0 does not handle "outcome" as columnname
  names(ls_split$train_dtm)[names(ls_split$train_dtm) == "outcome"] <- "outcomes"
  names(ls_split$test_dtm)[names(ls_split$test_dtm) == "outcome"] <- "outcomes"
  names(ls_split$data_dtm)[names(ls_split$data_dtm) == "outcome"] <- "outcomes"
  }
  
  
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # dtm specific tuning parameter
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  if(ml_model == "rf"){
  # 5% mtry - Update mtry_default for Random Forest
  mtry_default <- floor(sqrt(ncol(ls_split$train_dtm))) - 1
  tune_grid <- expand.grid(
    mtry = c(mtry_default: (mtry_default + floor(mtry_default *0.05))),
    splitrule = c("gini", "extratrees"),
    min.node.size = c(1, 3, 5)
  )
  source(paste0("01rscripts/02conventional_sl/03_training_config/01_train_control.R"))
  }
  
  if(ml_model == "avNNet"){
  max_size <- max(tune_grid$size)
  max_features <- ncol(select(
    ls_split$train_dtm,
    -c("pub_id")
  ))
  num_weights <- (max_size * max_features + 1) + max_size + 1
  source(paste0("01rscripts/02conventional_sl/03_training_config/01_train_control.R"))
  }
  
  
  
  
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # prepare output list
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  ls_model <- list(fit = vector(mode = "list",length = 1), 
                   train_pred_raw = vector(mode = "list",length = 1),
                   train_pred_prob = vector(mode = "list",length = 1),
                   train_y_ref = vector(mode = "list",length = 1),
                   test_pred_raw = vector(mode = "list",length = 1),
                   test_pred_prob = vector(mode = "list",length = 1),
                   test_y_ref = vector(mode = "list",length = 1),
                   train_cm = vector(mode = "list",length = 1),
                   test_cm = vector(mode = "list",length = 1)) 
  
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # source model training
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  source(paste0("01rscripts/02conventional_sl/03_training_config/02_training.R"))
  
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # source model prediction
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  source(paste0("01rscripts/02conventional_sl/03_training_config/03_prediction.R"))
  
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # save model run
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  #+++++++++++++++++++++++
  tic("save data")
  #+++++++++++++++++++++++
  
  saveRDS(ls_model, paste0(fd_data,ml_model_path, "/",output_name))
  
  #+++++++++++++++++++++++
  time_save <- toc()
  time_run <- toc()
  write.table(data.frame(token = output_name, 
                         cores = tidy_cores(),
                         time_run =  hms::as_hms(time_run$toc -  time_run$tic),
                         time_load =  hms::as_hms(time_load$toc -  time_load$tic), 
                         time_fit =  hms::as_hms(time_fit$toc -  time_fit$tic),
                         time_pred_train =  hms::as_hms(time_pred_train$toc -  time_pred_train$tic),
                         time_pred_test =  hms::as_hms(time_pred_test$toc -  time_pred_test$tic),
                         time_save =  hms::as_hms(time_save$toc -  time_save$tic),
                         row.names = NULL), 
              file = paste0(fd_data, ml_model_path,"/time.csv"),
              sep = ";", append = T, 
              row.names = F,
              col.names = !file.exists(paste0(fd_data, ml_model_path,"/time.csv")))
  #+++++++++++++++++++++++

  #+++++++++++++++++++++++
  # sink I
  #+++++++++++++++++++++++
  cat(paste(strrep("_", 40), "\n"))
  sink()
  
  #+++++++++++++++++++++++
  # sink II
  #+++++++++++++++++++++++
  sink(paste0(fd_data, ml_model_path, "/", ml_model, "_runs.txt"), append = TRUE)
  setTxtProgressBar(pb, j)
  sink()
}
close(pb)

#+++++++++++++++++++++++
sink(paste0(fd_data, ml_model_path, "/", ml_model, "_runs.txt"), append = TRUE)
cat(paste("\n"))
toc()
sink()
#+++++++++++++++++++++++
