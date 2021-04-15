################################################################################
## control script include alle directories to source the subproject           ##
##                                                                            ##
##                                                                            ##
## @author: Toni Lange                                                        ##
## @institution:                                                              ##
##      Center for Evidence-based Healthcare                                  ##
##      University Hospital and Medical Faculty Carl Gustav Carus,            ##
##      TU Dresden, Germany                                                   ##
##                                                                            ##
## @date: 04.09.2018                                                          ##
################################################################################

#****************************************
# run in Terminal with projetc directory
#****************************************
# # # source("01rscripts/03deeplearning/00control.R")

options(max.print = 500, stringsAsFactors = F, expressions = 500000, warning.length = 5000L)

debug <- T # only the first two scenarios will be used
# debug <- F # use all scenarios

#*******************************************************************************
# load data
#*******************************************************************************

load("00data/rdata/01preprocessing/09_grid_sl.RData")
df_data <- as.data.frame(df_data)
df_data_u02 <- as.data.frame(df_data_u02)
df_data_u03 <- as.data.frame(df_data_u03)
df_data_u04 <- as.data.frame(df_data_u04)
df_data_u05 <- as.data.frame(df_data_u05)

fd <- "01rscripts/03deeplearning/"

#*******************************************************************************
# packages
#*******************************************************************************
source(paste(fd,"01packages.R", sep = ""))

#*******************************************************************************
# functions
#*******************************************************************************
source(paste(fd,"02functions.R", sep = ""))

#*******************************************************************************
# word embedding
#*******************************************************************************

source(paste(fd,"03_wordembedding/00_wordembedding_runs.R", sep = ""))

#*******************************************************************************
# preparation
#*******************************************************************************

# source(paste(fd,"04_pad_seq_ftunes.R", sep = ""))
# source(paste(fd,"04_pad_seq_caret.R", sep = ""))

#*******************************************************************************
# DNN
#*******************************************************************************
# user <- 2
# source(paste(fd,"06_dnn/02_mlp_caret/00_dl_dnn_ml_info.R", sep = ""))
# source(paste(fd,"06_dnn/02_mlp_caret/00_dl_dnn_runs.R", sep = ""))
# source(paste(fd,"06_dnn/02_mlp_caret/06_dl_dnn_prep_eval.R", sep = ""))

#*******************************************************************************
# CNN
#*******************************************************************************
user <- 1
# source(paste(fd,"05_cnn/02_cnn_caret/000_infos/000_dl_cnn_2mlp_info.R", sep = ""))
# source(paste(fd,"05_cnn/02_cnn_caret/000_infos/000_dl_mlp_info.R", sep = ""))
# source(paste(fd,"05_cnn/02_cnn_caret/000_infos/000_dl_cnn_lstm_info.R", sep = ""))
source(paste(fd,"05_cnn/02_cnn_caret/000_infos/000_dl_cnn_lstm_new_info.R", sep = ""))
# source(paste(fd,"05_cnn/02_cnn_caret/000_infos/000_dl_cnn_single_info.R", sep = ""))
# source(paste(fd,"05_cnn/02_cnn_caret/00_dl_runs.R", sep = ""))
# source(paste(fd,"05_cnn/02_cnn_caret/06_dl_prep_eval.R", sep = ""))

source(paste(fd,"05_cnn/02_cnn_caret/000_pretest/02_model_fit_cnn_lstm.R", sep = ""))

#*******************************************************************************
# RNN
#*******************************************************************************
