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

# source(paste(fd,"03_wordembedding/00_wordembedding_runs.R", sep = ""))

#*******************************************************************************
# input data preparation
#*******************************************************************************

# source(paste(fd,"04_pad_seq/04_pad_seq_bow.R", sep = "")) # use for caret 
# source(paste(fd,"04_pad_seq/04_pad_seq_emb.R", sep = "")) # use for ftunes

#*******************************************************************************
# DNN
#*******************************************************************************

source(paste(fd, "07_model_config/01_config_mlp_bow.R", sep = ""))
source(paste(fd, "07_model_config/02_config_mlp_emb.R", sep = ""))
source(paste(fd, "07_model_config/03_config_cnn_only.R", sep = ""))
source(paste(fd, "07_model_config/04_config_cnn_2mlp.R", sep = ""))


