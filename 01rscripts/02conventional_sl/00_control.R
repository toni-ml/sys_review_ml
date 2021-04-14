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
# # # source("01rscripts/02conventional_sl/00_control.R")

options(max.print = 500, stringsAsFactors = F, expressions = 500000, warning.length = 5000L)

debug <- T # only the first two scenarios will be used
# debug <- F # use all scenarios
#*******************************************************************************
# load data
#*******************************************************************************

load("00data/rdata/01preprocessing/09_grid_sl.RData")

fd <- "01rscripts/02conventional_sl/"

#*******************************************************************************
# packages
#*******************************************************************************
source(paste(fd,"01_packages.R", sep = ""))

#*******************************************************************************
# functions
#*******************************************************************************
source(paste(fd,"02_functions.R", sep = ""))

#*******************************************************************************
# Model Config
#*******************************************************************************
source(paste(fd,"04_model_config/00_config_null_model.R", sep = ""))
source(paste(fd,"04_model_config/01_config_logreg.R", sep = ""))
source(paste(fd,"04_model_config/02_config_c50.R", sep = ""))
source(paste(fd,"04_model_config/03_config_rf.R", sep = ""))
source(paste(fd,"04_model_config/04_config_svm.R", sep = ""))
source(paste(fd,"04_model_config/05_config_nb.R", sep = ""))
source(paste(fd,"04_model_config/06_config_knn.R", sep = ""))
