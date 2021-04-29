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
# # # source("01rscripts/04ensemble/00_control.R")

options(max.print = 500, stringsAsFactors = F, expressions = 500000, warning.length = 5000L)

debug <- T # only the first two scenarios will be used
# debug <- F # use all scenarios
#*******************************************************************************
# load data
#*******************************************************************************

load("00data/rdata/01preprocessing/09_grid_sl.RData")

fd <- "01rscripts/04ensemble/"

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
source(paste(fd,"04_model_config/01_pretrained_caretlist.R", sep = ""))
source(paste(fd,"04_model_config/02_pretrained_caretstack.R", sep = ""))
source(paste(fd,"04_model_config/03_pretrained_softvoting.R", sep = ""))
