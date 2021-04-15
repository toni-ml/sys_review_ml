#*******************************************************************************
#*******************************************************************************
##########################     packages      ###################################
#*******************************************************************************
#*******************************************************************************

needed.packages <- c(
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # data import
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # data transformation
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # tidy universe
  "tidyverse", "magrittr", "rlang",
  # textmining
  "tidytext",
  "textstem",
  
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # modeling
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # machine learning plattform
  "caret", 
  
  # metrics
  "MLmetrics", "ModelMetrics",
  
  # python connection
  "reticulate",
  
  # keras API
  "keras",
  
  # Tensoflow
  "tensorflow",

  # run flags
  "tfruns",
  
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # data visualization
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # programming
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  "tictoc", "filenamer", "doSNOW", "doParallel", "pbapply", "stringr"
  
)
len <- needed.packages %in% installed.packages()[, "Package"]
if (sum(!len) > 0) {
  install.packages(needed.packages[!len], dependencies = TRUE)
}
invisible(lapply(needed.packages, library, character.only = TRUE))
rm(len)
rm(needed.packages)

# # # Use seeds - No multicore use possible
# # use_session_with_seed(seed = 42)
# 
# # specify the cpu cores
# config <- tf$ConfigProto(intra_op_parallelism_threads = 15L,
#                          inter_op_parallelism_threads = 15L
#                          # log_device_placement = TRUE
#                          )
# session = tf$Session(config = config)
# k_set_session(session)
# 
# # test
# const <- tf$constant(42)
# session$run(const)


#*******************************************************************************
#*******************************************************************************
##############################      DEBUG     ##################################
#*******************************************************************************
#*******************************************************************************

# install tensoflow for first time
# devtools::install_github(c("rstudio/reticulate", "rstudio/tfruns", "rstudio/tensorflow", "rstudio/keras", "rstudio/tfdatasets"))
# install_tensorflow()
# mnist <- dataset_mnist()
