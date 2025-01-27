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
  "tidyverse", "magrittr", "rlang", "stringr",

  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # modeling
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # machine learning plattform
  "caret", "caretEnsemble", 
  
  # metrics
  "MLmetrics", "ModelMetrics",
  
  # random forest
  "ranger",
  
  # naive bayes
  "e1071",
  "class",
  
  # C5.0
  "C50",
  
  # glmnet
  "glmnet",
  
  # svm
  "LiblineaR",
  
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # data visualization
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  "yarrr", "pROC", "plotROC", 
  "lime", "grid", "gridExtra",
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # programming
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  "tictoc", "filenamer", "doSNOW", "doParallel", "pbapply"
  
)
len <- needed.packages %in% installed.packages()[, "Package"]
if (sum(!len) > 0) {
  install.packages(needed.packages[!len], dependencies = TRUE)
}
invisible(lapply(needed.packages, library, character.only = TRUE))
rm(len)
rm(needed.packages)


