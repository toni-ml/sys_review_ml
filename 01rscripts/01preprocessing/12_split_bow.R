#*******************************************************************************
#*******************************************************************************
#############################     split      ###################################
#*******************************************************************************
#*******************************************************************************

# save split for each variant of data

#*******************************************************************************

#+++++++++++++++++++++++
tic("total script: 12_split.R")
#+++++++++++++++++++++++

#*******************************************************************************
########################      load data     ####################################
#*******************************************************************************

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# load data
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#+++++++++++++++++++++++
tic("load data")
#+++++++++++++++++++++++

load("00data/rdata/01preprocessing/09_grid_sl.RData")
f <- !paste0("ls_split_", grid_sl$data_id_config, ".RDS") %in% list.files(path = "00data/rdata/01preprocessing/12_split_bow/")
grid_sl <- grid_sl[f,]

#+++++++++++++++++++++++
toc()
#+++++++++++++++++++++++

# #*******************************************************************************
# ##################      save split single core          ########################
# #*******************************************************************************
# 
# # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# # save split
# # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 
# #+++++++++++++++++++++++
# tic("save split")
# #+++++++++++++++++++++++
# pb <- txtProgressBar(max = nrow(grid_sl), style = 3, width = 100)
# for(j in 1:nrow(grid_sl)){
#   store_split(j)
#   setTxtProgressBar(pb, j)
# }
# close(pb)
# #+++++++++++++++++++++++
# toc()
# #+++++++++++++++++++++++

#*******************************************************************************
##################      save split multi core foreach      #####################
#*******************************************************************************

# # use grid_pack for memory use in foreach
# loop_pack <- 100
# # loop_pack <- tidy_cores()
# start <- c(1, 1 + seq(nrow(grid_sl)/loop_pack) * loop_pack)
# end <- c(seq(nrow(grid_sl)/loop_pack) * loop_pack, loop_pack) 
# end[length(end)] <- nrow(grid_sl)
# grid_pack <- data.frame(start, end)
# 
# #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# # save split
# #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# #+++++++++++++++++++++++
# tic("save split")
# #+++++++++++++++++++++++
# 
# for (i in 1:nrow(grid_pack)) {
#   cores <- tidy_cores()
#   cl <- makeCluster(cores, outfile = "", type='SOCK')
#   registerDoSNOW(cl)
# 
#   pb <- txtProgressBar(max = length(grid_pack$start[i]:grid_pack$end[i]), style = 3)
#   progress <- function(n) setTxtProgressBar(pb, n)
#   opts <- list(progress = progress)
# 
#   foreach(
#     j = grid_pack$start[i]:grid_pack$end[i],
#     .options.snow = opts,
#     .packages = c("tidyverse", "tictoc"),
#     .export = c("df_data", "df_data_u02", "df_data_u03", "df_data_u03", "df_data_u04", "df_data_u05")
#   ) %dopar% {
#     store_split(j)
#   }
#   stopCluster(cl)
#   close(pb)
# }
# 
# #+++++++++++++++++++++++
# toc()
# #+++++++++++++++++++++++

#*******************************************************************************
##################      save split multi core parlapplylb      #################
#*******************************************************************************

#+++++++++++++++++++++++
tic("save token")
#+++++++++++++++++++++++
opb <- pboptions(style = 1, char = "=",txt.width=80, use_lb = TRUE)
cores <- tidy_cores()
cl <- makeCluster(cores, outfile = "", type='SOCK')
registerDoSNOW(cl)
clusterExport(cl=cl, varlist=c("grid_sl", "df_data", "df_data_u02", "df_data_u03", "df_data_u03", "df_data_u04", "df_data_u05", "ls_id"), envir=environment())
clusterEvalQ(cl, library(tidyverse))
clusterEvalQ(cl, library(tictoc))

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# save token
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

pblapply(X = 1:nrow(grid_sl), store_split, cl = cl)

stopCluster(cl)
#+++++++++++++++++++++++
toc()
#+++++++++++++++++++++++




#*******************************************************************************
########################      remove objects     ###############################
#*******************************************************************************

rm(list = ls()[ls() %in%
                 c(
                   "loop_pack",
                   "start",
                   "end",
                   "grid_pack"
                 )])

#*******************************************************************************
########################      save image     ###################################
#*******************************************************************************

#+++++++++++++++++++++++
toc()
#+++++++++++++++++++++++
