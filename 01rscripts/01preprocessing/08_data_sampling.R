#*******************************************************************************
#*******************************************************************************
#########################     Sampling data sets      ##########################
#*******************************************************************************
#*******************************************************************************

# downsampling data for the ratios: 02,03,04,05

#*******************************************************************************

#-----------------------
tic("total script: 08_data_sampling.R")
#-----------------------

#*******************************************************************************
########################      load data     ####################################
#*******************************************************************************

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# load data
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#-----------------------
tic("load data")
#-----------------------

load("00data/rdata/01preprocessing/07_data_partitioning.RData")

#-----------------------
toc() #load data: 0.945 sec elapsed
#-----------------------

n_rev <- length(table(df_data$rev_id))

#*******************************************************************************
########################      undersampling   ##################################
#*******************************************************************************

method <- "under"
# df_data %>% filter(rev_id == 1) %>% pull(rf_tas) %>% table(., useNA = "ifany")


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ratio: 0.2 - 20:80 - 1:4 - 0.8/0.2
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

p_ratio <- 0.2

# list
ls_data <- vector(mode = "list", length = 4)

pb <- txtProgressBar(max = n_rev, style = 3, width = 100)
for (i in 1:n_rev) {
  ls_data[[i]] <- ovun.sample(
    formula = y_label ~ .,
    data = df_data,
    method = method,
    p = p_ratio,
    subset = df_data$rev_id == i,
    # na.action = ,
    seed = 42
  )$data %>%
    as_tibble() %>%
    arrange(rev_id, pub_id)
  setTxtProgressBar(pb, i)
}
close(pb)
df_data_u02 <- bind_rows(ls_data) %>% 
  mutate(y_label = factor(y_label, levels = c("inclusion", "exclusion")))

# calc prevalence
ls_counts_u02 <- tidy_calc_prev(df_data_u02)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ratio: 0.3 - 30:70 - 1:2.3 - 0.7/0.3
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

p_ratio <- 0.3 

# list
ls_data <- vector(mode = "list", length = 4)

pb <- txtProgressBar(max = n_rev, style = 3, width = 100)
for (i in 1:n_rev) {
  ls_data[[i]] <- ovun.sample(
    formula = y_label ~ .,
    data = df_data,
    method = method,
    p = p_ratio,
    subset = df_data$rev_id == i,
    # na.action = ,
    seed = 42
  )$data %>%
    as_tibble() %>%
    arrange(rev_id, pub_id)
  setTxtProgressBar(pb, i)
}
close(pb)
df_data_u03 <- bind_rows(ls_data) %>% 
  mutate(y_label = factor(y_label, levels = c("inclusion", "exclusion")))

# calc prevalence
ls_counts_u03 <- tidy_calc_prev(df_data_u03)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ratio: 0.4 - 40:60 - 1:1.5 0.6/0.4
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

p_ratio <- 0.4 

# list
ls_data <- vector(mode = "list", length = 4)

pb <- txtProgressBar(max = n_rev, style = 3, width = 100)
for (i in 1:n_rev) {
  ls_data[[i]] <- ovun.sample(
    formula = y_label ~ .,
    data = df_data,
    method = method,
    p = p_ratio,
    subset = df_data$rev_id == i,
    # na.action = ,
    seed = 42
  )$data %>%
    as_tibble() %>%
    arrange(rev_id, pub_id)
  setTxtProgressBar(pb, i)
}
close(pb)
df_data_u04 <- bind_rows(ls_data) %>% 
  mutate(y_label = factor(y_label, levels = c("inclusion", "exclusion")))

# calc prevalence
ls_counts_u04 <- tidy_calc_prev(df_data_u04)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ratio: 0.5 - 50:50 - 1:1
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
p_ratio <- 0.5


# list
ls_data <- vector(mode = "list", length = 4)

pb <- txtProgressBar(max = n_rev, style = 3, width = 100)
for (i in 1:n_rev) {
  ls_data[[i]] <- ovun.sample(
    formula = y_label ~ .,
    data = df_data,
    method = method,
    p = p_ratio,
    subset = df_data$rev_id == i,
    # na.action = ,
    seed = 42
  )$data %>%
    as_tibble() %>%
    arrange(rev_id, pub_id)
setTxtProgressBar(pb, i)
}
close(pb)
df_data_u05 <- bind_rows(ls_data) %>% 
  mutate(y_label = factor(y_label, levels = c("inclusion", "exclusion")))

# calc prevalence
ls_counts_u05 <- tidy_calc_prev(df_data_u05)



#*******************************************************************************
########################      remove objects     ###############################
#*******************************************************************************

rm(list = ls()[ls() %in%
                 c(
                   "method",
                   "p_ratio",
                   "pb",
                   "i",
                   "tab",
                   "tidy_calc_prev"
                 )])

#*******************************************************************************
################################      save    ##################################
#*******************************************************************************

#-----------------------
tic("save data")
#-----------------------

save.image("00data/rdata/01preprocessing/08_data_sampling.RData")

#-----------------------
toc() 
toc()
#-----------------------

