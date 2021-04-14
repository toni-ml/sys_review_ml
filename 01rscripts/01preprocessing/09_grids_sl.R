#*******************************************************************************
#*******************************************************************************
#########################     grids data sets      #############################
#*******************************************************************************
#*******************************************************************************

# define grids for ti und tiab

#*******************************************************************************

#+++++++++++++++++++++++
tic("total script: 09_grids_sl.R")
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

load("00data/rdata/01preprocessing/08_data_sampling.RData")
n_rev <- length(table(df_data$rev_id))
#+++++++++++++++++++++++
toc() #load data: 0.945 sec elapsed
#+++++++++++++++++++++++

#*******************************************************************************
##################      define grid for data sets         ######################
#*******************************************************************************

sampling_runs_config <- c("df_data", "df_data_u02", "df_data_u03", "df_data_u04", "df_data_u05")
sampling_runs_names_config <- c("org", "u02", "u03", "u04", "u05")

input_config <- c("tiab_pp_stop", "tiab_pp_lemma_stop")
input_names1_config <- c("tiab","tiab")
input_names2_config <- c("ss","sl")
stemming_names_config <- c(TRUE, FALSE)

ngram_config <- 1:3
ngram_names_config <- c("n1","n2", "n3")
max_init_tokens_config <- c(40000, 15000, 10000, 5000, 1000, 500)
max_init_tokens_names_config <- c("org", "15k", "10k", "05k", "01k", "05h")
weight_tf_tf_idf_config <- c("weightTf", "weightTfIdf")
weight_tf_tf_idf_names_config <- c("tf", "tfidf")




#*******************************************************************************
##################      define grid for data sets (tiab)    ####################
#*******************************************************************************

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# define a grid of parameters for title
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

grid_sl_tiab <- expand.grid(sampling_runs_config = sampling_runs_config, 
                            input_config = input_config, 
                            weight_tf_tf_idf_config = weight_tf_tf_idf_config, 
                            ngram_config = ngram_config, 
                            max_init_tokens_config = max_init_tokens_config)

grid_sl_tiab <- grid_sl_tiab %>% 
  arrange(sampling_runs_config, 
          input_config, 
          weight_tf_tf_idf_config, 
          ngram_config, 
          desc(max_init_tokens_config)) %>%
  as_tibble()

grid_sl_tiab <- 
  grid_sl_tiab %>%
  mutate_if(sapply(grid_sl_tiab, is.factor), as.character) %>% 
  left_join(data.frame(sampling_runs_config, sampling_runs_names_config),
            by = "sampling_runs_config"
  ) %>% 
  left_join(data.frame(max_init_tokens_config, max_init_tokens_names_config),
            by = "max_init_tokens_config"
  ) %>% 
  left_join(data.frame(ngram_config, ngram_names_config),
            by = "ngram_config"
  ) %>% 
  left_join(data.frame(input_config, input_names1_config),
            by = "input_config"
  ) %>% 
  left_join(data.frame(input_config, input_names2_config),
            by = "input_config"
  ) %>% 
  left_join(data.frame(input_config, stemming_names_config),
            by = "input_config"
  ) %>% 
  left_join(data.frame(weight_tf_tf_idf_config, weight_tf_tf_idf_names_config),
            by = "weight_tf_tf_idf_config"
  ) %>% 
  mutate(data_id_config = paste(sampling_runs_names_config, input_names1_config, 
                         weight_tf_tf_idf_names_config, input_names2_config, 
                         ngram_names_config, max_init_tokens_names_config,
                         sep = "_")) 

# glimpse(grid_sl_tiab)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# check max tokens
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

grid_temp <- expand.grid(sampling_runs_config = sampling_runs_config, 
                         input_config = input_config, 
                         rev_id_config = 1:n_rev)
grid_temp %<>% mutate_if(sapply(grid_temp, is.factor), as.character) 

pb <- txtProgressBar(max = nrow(grid_temp), style = 3, width = 100)
for(j in 1:nrow(grid_temp)){
  grid_temp$max_token[j] <- tidyDictionary(
    data = get(grid_temp$sampling_runs_config[j]),
    input = grid_temp$input_config[j], 
    rev_id_conf = grid_temp$rev_id_config[j],
    stemming_conf = !str_detect(grid_temp$input_config[j], "tiab_pp_lemma_stop")
  ) 
  setTxtProgressBar(pb, j)
}
close(pb)

# combin max tokens
grid_sl_tiab <- 
  grid_sl_tiab %>% 
  left_join(grid_temp, 
            by = c("sampling_runs_config", "input_config")
  ) 

# filter to store max tokens to reduce ngrams to max tokens
f <- grid_sl_tiab$max_init_tokens_names_config == "org"
grid_sl_tiab$max_init_tokens_config[f] <- grid_sl_tiab$max_token[f]

# choose org data or limit max tokens
grid_sl_tiab <- grid_sl_tiab %>% 
  filter(max_init_tokens_config <= max_token) %>% 
  mutate(data_id_config = paste("rev", formatC(rev_id_config,width = 2, flag = 0), "_",data_id_config, sep = ""))

#*******************************************************************************
##################      combine grids,     #######################################
#*******************************************************************************

grid_sl <- bind_rows(grid_sl_tiab,.id = "grid") %>% 
  mutate(grid = if_else(grid == 1, "tiab", "zzz" ))

grid_sl <- 
  grid_sl %>% 
  arrange(grid, rev_id_config, desc(max_init_tokens_config)) %>% 
  mutate(grid_id = 1:nrow(grid_sl),
         data_id_config = paste(formatC(grid_id,width = 4, flag = 0), data_id_config ,sep = "_")) %>% 
  select(grid_id, everything())
# View(grid_sl)

#*******************************************************************************
########################      remove objects     ###############################
#*******************************************************************************

rm(list = ls()[ls() %in%
                 c(
                   "pb",
                   "j",
                   "user",
                   "grid_temp",
                   "grid_sl_tiab",
                   "sampling_runs_config",
                   "sampling_runs_names_config",
                   "input_config",
                   "input_names1_config",
                   "input_names2_config",
                   "stemming_names_config",
                   "ngram_config",
                   "ngram_names_config",
                   "max_init_tokens_config",
                   "max_init_tokens_names_config",
                   "weight_tf_tf_idf_config",
                   "weight_tf_tf_idf_names_config"
                 )])

#*******************************************************************************
################################      save    ##################################
#*******************************************************************************

#+++++++++++++++++++++++
tic("save data")
#+++++++++++++++++++++++

save.image("00data/rdata/01preprocessing/09_grid_sl.RData")

#+++++++++++++++++++++++
toc() 
toc()
#+++++++++++++++++++++++

