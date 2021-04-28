#*******************************************************************************
#*******************************************************************************
#######################    data preparation pad sequences    ###################
#*******************************************************************************
#*******************************************************************************


#*******************************************************************************

#+++++++++++++++++++++++
tic("total script")
#+++++++++++++++++++++++

#*******************************************************************************
##################      define grid for data sets     ##########################
#*******************************************************************************
fd_data <- "00data/rdata/03deeplearning/00_pad_seq_bow/"
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# choose parameter for grid
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

n_rev <- length(table(df_data$rev_id))

rev_id <- n_rev
input <- c("tiab")
grid_f <- grid_sl %>% filter(ngram_config == 1, 
                             weight_tf_tf_idf_names_config == "tf")
f <- !paste0("pad_", grid_f$data_id_config, ".RDS") %in% list.files(path = fd_data)
grid_f <- grid_f[f,]


#*******************************************************************************
####################           runs              ###############################
#*******************************************************************************

pb <- txtProgressBar(max = nrow(grid_f), style = 3, width = 100)
for(j in 1:nrow(grid_f)){
  #+++++++++++++++++++++++
  tic("data split")
  #+++++++++++++++++++++++
  
  #name of output file
  output_name <- paste0("pad_", grid_f$data_id_config[j], ".RDS")

  #*******************************************************************************
  ##################          prepare lists        ###############################
  #*******************************************************************************
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # prepare lists
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  # list for word - integer sequences
  ls_x_seq <- vector(mode = "list")
  
  # list of x-data after padding
  ls_split <- vector(mode = "list")

  #*******************************************************************************
  ##################      define token length data sets     ######################
  #*******************************************************************************
    
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # define padding sequences for dtm
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  # define padding length with restriction to max_init_tokens
  ls_split$pad_max_length <- grid_f$max_init_tokens_config[j]
  
  
  # define number of words for corresponding dictionary
  num_words <- tidyDictionary(
    data = df_data, input = grid_f$input_config[j], rev_id_conf = 1:n_rev,
    ngramm_conf = 1, stemming_conf = grid_f$stemming_names_config[j]
  )
 
  #*******************************************************************************
  ###############      initialize tokenzier for padding     ######################
  #*******************************************************************************
  
  #+++++++++++++++++++++++
  tic("tokenizer")
  #+++++++++++++++++++++++
  
  # fitting the tokenizer with text - build the dictonary - same as used for wordembedding
  tokenizer <- text_tokenizer(num_words = num_words)
  tokenizer %>% fit_text_tokenizer(df_data[,grid_f$input_config[j]]) 
  
  # # check for dictonary tokenizer
  # tokenizer$document_count
  # # tokenizer$word_counts %>% head()
  # tokenizer$word_index %>% head()
  
  #+++++++++++++++++++++++
  toc()
  #+++++++++++++++++++++++

  #*******************************************************************************
  ##################      word to integer sequence     ###########################
  #*******************************************************************************
  
  #+++++++++++++++++++++++
  tic("word to text sequences")
  #+++++++++++++++++++++++
  
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # train set - word to text sequence
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  ls_x_seq$train_x_seq <-
    get(grid_f$sampling_runs_config[j]) %>%
    filter(rev_id == grid_f$rev_id_config[j]) %>%
    filter(pub_id %in% ls_id$train_id[[grid_f$rev_id_config[j]]]) %>%
    as.data.frame() %>%
    pull(grid_f$input_config[j]) %>%
    texts_to_sequences(tokenizer = tokenizer)

  # # decode word from index to check tokenizer
  # # +++++++++++++++++++++++
  # paper <- 2
  # glimpse(ls_x_seq$train_x_seq) # train_x_seq %>% head()
  # decode(ls_x_seq$train_x_seq[[paper]])
  # df_data %>%
  #   filter(rev_id == grid_f$rev_id_config[j]) %>%
  #   filter(pub_id %in% ls_id$train_id[[grid_f$rev_id_config[j]]]) %>%
  #   filter(row_number()==paper) %>%
  #   pull(grid_f$input_config[j])

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# test set - word to text sequence
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  ls_x_seq$test_x_seq <-
    # get(grid_f$sampling_runs_config[j]) %>% # hier ist das falsche Ratio daher immer aus df_data für den Testdatensatz
    get("df_data") %>%
    filter(rev_id == grid_f$rev_id_config[j]) %>%
    filter(pub_id %in% ls_id$test_id[[grid_f$rev_id_config[j]]]) %>%
    as.data.frame() %>%
    pull(grid_f$input_config[j]) %>%
    texts_to_sequences(tokenizer = tokenizer)


  # # # decode word from index to check tokenizer
  # # # +++++++++++++++++++++++
  # paper <- 1
  # glimpse(ls_x_seq$test_x_seq)
  # decode(ls_x_seq$test_x_seq[[paper]])
  # df_data %>%
  #   filter(rev_id == grid_f$rev_id_config[j]) %>%
  #   filter(pub_id %in% ls_id$test_id[[grid_f$rev_id_config[j]]]) %>%
  #   filter(row_number()==paper) %>%
  #   pull(grid_f$input_config[j])

  #+++++++++++++++++++++++
  toc()
  #+++++++++++++++++++++++

  #*******************************************************************************
  ##################      word to text sequence     ##############################
  #*******************************************************************************
  
  #+++++++++++++++++++++++
  tic("padding")
  #+++++++++++++++++++++++

  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # train set - padding sequences (build - "dtm")
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  ls_split$train_x <- 
    ls_x_seq$train_x_seq %>%
    pad_sequences(maxlen = ls_split$pad_max_length) 
  
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # test set - padding sequences (build - "dtm")
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  ls_split$test_x <- 
    ls_x_seq$test_x_seq %>%
    pad_sequences(maxlen = ls_split$pad_max_length) 

  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # check dims - padding sequences (build - "dtm")
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  # # check dims
  # print(dim(ls_split$train_x))
  # print(dim(ls_split$test_x))

  #+++++++++++++++++++++++
  toc() 
  #+++++++++++++++++++++++

  #*******************************************************************************
  ##################      word to text sequence     ##############################
  #*******************************************************************************

  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # y_label # reject: 0, accept: 1
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  #+++++++++++++++++++++++
  tic("y_label")
  #+++++++++++++++++++++++

  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # train set - y_label
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  ls_split$train_y <- 
    get(grid_f$sampling_runs_config[j]) %>% 
    filter(rev_id == grid_f$rev_id_config[j]) %>%
    filter(pub_id %in% ls_id$train_id[[grid_f$rev_id_config[j]]]) %>% 
    pull(y_label)

  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # test set - y_label
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  ls_split$test_y <- 
    # get(grid_f$sampling_runs_config[j]) %>% 
    get("df_data") %>% # falsches Ratio sonst, daher immer für test auch aus df_data
    filter(rev_id == grid_f$rev_id_config[j]) %>%
    filter(pub_id %in% ls_id$test_id[[grid_f$rev_id_config[j]]]) %>% 
    pull(y_label) 

  #+++++++++++++++++++++++
  toc() 
  #+++++++++++++++++++++++
  
  #*******************************************************************************
  ##################      Combine to train  as data.frame    #####################
  #*******************************************************************************
  

  ls_split$train <- data.frame(y_label = ls_split$train_y, as.data.frame(ls_split$train_x))
  ls_split$test <- data.frame(y_label = ls_split$test_y, as.data.frame(ls_split$test_x))
  
  
  #*******************************************************************************
  ################################      save    ##################################
  #*******************************************************************************
  
  #+++++++++++++++++++++++
  tic("save data") 
  #+++++++++++++++++++++++
  
  saveRDS(object = ls_split, file = paste0(fd_data, output_name))
  
  #+++++++++++++++++++++++
  toc() 
  toc() 
  #+++++++++++++++++++++++
  setTxtProgressBar(pb, j)
  cat("\n")
}
close(pb)

#+++++++++++++++++++++++
toc() 
#+++++++++++++++++++++++

#*******************************************************************************
########################      remove objects     ###############################
#*******************************************************************************

rm(list = ls()[ls() %in% 
                 c(
                   "tokenizer",
                   "j",
                   "paper",
                   "rev_config",
                   "ls_x_seq"
                 )])

#*******************************************************************************
#*******************************************************************************
##############################      DEBUG     ##################################
#*******************************************************************************
#*******************************************************************************

# # check dims
# #+++++++++++++++++++++++
# 
# for (i in 1:4) {
#     ls_x_seq$train_x_seq[[i]] %>%
#     pad_sequences(maxlen = pad_max_length[i]) %>% dim() %>% print()
# }
