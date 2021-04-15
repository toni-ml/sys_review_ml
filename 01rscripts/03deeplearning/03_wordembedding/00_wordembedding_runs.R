#*******************************************************************************
#*#*******************************************************************************
##################    Pre-training wordembedding - Runs    #####################
#*******************************************************************************
#*******************************************************************************

# pre train a wordembedding on all 2 data sets for: 
# - tiab_pp_stop_stem
# - tiab_pp_lemma_stop

# word embedding
# input_dim: This is the size of the vocabulary in the text data. For example, if your data is integer encoded to values between 0-10, then the size of the vocabulary would be 11 words.
# output_dim: This is the size of the vector space in which words will be embedded. It defines the size of the output vectors from this layer for each word. For example, it could be 32 or 100 or even larger. Test different values for your problem.
# input_length: This is the length of input sequences, as you would define for any input layer of a Keras model. For example, if all of your input documents are comprised of 1000 words, this would be 1000.

#*******************************************************************************

#+++++++++++++++++++++++
tic("total script")
#+++++++++++++++++++++++

#*******************************************************************************
####################    script configuration    ################################
#*******************************************************************************

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# choose parameter for grid
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
n_rev <- length(table(df_data$rev_id))

rev_id <- n_rev
input <- c("tiab")
grid_f <- grid_sl %>% filter(sampling_runs_config == "df_data", 
                             ngram_config == 1, 
                             max_init_tokens_names_config == "org", 
                             weight_tf_tf_idf_names_config == "tf",
                             rev_id_config == 1)
grid_f$input_config <- if_else(grid_f$input_config == "ti_pp_stop", "ti_pp_stem_stop", grid_f$input_config)
grid_f$input_config <- if_else(grid_f$input_config == "tiab_pp_stop", "tiab_pp_stem_stop", grid_f$input_config)
f <- !paste0("wordembedding_", grid_f$input_config, ".h5") %in% list.files(path = "00data/rdata/03deeplearning/03_wordembedding/")
grid_f <- grid_f[f,]

#*******************************************************************************
####################           runs              ###############################
#*******************************************************************************


for (j in 1:nrow(grid_f)) {
  
  #+++++++++++++++++++++++
  # sink I
  #+++++++++++++++++++++++
  sink(
    paste0(
      "00data/rdata/03deeplearning/03_wordembedding/time.txt"
    ), append = TRUE)
  
  #+++++++++++++++++++++++
  # define names
  #+++++++++++++++++++++++
  input_config <- grid_f$input_config[j]
  cat(paste("\n -->", input_config, "\n\n"))
  
  #+++++++++++++++++++++++
  # source
  #+++++++++++++++++++++++
  
  source("01rscripts/03deeplearning/03_wordembedding/01_wordembedding_flag.R")
  
  #+++++++++++++++++++++++
  # sink I
  #+++++++++++++++++++++++
  cat(paste(strrep("_", 40), "\n"))
  sink()
  
}

#+++++++++++++++++++++++
toc()
#+++++++++++++++++++++++

#*******************************************************************************
#*******************************************************************************
##############################      DEBUG     ##################################
#*******************************************************************************
#*******************************************************************************
