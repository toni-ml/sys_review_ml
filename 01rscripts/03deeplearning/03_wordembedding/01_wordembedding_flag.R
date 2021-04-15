#*******************************************************************************
#*******************************************************************************
#######################        Flag runs       #################################
#*******************************************************************************
#*******************************************************************************

#+++++++++++++++++++++++
tic("run")
#+++++++++++++++++++++++

#*******************************************************************************
######################     Wordembedding    ####################################
#*******************************************************************************

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# tokenizer
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#+++++++++++++++++++++++
tic("tokenizer")
#+++++++++++++++++++++++

num_words <- tidyDictionary(
  data = df_data, input = grid_f$input_config[j], rev_id_conf = 1:n_rev,
  ngramm_conf = 1, stemming_conf = grid_f$stemming_names_config[j]
)


tokenizer <- text_tokenizer(num_words = num_words)
tokenizer %>% fit_text_tokenizer(df_data[,grid_f$input_config[j]])

#+++++++++++++++++++++++
toc() #load data: 0.706 sec elapsed
#+++++++++++++++++++++++

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# model configuration
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#+++++++++++++++++++++++
tic("Wordembedding")
#+++++++++++++++++++++++

# configurations
embedding_size <- 300  # Dimension of the embedding vector.
skip_window <- 5       # How many words to consider left and right.
num_sampled <- 1       # Number of negative examples to sample for each word.

# placeholder
input_target <- layer_input(shape = 1)
input_context <- layer_input(shape = 1)

# definition of embedding matrix/layer
embedding <- layer_embedding(
  input_dim = tokenizer$num_words +1, # size of the vocabulary in the text data
  output_dim = embedding_size, # size of the output vectors from this layer for each word
  input_length = 1, # This is the length of input sequences (1) for n-gramm model
  name = "word_embedding"
)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# model architecture
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# placeholder for target_vector
target_vector <- input_target %>% 
  embedding() %>% 
  layer_flatten()

# placeholder for context_vector
context_vector <- input_context %>%
  embedding() %>%
  layer_flatten()

# normalization
dot_product <- layer_dot(list(target_vector, context_vector), axes = 1)

# output layer
output <- layer_dense(dot_product, units = 1, activation = "sigmoid")

model <- keras_model(list(input_target, input_context), output)

#+++++++++++++++++++++++
# sink II
#+++++++++++++++++++++++
sink()
sink(
  paste0(
    "00data/rdata/03deeplearning/03_wordembedding/embedidng_architecture.txt"
  ),
  append = TRUE
)
cat(paste("\n -->", input_config, "\n\n"))

summary(model)

sink()

#+++++++++++++++++++++++
# sink I
#+++++++++++++++++++++++
sink(
  paste0(
    "00data/rdata/03deeplearning/03_wordembedding/time.txt"
  ), append = TRUE)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# compile the model
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

model %>% compile(optimizer = "adam",
                  loss = "binary_crossentropy")

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# fit the model
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

model %>%
  fit(workers = 1,
      epochs = 10,
      steps_per_epoch = nrow(df_data),
      skipgrams_generator(text = grid_f$input_config[j], 
                                    tokenizer = tokenizer, 
                                    window_size = skip_window, 
                                    negative_samples = num_sampled)
  )

#+++++++++++++++++++++++
cat(paste("\n\n\n"))
toc()
#+++++++++++++++++++++++

#*******************************************************************************
################################      save    ##################################
#*******************************************************************************

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# save the model
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#+++++++++++++++++++++++
tic("Save model")
#+++++++++++++++++++++++

model %>% save_model_hdf5(paste0("00data/rdata/03deeplearning/03_wordembedding/wordembedding_",grid_f$input_config[j],".h5"))

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# save the model weights into embedding matrix
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

embedding_matrix <- get_weights(model)[[1]]

words <- tibble(
  word = names(tokenizer$word_index),
  id = as.integer(unlist(tokenizer$word_index))
)

words <- words %>%
  filter(id <= tokenizer$num_words) %>%
  arrange(id)

row.names(embedding_matrix) <- c("UNK", words$word)

saveRDS(embedding_matrix, paste0("00data/rdata/03deeplearning/03_wordembedding/wordembedding_matrix_",grid_f$input_config[j], ".RDS"))

#+++++++++++++++++++++++
toc()
#+++++++++++++++++++++++

#*******************************************************************************
#####################      Inspect the wordembedding    ########################
#*******************************************************************************

# #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# # inspect embedding matrix
# #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 
# library(text2vec)
# 
# find_similar_words <- function(word, embedding_matrix, n = 5) {
#   similarities <- embedding_matrix[word, , drop = FALSE] %>%
#     sim2(embedding_matrix, y = ., method = "cosine")
#   
#   similarities[,1] %>% sort(decreasing = TRUE) %>% head(n)
# }
# find_similar_words("ligament", embedding_matrix)
# 
# library(Rtsne)
# library(ggplot2)
# library(plotly)
# 
# 
# 
# tsne <- Rtsne(embedding_matrix[2:500,], perplexity = 50, pca = FALSE)
# 
# tsne_plot <- tsne$Y %>%
#   as.data.frame() %>%
#   mutate(word = row.names(embedding_matrix)[2:500]) %>%
#   ggplot(aes(x = V1, y = V2, label = word)) + 
#   geom_text(size = 3)
# ggplotly(tsne_plot)


#*******************************************************************************
#*******************************************************************************
##############################      DEBUG     ##################################
#*******************************************************************************
#*******************************************************************************
