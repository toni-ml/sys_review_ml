#*******************************************************************************
#*******************************************************************************
##########################     functions      ##################################
#*******************************************************************************
#*******************************************************************************


#***************************************************************************************************************************************************************
#***********************        Timing         *****************************************************************************************************************
#***************************************************************************************************************************************************************

# Update tic toc pacakge
toc.outmsg <- function (tic, toc, msg) {
  if (is.null(msg) || is.na(msg) || length(msg) == 0) 
    outmsg <- paste(hms::as_hms(toc - tic), " processing time", 
                    sep = "")
  else outmsg <- paste(msg, ": ", hms::as_hms(toc - tic), " processing time", 
                       sep = "")
}

assignInNamespace(x = "toc.outmsg", value = toc.outmsg,ns = "tictoc")

#***************************************************************************************************************************************************************
#***********************        Cores         ******************************************************************************************************************
#***************************************************************************************************************************************************************
# use 90 percent of cores. But in case of > 32 core, use only the half
tidy_cores <- function(){
  case_when(detectCores() > 32 ~ floor(detectCores()/2 * 0.90),
            detectCores() <= 32 ~ floor(detectCores() * 0.90),
            TRUE ~ 1)}

#***************************************************************************************************************************************************************
#*******************        text decoding to check padding         #********************************************************************************************
#***************************************************************************************************************************************************************

decode <- function(dat) {
  word_index <- tokenizer$word_index
  reverse_word_index <- names(word_index)
  names(reverse_word_index) <- word_index
  cat(
    sapply(dat, function(index) {
      word <- reverse_word_index[[as.character(index)]]
      if (!is.null(word)) word else "?"
    })
  )
}

#***************************************************************************************************************************************************************
#****************        Preparing dictionaries         #*******************************************************************************************************
#***************************************************************************************************************************************************************

# tidyDictionary <- function(data, ngramm = 1, input, stopword = TRUE, stemming = TRUE) {
#   df_data_tidy <-
#     data %>%
#     unnest_tokens(output = word, input = !!sym(input), token = "ngrams", n = ngramm) %>%
#     {
#       if (stopword == TRUE & ngramm == 1) {
#         anti_join(., stop_words, by = "word")
#       } else {
#         .
#       }
#     } %>%
#     {
#       if (stopword == TRUE & ngramm == 2) {
#         separate(., word, c("word1", "word2"), sep = " ") %>%
#           filter(., !word1 %in% stop_words$word) %>%
#           filter(., !word2 %in% stop_words$word) %>%
#           unite(., word, word1, word2, sep = " ")
#       } else {
#         .
#       }
#     } %>%
#     {
#       if (stopword == TRUE & ngramm == 3) {
#         separate(., word, c("word1", "word2", "word3"), sep = " ") %>%
#           filter(., !word1 %in% stop_words$word) %>%
#           filter(., !word2 %in% stop_words$word) %>%
#           filter(., !word3 %in% stop_words$word) %>%
#           unite(., word, word1, word2, word3, sep = " ")
#       } else {
#         .
#       }
#     } %>%
#     {
#       if (stemming == TRUE) {
#         mutate(., word = wordStem(words = word, language = "en"))
#       } else {
#         .
#       }
#     } %>%
#     select(word) %>%
#     distinct(word) %>%
#     count(word, sort = T)
# }


tidyDictionary <- function(data,
                           input,
                           ngramm_conf = 1,
                           rev_id_conf,
                           stemming_conf = F) {
  data %>% filter(rev_id %in% rev_id_conf) %>%
    select(pub_id, token_var = !!sym(input)) %>%
    {
      if (stemming_conf == TRUE) {
        mutate(., token_var = stem_strings(x = token_var, language = "en"))
      } else {
        .
      }
    } %>%
    unnest_tokens(output = word, input = token_var, token = "ngrams", n = ngramm_conf) %>%
    select(word) %>%
    distinct(word) %>%
    summarize(n = n()) %>%
    pull(n)
}

#***************************************************************************************************************************************************************
#****************        seed function         #****************************************************************************************************************
#***************************************************************************************************************************************************************

setSeedsRepeatedCV <- function(numbers = 1, repeats = 1, tunes = NULL, seed = 42) {
  # list_length is the number of resamples and integer vector of M (numbers + tune length if any)
  list_length <- numbers * repeats
  
  if (is.null(list_length)) {
    seeds <- NULL
  } else {
    set.seed(seed = seed)
    seeds <- vector(mode = "list", length = list_length)
    seeds <- lapply(seeds, function(x) sample.int(n = 1000000, size = numbers + ifelse(is.null(tunes), 0, tunes)))
    seeds[[length(seeds) + 1]] <- sample.int(n = 1000000, size = 1)
  }
  # return seeds
  seeds
}


#***************************************************************************************************************************************************************
#********************        twoClassSummaryCustom         #****************************************************************************************************
#***************************************************************************************************************************************************************

twoClassSummaryCustom <- function(data, lev = NULL, model = NULL) {
  lvls <- levels(data$obs)
  if (length(lvls) > 2) {
    stop(paste("Your outcome has", length(lvls), "levels. The twoClassSummary() function isn't appropriate."))
  }
  if (!all(levels(data[, "pred"]) == lvls)) {
    stop("levels of observed and predicted data do not match")
  }
  if (!all(lvls == c("inclusion", "exclusion")) == TRUE) {
    stop("please use inclusion as the reference and not exclusion") # daher ist:
    # lvls[1] == "inclusion" und lvls[2] == "exclusion
  }
  
  out <- c(
    mean((data[, "inclusion"] - ifelse(data$obs == lvls[2], 0, 1))^2),
    # ModelMetrics::brier(actual = ifelse(data$obs == lvls[2], 0, 1), predicted = data[, "inclusion"]),
    MLmetrics::Accuracy(y_pred = data[, lvls[1]], y_true = ifelse(data$obs == lvls[1], 1, 0)),
    ModelMetrics::auc(actual = ifelse(data$obs == lvls[2], 0, 1), predicted = data[, lvls[1]]),
    caret::sensitivity(data[, "pred"], data[, "obs"], positive = lvls[1]),
    caret::specificity(data[, "pred"], data[, "obs"], negative = lvls[2]),
    # caret::posPredValue(data[, "pred"], data[, "obs"], positive = lvls[1]),
    ModelMetrics::ppv(actual = ifelse(data$obs == lvls[2], 0, 1), predicted = data[, "inclusion"]),
    # caret::negPredValue(data[, "pred"], data[, "obs"], negative = lvls[2]),
    ModelMetrics::npv(actual = ifelse(data$obs == lvls[2], 0, 1), predicted = data[, "inclusion"]),
    ModelMetrics::ce(actual = ifelse(data$obs == lvls[2], 0, 1), predicted = data[, "inclusion"]),
    ModelMetrics::f1Score(actual = ifelse(data$obs == lvls[2], 0, 1), predicted = data[, "inclusion"]),
    ModelMetrics::logLoss(actual = ifelse(data$obs == lvls[2], 0, 1), predicted = data[, "inclusion"]),
    MLmetrics::Gini(y_pred = data[, lvls[1]], y_true = ifelse(data$obs == lvls[1], 1, 0))
  )
  
  names(out) <- c("brier_score", "acc", "auc", "sens", "spec", "pos_pred", 
                  "neg_pred", "cl_error", "f1", "entropy", "gini"
  )
  out
}

#***************************************************************************************************************************************************************
#***************************        Wordembediing via Skipgram-Generator        #*******************************************************************************
#*#***************************************************************************************************************************************************************

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# skipgrams_generator - proposed code
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# skipgrams_generator <- function(text, tokenizer, window_size, negative_samples) {
#   gen <- texts_to_sequences_generator(tokenizer, sample(text))
#   function() {
#     skip <- iter_next(gen) %>%
#       skipgrams(
#         vocabulary_size = tokenizer$num_words,
#         window_size = window_size,
#         negative_samples = 1
#       )
#     x <- transpose(skip$couples) %>% map(. %>% unlist %>% as.matrix(ncol = 1))
#     y <- skip$labels %>% as.matrix(ncol = 1)
#     list(x, y)
#   }
# }

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# skipgrams_generator - fixed Code (running out of bound)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# # source:
# https://gist.github.com/jnolis/e23f00d752b47671c9f0feccd333e1d4

skipgrams_generator <- function(text, tokenizer, window_size, negative_samples) {
  gen <- texts_to_sequences_generator(tokenizer, sample(text))
  function() {
    next_value <- generator_next(gen)
    if (is.null(next_value)) { # if there isn't new text from the generator
      gen <<- texts_to_sequences_generator(tokenizer, sample(text)) # remake the generator
      next_value <- generator_next(gen)
    }
    skip <- next_value %>%
      skipgrams(
        vocabulary_size = tokenizer$num_words,
        window_size = window_size,
        negative_samples = 1
      )
    x <- transpose(skip$couples) %>% map(. %>% unlist() %>% as.matrix(ncol = 1))
    y <- skip$labels %>% as.matrix(ncol = 1)
    list(x, y)
  }
}


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# tensoflow - keras custom metric
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

sensitivity <- custom_metric("sensitivity", function(y_true, y_pred) {
  
  true_positives = k_sum(k_round(k_clip(y_true * y_pred, 0, 1)))
  possible_positives = k_sum(k_round(k_clip(y_true, 0, 1)))
  sens = true_positives / (possible_positives + k_epsilon())
  return(sens)
  
})

specificity <- custom_metric("specificity", function(y_true, y_pred) {

  true_negatives = k_sum(k_round(k_clip((1-y_true) * (1-y_pred), 0, 1)))
  possible_negatives = k_sum(k_round(k_clip(1-y_true, 0, 1)))
  spec = true_negatives / (possible_negatives + k_epsilon())
  return(spec)
})


# specificity <- custom_metric("specificity", function(y_true, y_pred) {
#   neg_y_true = 1 - y_true
#   neg_y_pred = 1 - y_pred
#   fp = k_sum(neg_y_true * y_pred)
#   tn = k_sum(neg_y_true * neg_y_pred)
#   spec = tn / (tn + fp + k_epsilon())
#   return(spec)
# })
# Accuracy = (sensitivity) (prevalence) + (specificity) (1 - prevalence)
# (0.4091 * 0.4) + (0.1364 *(1-0.4)) # metricen hauen nicht hin
# (0.36 * 0.4) + (0.27 *(1-0.4)) # metricen hauen nicht hin


# specificity <- custom_metric("specificity", function(y_true, y_pred) {
# 
  # neg_y_true = 1 - y_true
  # neg_y_pred = 1 - y_pred
  # fp = k_sum(neg_y_true * y_pred)
  # tn = k_sum(neg_y_true * neg_y_pred)
  # spec = tn / (tn + fp + k_epsilon())
  # return(spec)
# })

# specificity_loss <- function(y_true, y_pred) {
#   
#   neg_y_true = 1 - y_true
#   neg_y_pred = 1 - y_pred
#   fp = k_sum(neg_y_true * y_pred)
#   tn = k_sum(neg_y_true * neg_y_pred)
#   spec = tn / (tn + fp + k_epsilon())
#   return(1-spec)
#   
# }
# 
# 
# sensitivity_loss <- function(y_true, y_pred) {
#   
#   tp = k_sum(y_true * y_pred)
#   p = k_sum(y_true)
#   sens = tp / (p + k_epsilon())
#   return(1-sens)
#   
# }

