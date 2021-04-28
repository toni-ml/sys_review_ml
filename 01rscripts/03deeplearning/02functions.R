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

#***************************************************************************************************************************************************************
#**********************         store_eval_dat         #*******************************************************************************************************
#***************************************************************************************************************************************************************


store_eval_dat <- function(i){
  #+++++++++++++++++++++++
  # load data
  #+++++++++++++++++++++++
  fit_name <- paste0(ml_model, "_", grid_f$data_id_config[i])
  fit_name_null <- paste0("null_model_", grid_f$data_id_config[i])
  ls_model <- readRDS(paste0(fd_data, ml_model_path, "/",fit_name, ".RDS"))
  ls_model_null <- readRDS(paste0("00data/rdata/02conventional_sl/00_null_model/",fit_name_null, ".RDS"))
  
  # calc average of metrics over folds and calc brier_skill score
  results_temp <- ls_model$fit$resample %>%  
    left_join(
      ls_model_null$fit$resample %>% 
        select(Resample, brier_score_ref = brier_score),
      by = "Resample") %>% 
    mutate(brier_skill_score = (brier_score_ref - brier_score)/brier_score_ref,
           brier_comp_score = brier_score_ref - brier_score,
           net_benefit05 = (sens - (1 - spec)*(0.05/(1-0.05))),
           net_benefit10 = (sens - (1 - spec)*(0.05/(1-0.1))))
  
  
  lvls <- levels(ls_model$test_y_ref)
  
  #+++++++++++++++++++++++
  # overall
  #+++++++++++++++++++++++
  ls_temp_return <- 
    list(
      dat_id = as.character(grid_f$data_id_config[i]),
      rev_id = as.character(grid_f$rev_id_config[i]),
      sampling = as.character(grid_f$sampling_runs_names_config[i]),
      ti_tiab = as.character(grid_f$input_names1_config[i]),
      stem_lemma = as.character(if_else(grid_f$input_names2_config[i] == "ss", "stem", "lemma")),
      weight = as.character(grid_f$weight_tf_tf_idf_names_config[i]),
      ngram = as.character(grid_f$ngram_config[i]),
      max_tokens = as.numeric(grid_f$max_init_tokens_config[i]),
      max_tokens_names = as.character(grid_f$max_init_tokens_names_config[i]),
      
      model_short = as.character(ml_model),
      model_type = as.character(ls_model$fit$modelType),
      model_method = as.character(ls_model$fit$method),
      
      best_tune = ls_model$fit$bestTune,
      
      tune_time = as.character(hms::as_hms(ls_model$fit$times$everything[[3]])),
      
      optim_metric = as.character(ls_model$fit$metric),
      
      cores = times %>% filter(token == paste0(ml_model,"_",grid_f$data_id_config[i], ".RDS")) %>% pull(cores),
      time_run = times %>% filter(token == paste0(ml_model,"_",grid_f$data_id_config[i], ".RDS")) %>% pull(time_run),
      time_load = times %>% filter(token == paste0(ml_model,"_",grid_f$data_id_config[i], ".RDS")) %>% pull(time_load),
      time_fit = times %>% filter(token == paste0(ml_model,"_",grid_f$data_id_config[i], ".RDS")) %>% pull(time_fit),
      time_pred_train = times %>% filter(token == paste0(ml_model,"_",grid_f$data_id_config[i], ".RDS")) %>% pull(time_pred_train),
      time_pred_test = times %>% filter(token == paste0(ml_model,"_",grid_f$data_id_config[i], ".RDS")) %>% pull(time_pred_test),
      time_save = times %>% filter(token == paste0(ml_model,"_",grid_f$data_id_config[i], ".RDS")) %>% pull(time_save),
      
      #+++++++++++++++++++++++
      # results of resamples
      #+++++++++++++++++++++++
      kcross_brier_score = as.numeric(mean(results_temp$brier_score)),
      kcross_brier_score_sd = as.numeric(sd(results_temp$brier_score)),
      kcross_brier_score_ref = as.numeric(mean(results_temp$brier_score_ref)),
      kcross_brier_score_ref_sd = as.numeric(sd(results_temp$brier_score_ref)),
      kcross_brier_skill_score = as.numeric(mean(results_temp$brier_skill_score)),
      kcross_brier_skill_score_sd = as.numeric(sd(results_temp$brier_skill_score)),
      kcross_brier_comp_score = as.numeric(mean(results_temp$brier_comp_score)),
      kcross_brier_comp_score_sd = as.numeric(sd(results_temp$brier_comp_score)),
      
      kcross_net_benefit05 = as.numeric(mean(results_temp$net_benefit05)),
      kcross_net_benefit05_sd = as.numeric(sd(results_temp$net_benefit05)),
      kcross_net_benefit10 = as.numeric(mean(results_temp$net_benefit10)),
      kcross_net_benefit10_sd = as.numeric(sd(results_temp$net_benefit10)),
      
      kcross_acc = as.numeric(mean(results_temp$acc)),
      kcross_acc_sd = as.numeric(sd(results_temp$acc)),
      kcross_auc = as.numeric(mean(results_temp$auc)),
      kcross_auc_sd = as.numeric(sd(results_temp$auc)),
      kcross_sens = as.numeric(mean(results_temp$sens)),
      kcross_sens_sd = as.numeric(sd(results_temp$sens)),
      kcross_spec = as.numeric(mean(results_temp$spec)),
      kcross_spec_sd = as.numeric(sd(results_temp$spec)),
      
      kcross_pos_pred = as.numeric(mean(results_temp$pos_pred)),
      kcross_pos_pred_sd = as.numeric(sd(results_temp$pos_pred)),
      kcross_neg_pred = as.numeric(mean(results_temp$neg_pred)),
      kcross_neg_pred_sd = as.numeric(sd(results_temp$neg_pred)),
      kcross_cl_error = as.numeric(mean(results_temp$cl_error)),
      kcross_cl_error_sd = as.numeric(sd(results_temp$cl_error)),
      kcross_f1 = as.numeric(mean(results_temp$f1)),
      kcross_f1_sd = as.numeric(sd(results_temp$f1)),
      kcross_entropy = as.numeric(mean(results_temp$entropy)),
      kcross_entropy_sd = as.numeric(sd(results_temp$entropy)),
      kcross_gini = as.numeric(mean(results_temp$gini)),
      kcross_gini_sd = as.numeric(sd(results_temp$gini)),
      
      #+++++++++++++++++++++++
      # train_cm
      #+++++++++++++++++++++++
      train_brier_score = as.numeric(mean((ls_model$train_pred_prob[["inclusion"]] - ifelse(ls_model$train_y_ref == lvls[2], 0, 1))^2)),
      train_brier_score_ref = as.numeric(mean((ls_model_null$train_pred_prob[["inclusion"]] - ifelse(ls_model_null$train_y_ref == lvls[2], 0, 1))^2)),
      train_brier_skill_score = as.numeric(1 - mean((ls_model$train_pred_prob[["inclusion"]] - ifelse(ls_model$train_y_ref == lvls[2], 0, 1))^2)/mean((ls_model_null$train_pred_prob[["inclusion"]] - ifelse(ls_model_null$train_y_ref == lvls[2], 0, 1))^2)),
      train_brier_comp_score = as.numeric(mean((ls_model_null$train_pred_prob[["inclusion"]] - ifelse(ls_model_null$train_y_ref == lvls[2], 0, 1))^2) - mean((ls_model$train_pred_prob[["inclusion"]] - ifelse(ls_model$train_y_ref == lvls[2], 0, 1))^2)),
      train_auc =  as.numeric(ModelMetrics::auc(actual = ifelse(ls_model$train_y_ref == lvls[2], 0, 1), predicted = ls_model$train_pred_prob[, lvls[1]])),
      train_cl_error = as.numeric(ModelMetrics::ce(actual = ifelse(ls_model$train_y_ref == lvls[2], 0, 1), predicted = ls_model$train_pred_prob[, "inclusion"])),
      train_entropy = as.numeric(ModelMetrics::logLoss(actual = ifelse(ls_model$train_y_ref == lvls[2], 0, 1), predicted = ls_model$train_pred_prob[, "inclusion"])),
      train_gini = as.numeric(MLmetrics::Gini(y_pred = ls_model$train_pred_prob[, lvls[1]], y_true = ifelse(ls_model$train_y_ref == lvls[1], 1, 0))),
      train_cm_ref_incl_pred_incl = as.numeric(ls_model$train_cm$tabl[[1]]),
      train_cm_ref_incl_pred_excl = as.numeric(ls_model$train_cm$tabl[[2]]),
      train_cm_ref_excl_pred_incl = as.numeric(ls_model$train_cm$tabl[[3]]),
      train_cm_ref_excl_pred_excl = as.numeric(ls_model$train_cm$tabl[[4]]),
      train_accuracy = as.numeric(ls_model$train_cm$overall[[1]]),
      train_kappa = as.numeric(ls_model$train_cm$overall[[2]]),
      train_accuracy_lower = as.numeric(ls_model$train_cm$overall[[3]]),
      train_accuracy_upper = as.numeric(ls_model$train_cm$overall[[4]]),
      train_no_information_rate = as.numeric(ls_model$train_cm$overall[[5]]),
      train_accuracy_pvalue = as.numeric(ls_model$train_cm$overall[[6]]),
      train_mcnemar_pvalue = as.numeric(ls_model$train_cm$overall[[7]]),
      train_sens = as.numeric(ls_model$train_cm$byClass[[1]]),
      train_spec = as.numeric(ls_model$train_cm$byClass[[2]]),
      
      train_net_benefit05 = as.numeric(ls_model$train_cm$byClass[[1]] - ((1-ls_model$train_cm$byClass[[2]])*(0.05/(1-0.05)))),
      train_net_benefit10 = as.numeric(ls_model$train_cm$byClass[[1]] - ((1-ls_model$train_cm$byClass[[2]])*(0.1/(1-0.1)))),
      
      train_pos_pred_val = as.numeric(ls_model$train_cm$byClass[[3]]),
      train_neg_pred_val = as.numeric(ls_model$train_cm$byClass[[4]]),
      train_precision = as.numeric(ls_model$train_cm$byClass[[5]]),
      train_recall = as.numeric(ls_model$train_cm$byClass[[6]]),
      train_f1 = as.numeric(ls_model$train_cm$byClass[[7]]),
      train_prevalence = as.numeric(ls_model$train_cm$byClass[[8]]),
      train_detect_rate = as.numeric(ls_model$train_cm$byClass[[9]]),
      train_detect_prev = as.numeric(ls_model$train_cm$byClass[[10]]),
      train_balanced_acc = as.numeric(ls_model$train_cm$byClass[[11]]),
      
      #+++++++++++++++++++++++
      # test_cm
      #+++++++++++++++++++++++
      test_brier_score = as.numeric(mean((ls_model$test_pred_prob[["inclusion"]] - ifelse(ls_model$test_y_ref == lvls[2], 0, 1))^2)),
      test_brier_score_ref = as.numeric(mean((ls_model_null$test_pred_prob[["inclusion"]] - ifelse(ls_model_null$test_y_ref == lvls[2], 0, 1))^2)),
      test_brier_skill_score = as.numeric(1 - mean((ls_model$test_pred_prob[["inclusion"]] - ifelse(ls_model$test_y_ref == lvls[2], 0, 1))^2)/mean((ls_model_null$test_pred_prob[["inclusion"]] - ifelse(ls_model_null$test_y_ref == lvls[2], 0, 1))^2)),
      test_brier_comp_score = as.numeric(mean((ls_model_null$test_pred_prob[["inclusion"]] - ifelse(ls_model_null$test_y_ref == lvls[2], 0, 1))^2) - mean((ls_model$test_pred_prob[["inclusion"]] - ifelse(ls_model$test_y_ref == lvls[2], 0, 1))^2)),
      test_auc = as.numeric(ModelMetrics::auc(actual = ifelse(ls_model$test_y_ref == lvls[2], 0, 1), predicted = ls_model$test_pred_prob[, lvls[1]])),
      test_cl_error = as.numeric(ModelMetrics::ce(actual = ifelse(ls_model$test_y_ref == lvls[2], 0, 1), predicted = ls_model$test_pred_prob[, "inclusion"])),
      test_entropy = as.numeric(ModelMetrics::logLoss(actual = ifelse(ls_model$test_y_ref == lvls[2], 0, 1), predicted = ls_model$test_pred_prob[, "inclusion"])),
      test_gini = as.numeric(MLmetrics::Gini(y_pred = ls_model$test_pred_prob[, lvls[1]], y_true = ifelse(ls_model$test_y_ref == lvls[1], 1, 0))),
      test_cm_ref_incl_pred_incl = as.numeric(ls_model$test_cm$tabl[[1]]),
      test_cm_ref_incl_pred_excl = as.numeric(ls_model$test_cm$tabl[[2]]),
      test_cm_ref_excl_pred_incl = as.numeric(ls_model$test_cm$tabl[[3]]),
      test_cm_ref_excl_pred_excl = as.numeric(ls_model$test_cm$tabl[[4]]),
      test_accuracy = as.numeric(ls_model$test_cm$overall[[1]]),
      test_kappa = as.numeric(ls_model$test_cm$overall[[2]]),
      test_accuracy_lower = as.numeric(ls_model$test_cm$overall[[3]]),
      test_accuracy_upper = as.numeric(ls_model$test_cm$overall[[4]]),
      test_no_information_rate = as.numeric(ls_model$test_cm$overall[[5]]),
      test_accuracy_pvalue = as.numeric(ls_model$test_cm$overall[[6]]),
      test_mcnemar_pvalue = as.numeric(ls_model$test_cm$overall[[7]]),
      test_sens = as.numeric(ls_model$test_cm$byClass[[1]]),
      test_spec = as.numeric(ls_model$test_cm$byClass[[2]]),
      
      test_net_benefit05 = as.numeric(ls_model$test_cm$byClass[[1]] - ((1-ls_model$test_cm$byClass[[2]])*(0.05/(1-0.05)))),
      est_net_benefit10 = as.numeric(ls_model$test_cm$byClass[[1]] - ((1-ls_model$test_cm$byClass[[2]])*(0.1/(1-0.1)))),
      
      test_pos_pred_val = as.numeric(ls_model$test_cm$byClass[[3]]),
      test_neg_pred_val = as.numeric(ls_model$test_cm$byClass[[4]]),
      test_precision = as.numeric(ls_model$test_cm$byClass[[5]]),
      test_recall = as.numeric(ls_model$test_cm$byClass[[6]]),
      test_f1 = as.numeric(ls_model$test_cm$byClass[[7]]),
      test_prevalence = as.numeric(ls_model$test_cm$byClass[[8]]),
      test_detect_rate = as.numeric(ls_model$test_cm$byClass[[9]]),
      test_detect_prev = as.numeric(ls_model$test_cm$byClass[[10]]),
      test_balanced_acc = as.numeric(ls_model$test_cm$byClass[[11]])
      
    ) %>% as.data.frame()
  names(ls_temp_return) <- str_replace_all(names(ls_temp_return), pattern = "tune.", replacement = "tune_")
  
  return(ls_temp_return)
}




