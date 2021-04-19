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
#***********************        matrix2tibble         **********************************************************************************************************
#***************************************************************************************************************************************************************

#Update Matrix to tible function
matrix2tibble <- function (matrix.object) 
{
  
  matrix.object <- as.matrix(matrix.object)
  
  dat <- data.frame(pub_id = as.numeric(rownames(matrix.object)), matrix.object, 
                    row.names = NULL, stringsAsFactors = FALSE, check.names = TRUE) 
  dat %>% 
    as_tibble()
}


#***************************************************************************************************************************************************************
#***********************        tidyReplaceNA       ************************************************************************************************************
#***************************************************************************************************************************************************************

tidyReplaceNA = function(DT) {
  for (j in seq_len(ncol(DT))){
    data.table::set(DT,which(is.na(DT[[j]])),j,0)
  }
}


#***************************************************************************************************************************************************************
#***********************        Cores         ******************************************************************************************************************
#***************************************************************************************************************************************************************
# use 90 percent of cores. But in case of > 32 core, use only the half
tidy_cores <- function(){
  case_when(detectCores() > 32 ~ floor(detectCores()/2 * 0.90),
            detectCores() <= 32 ~ floor(detectCores() * 0.90),
            TRUE ~ 1)}

#***************************************************************************************************************************************************************
#***********************        tabs         *******************************************************************************************************************
#***************************************************************************************************************************************************************

tab <- function(x) {
  return(
    data.frame(
      abs = table(factor(x, labels = c("inclusion", "exclusion"))),
      rel = round(prop.table(table(factor(x, labels = c("inclusion", "exclusion")))), 2),
      total = length(x)
    )[1, c(1, 5, 2, 4)]
  )
}

#***************************************************************************************************************************************************************
#***********************        SQL         ********************************************************************************************************************
#***************************************************************************************************************************************************************

loadAllData <- function(table) {
  db <- dbConnect(MySQL(),
                  dbname = options()$mysql$databasename, host = options()$mysql$host,
                  port = options()$mysql$port, user = options()$mysql$user,
                  password = options()$mysql$password
  )
  
  query <- paste(sep = "", "SELECT * FROM ", options()$mysql$databasename, ".", table, ";")
  query_send <- dbSendQuery(db, query)
  return(fetch(query_send, n = -1))
  invisible(dbDisconnect(db))
}


#***************************************************************************************************************************************************************
#********************             data cleaning functions   ****************************************************************************************************
#***************************************************************************************************************************************************************

add_point_space <- function(x) {
  gsub("(\\.)([^ ])", "\\1 \\2", x)
}

add_point_slash <- function(x) {
  gsub("/", " ", x)
}

# deleteShortWords <- function(x) gsub(x = x,"\\b\\w{1,2}\\s", " ", perl=T)

#***************************************************************************************************************************************************************
#**********                calculate prevalences  **************************************************************************************************************
#***************************************************************************************************************************************************************

tidy_calc_prev <- function(data) {
  # check how many reviews are in the data for the loop
  n_rev <- length(table(data$rev_id))
  
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # prepare lists
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ls_count_temp <- list(
    total_count = vector(mode = "list", length = n_rev),
    train_count = vector(mode = "list", length = n_rev),
    test_count = vector(mode = "list", length = n_rev),
    train_val_count = vector(mode = "list", length = n_rev),
    val_count = vector(mode = "list", length = n_rev)
  )
  
  ls_count <- vector(mode = "list")
  
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # total counts
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  for (i in 1:n_rev) {
    ls_count_temp$total_count[[i]] <-
      data %>%
      filter(rev_id == i) %>%
      pull(y_label) %>%
      tab()
  }
  
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # train counts
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  for (i in 1:n_rev) {
    ls_count_temp$train_count[[i]] <-
      data %>%
      filter(rev_id == i) %>%
      filter(pub_id %in% ls_id$train_id[[i]]) %>%
      pull(y_label) %>%
      tab()
  }
  
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # test counts
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  for (i in 1:n_rev) {
    ls_count_temp$test_count[[i]] <-
      data %>%
      filter(rev_id == i) %>%
      filter(pub_id %in% ls_id$test_id[[i]]) %>%
      pull(y_label) %>%
      tab()
  }
  
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # train_val counts
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  for (i in 1:n_rev) {
    ls_count_temp$train_val_count[[i]] <-
      data %>%
      filter(rev_id == i) %>%
      filter(pub_id %in% ls_id$train_val_id[[i]]) %>%
      pull(y_label) %>%
      tab()
  }
  
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # val counts
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  for (i in 1:n_rev) {
    ls_count_temp$val_count[[i]] <-
      data %>%
      filter(rev_id == i) %>%
      filter(pub_id %in% ls_id$val_id[[i]]) %>%
      pull(y_label) %>%
      tab()
  }
  
  ls_count$total_count <- bind_rows(ls_count_temp$total_count)
  ls_count$train_count <- bind_rows(ls_count_temp$train_count)
  ls_count$test_count <- bind_rows(ls_count_temp$test_count)
  ls_count$train_val_count <- bind_rows(ls_count_temp$train_val_count)
  ls_count$val_count <- bind_rows(ls_count_temp$val_count)
  
  ls_count$split_ratio <- data.frame(
    data_set = 1:n_rev,
    train_ratio = round(ls_count$train_count$total / ls_count$total_count$total, 2),
    test_ratio = round(ls_count$test_count$total / ls_count$total_count$total, 2),
    train_val_ratio = round(ls_count$train_val_count$total / ls_count$total_count$total, 2),
    val_ratio = round(ls_count$val_count$total / ls_count$total_count$total, 2)
  )
  return(ls_count)
}

#***************************************************************************************************************************************************************
#**************                     tidy_max_tokens          ***************************************************************************************************
#***************************************************************************************************************************************************************

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
#****************        data wrangling   **********************************************************************************************************************
#***************************************************************************************************************************************************************

# - max tokens 
tidyWrangling <- function(data,
                          rev_id_conf = 1,
                          ngramm_conf = 1,
                          input_conf = "ti_pp_stop",
                          stemming_conf = TRUE,
                          max_init_tokens_conf = 500) {
  
  df_max_tokens <-
    data %>%
    filter(rev_id == rev_id_conf) %>%
    select(pub_id, token_var = !!sym(input_conf)) %>%
    {
      if (stemming_conf == TRUE) {
        mutate(., token_var = stem_strings(x = token_var, language = "en"))
      } else {
        .
      }
    } %>%
    unnest_tokens(output = word, input = token_var, token = "ngrams", n = ngramm_conf) %>%
    select(pub_id, word) %>%
    group_by(word) %>%
    summarize(n_to_redurce = n()) %>%
    arrange(desc(n_to_redurce)) %>%
    slice(1:max_init_tokens_conf)
  
  
  #*********************************************
  # build tokens and count token per pub_id
  #*********************************************
  df_data_tidy <-
    data %>%
    filter(rev_id == rev_id_conf) %>%
    select(rev_id, pub_id, token_var = !!sym(input_conf)) %>%
    
    # for stemming_conf == T
    #*********************************************
  {
    if (stemming_conf == TRUE) {
      mutate(., token_var = stem_strings(x = token_var, language = "en"))
    } else {
      .
    }
  } %>% 
      unnest_tokens(., output = word, input = token_var, token = "ngrams", n = ngramm_conf) %>%
        select(., rev_id, pub_id, word) %>%
        left_join(., df_max_tokens, by = "word") %>%
        filter(!is.na(n_to_redurce)) %>%
        select(., rev_id, pub_id, word) %>% 
    group_by(rev_id, pub_id) %>%
    count(word, sort = T)
 

  #   count(word, sort = T)
   
  #*********************************************
  # build total of tokens per pub_id and combine with token per pub_id to calc tf and idf and tf_idf
  #*********************************************
  df_data_tidy %>%
    left_join(df_data_tidy %>%
                group_by(pub_id) %>%
                summarize(total = sum(n)), by = c("pub_id")) %>%
    group_by(pub_id) %>%
    bind_tf_idf(word, pub_id, n) %>%
    arrange(desc(total))
}




#***************************************************************************************************************************************************************
#****************                tidyDataTFIdf     *************************************************************************************************************
#***************************************************************************************************************************************************************

tidyDataTFidf <- function(token, 
                          data, 
                          weight) {
  
  rev_id_conf <- token %>% group_by(rev_id) %>% summarize(n = n()) %>% pull(rev_id)
  
  if(rev_id_conf%%1==0){
    
    if ( weight == "weightTf"){
      
      
      
      dtm <- token %>%
        cast_dtm(document = pub_id, term = word, value = n, weighting = tm::weightTf) %>% 
        matrix2tibble()
      
      f <- dtm$pub_id[!(select(data %>%
                                 filter(rev_id == rev_id_conf), rev_id, pub_id) %>% pull(pub_id)) %in% dtm$pub_id]
      
      if (length(f) > 0){
        dtm_add <- data.frame(matrix(data = vector(mode = "numeric",length = length(f) * ncol(dtm)),nrow = length(f)))
        dtm_add$X1 <- f
        names(dtm_add) <- names(dtm)
        dtm <- bind_rows(dtm,dtm_add
        ) %>% as_tibble()
      }
      
      
      if (any(str_detect(string = "#", colnames(dtm))) == TRUE){
        stop("not unique words")
      }
      
      return(dtm)
    } else
      
      if ( weight == "weightTfIdf"){

        
        dtm <- token %>%
          cast_dtm(document = pub_id, term = word, value = n, weighting = tm::weightTfIdf) %>% 
          matrix2tibble()
        
        f <- dtm$pub_id[!(select(data %>%
                                   filter(rev_id == rev_id_conf), rev_id, pub_id) %>% pull(pub_id)) %in% dtm$pub_id]
        
        if (length(f) > 0){
          dtm_add <- data.frame(matrix(data = vector(mode = "numeric",length = length(f) * ncol(dtm)),nrow = length(f)))
          dtm_add$X1 <- f
          names(dtm_add) <- names(dtm)
          dtm <- bind_rows(dtm,dtm_add) %>% as_tibble()
        }
        
        
        if (any(str_detect(string = "#", colnames(dtm))) == TRUE){
          stop("not unique words")
        }
        
        return(dtm)
      }
    
  } else {
    stop("rev_id don't match in token, consider revising")
  }
  
}


#***************************************************************************************************************************************************************
#***************   store_token   *******************************************************************************************************************************
#***************************************************************************************************************************************************************

store_token <- function(j) {
  #*************************
  tic(paste("store token:", grid_sl$data_id_config[j]))
  #*************************
  
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # tokens
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  token <- tidyWrangling(
    data = get(grid_sl$sampling_runs_config[j]),
    rev_id_conf = grid_sl$rev_id_config[j],
    ngramm_conf = grid_sl$ngram_config[j],
    input_conf = grid_sl$input_config[j],
    stemming_conf = grid_sl$stemming_names_config[j],
    max_init_tokens_conf = grid_sl$max_init_tokens_config[j]
  )
  
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # save token and dtm
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  saveRDS(object = token, file = paste0("00data/rdata/01preprocessing/10_token/token_", 
                                        grid_sl$data_id_config[j], ".RDS"))
  
  rm(token)
  #******************************
  time <- toc(quiet = T)
  write.table(data.frame(token = time$msg, time =  hms::as.hms(time$toc -  time$tic), row.names = NULL), 
              file = "00data/rdata/01preprocessing/10_token/time.csv",
              sep = ";", append = T, col.names = F)
  #******************************
}




#***************************************************************************************************************************************************************
#***************************   store_dtm   *********************************************************************************************************************
#****************************************************************************************************************************************************************

store_dtm <- function(j) {
  #*************************
  tic(paste("store dtm:", grid_sl$data_id_config[j]))
  #*************************
  
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # tokens
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  token <- readRDS(paste0("00data/rdata/01preprocessing/10_token/token_", 
                          grid_sl$data_id_config[j], ".RDS"))
  
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # create dtm
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  dtm <- tidyDataTFidf(token = token, data = get(grid_sl$sampling_runs_config[j]), 
                       weight = grid_sl$weight_tf_tf_idf_config[j])
  
  
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # save token and dtm
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  saveRDS(object = dtm, file = paste0("00data/rdata/01preprocessing/11_dtm/dtm_", 
                                      grid_sl$data_id_config[j], ".RDS"))
  
  rm(token)
  rm(dtm)
  #*************************
  time <- toc(quiet = T)
  write.table(data.frame(token = time$msg, time =  hms::as.hms(time$toc -  time$tic), row.names = NULL), 
              file = "00data/rdata/01preprocessing/11_dtm/time.csv",
              sep = ";", append = T, col.names = F)
  #*************************
}



#***************************************************************************************************************************************************************
#*****   store_split fo conventionel ml  ***********************************************************************************************************************
#***************************************************************************************************************************************************************

store_split <- function(j) {
  #*************************
  tic(paste("store ls_split:", grid_sl$data_id_config[j]))
  #*************************
  rev_f <- grid_sl$rev_id_config[j]  
  ls_split = vector(mode = "list")
  
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # read dtm
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  dtm <- readRDS(paste0("00data/rdata/01preprocessing/11_dtm/dtm_", 
                        grid_sl$data_id_config[j], ".RDS"))
  
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # data Wrangling
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  ls_split$data_dtm <-
    get(grid_sl$sampling_runs_config[j]) %>%
    filter(rev_id == grid_sl$rev_id_config[j]) %>%
    select(pub_id, y_label) %>%
    arrange(pub_id) %>%
    left_join(dtm, "pub_id") %>%
    na.omit()
  rm(dtm)
  
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # training set
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  ls_split$train_dtm <-
    ls_split$data_dtm %>%
    filter(pub_id %in% ls_id$train_id[[rev_f]])
  # ls_split$train_dtm <- ls_split$data_dtm[ls_split$data_dtm$pub_id %in% ls_id$train_id[[grid_sl$rev_id_config[j]]],]
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # test set
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  ls_split$test_dtm <-
    ls_split$data_dtm %>%
    filter(pub_id %in% ls_id$test_id[[rev_f]])
  # ls_split$test_dtm <- ls_split$data_dtm[ls_split$data_dtm$pub_id %in% ls_id$test_id[[grid_sl$rev_id_config[j]]],]
  #*****************************************************************************
  # save split
  #*****************************************************************************
  
  saveRDS(object = ls_split, file = paste0("00data/rdata/01preprocessing/12_split_bow/ls_split_", grid_sl$data_id_config[j], ".RDS"))
  
  rm(ls_split)
  #*************************
  time <- toc(quiet = T)
  write.table(data.frame(token = time$msg, time =  hms::as.hms(time$toc -  time$tic), row.names = NULL), 
              file = "00data/rdata/01preprocessing/12_split_bow/time.csv",
              sep = ";", append = T, col.names = F)
  #*************************
}





#***************************************************************************************************************************************************************
#************   store_split für conventionelle  ****************************************************************************************************************
#***************************************************************************************************************************************************************

store_split_embedding <- function(j) {
  #*************************
  tic(paste("store ls_split:", grid_sl$data_id_config[j]))
  #*************************
  rev_f <- grid_sl$rev_id_config[j]  
  ls_split = vector(mode = "list")
  
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # read dtm
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  dtm <- readRDS(paste0("00data/rdata/01preprocessing/11_dtm/dtm_", 
                        grid_sl$data_id_config[j], ".RDS"))
  
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # data Wrangling
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  ls_split$data_dtm <-
    get(grid_sl$sampling_runs_config[j]) %>%
    filter(rev_id == grid_sl$rev_id_config[j]) %>%
    select(pub_id, y_label) %>%
    arrange(pub_id) %>%
    left_join(dtm, "pub_id") %>%
    na.omit()
  rm(dtm)
  
  
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # training set
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  ls_split$train_dtm <-
    ls_split$data_dtm %>%
    filter(pub_id %in% ls_id$train_val_id[[rev_f]])
  # ls_split$train_dtm <- ls_split$data_dtm[ls_split$data_dtm$pub_id %in% ls_id$train_val_id[[grid_sl$rev_id_config[j]]],]
  
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # validation set
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  ls_split$val_dtm <-
    ls_split$data_dtm %>%
    filter(pub_id %in% ls_id$val_id[[rev_f]])
  
  
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # test set
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  ls_split$test_dtm <-
    ls_split$data_dtm %>%
    filter(pub_id %in% ls_id$test_id[[rev_f]])
  # ls_split$test_dtm <- ls_split$data_dtm[ls_split$data_dtm$pub_id %in% ls_id$test_id[[grid_sl$rev_id_config[j]]],]
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # save split
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  saveRDS(object = ls_split, file = paste0("00data/rdata/01preprocessing/13_split_emb/ls_split_", grid_sl$data_id_config[j], ".RDS"))
  
  rm(ls_split)
  #*************************
  time <- toc(quiet = T)
  write.table(data.frame(token = time$msg, time =  hms::as.hms(time$toc -  time$tic), row.names = NULL), 
              file = "00data/rdata/01preprocessing/13_split_emb/time.csv",
              sep = ";", append = T, col.names = F)
  #*************************
}


