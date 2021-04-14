#*******************************************************************************
#*******************************************************************************
##########################     cleaning      ###################################
#*******************************************************************************
#*******************************************************************************

# clean the text of title and abstract 
# use lemmatazation of text
#*******************************************************************************

#+++++++++++++++++++++++
tic("total script: 06_cleaning.R")
#+++++++++++++++++++++++

#*******************************************************************************
#     load data     ############################################################
#*******************************************************************************

#*******************************************************************************
# load data
#*******************************************************************************

#+++++++++++++++++++++++
tic("load data")
#+++++++++++++++++++++++

load("00data/rdata/01preprocessing/05_remove_add_content.RData")

#+++++++++++++++++++++++
toc()
#+++++++++++++++++++++++

#*******************************************************************************
#    Cleaning Corpus with preprocessing title      #############################
#*******************************************************************************

#+++++++++++++++++++++++
tic("cleaning title")
#+++++++++++++++++++++++

df_data <-
  df_data %>%
  #*******************************************************************************
  # remove internet content
  #*******************************************************************************
  # replace html tags
  #+++++++++++++++++++++++++++++++++++++++++++++++++
  mutate(ti_pp = replace_html(ti_cl)) %>%
  # replace url
  #+++++++++++++++++++++++++++++++++++++++++++++++++
  mutate(ti_pp = replace_url(ti_pp)) %>%
  # replace mails
  #+++++++++++++++++++++++++++++++++++++++++++++++++
  mutate(ti_pp = replace_email(ti_pp)) %>%
  #*******************************************************************************
  # adding content
  #*******************************************************************************
  # add space after comma
  #+++++++++++++++++++++++++++++++++++++++++++++++++
  mutate(ti_pp = add_comma_space(ti_pp)) %>%
  # add space after slash
  #+++++++++++++++++++++++++++++++++++++++++++++++++
  mutate(ti_pp = add_point_slash(ti_pp)) %>%
  # add space after comma
  #+++++++++++++++++++++++++++++++++++++++++++++++++
  mutate(ti_pp = add_point_space(ti_pp)) %>%
  # e.g., "1st" becomes "first"
  #+++++++++++++++++++++++++++++++++++++++++++++++++
  mutate(ti_pp = replace_ordinal(ti_pp)) %>%
  # replace dates
  #+++++++++++++++++++++++++++++++++++++++++++++++++
  mutate(ti_pp = replace_date(ti_pp)) %>%
  # remove numbers, alternative: replace numbers to words 1001 becomes one thousand one
  #+++++++++++++++++++++++++++++++++++++++++++++++++
  mutate(ti_pp = removeNumbers(ti_pp)) %>%  # mutate(ti_pp = replace_number(ti_pp, remove = TRUE)) %>%
  # replace Abkürzungen: what's -> what is
  #+++++++++++++++++++++++++++++++++++++++++++++++++
  mutate(ti_pp = replace_contraction(ti_pp)) %>%
  # replace symbols
  #+++++++++++++++++++++++++++++++++++++++++++++++++
  mutate(ti_pp = replace_symbol(ti_pp, pound = F)) %>%
  # replace ascii
  #+++++++++++++++++++++++++++++++++++++++++++++++++
  mutate(ti_pp = replace_non_ascii(ti_pp)) %>%
  # replace B O M B to BOMB
  #+++++++++++++++++++++++++++++++++++++++++++++++++
  mutate(ti_pp = replace_kern(ti_pp)) %>%
  # Replace Abkürzungen: in. -> inch
  #+++++++++++++++++++++++++++++++++++++++++++++++++
  # mutate(ti_pp = qdap::replace_abbreviation(ti_pp)) %>%
  #*******************************************************************************
  # manipulate content
  #*******************************************************************************
  # Groß - Kleinschrebung
  mutate(ti_pp = tolower(ti_pp)) %>%
  #*******************************************************************************
  # remove contemt
  #*******************************************************************************
  # remove punctuations
  #+++++++++++++++++++++++++++++++++++++++++++++++++
  mutate(ti_pp = removePunctuation(ti_pp,
    preserve_intra_word_contractions = FALSE,
    preserve_intra_word_dashes = FALSE
  )) %>%
  # remove whitespaces
  #+++++++++++++++++++++++++++++++++++++++++++++++++
  mutate(ti_pp = replace_white(ti_pp))

#+++++++++++++++++++++++
toc()
#+++++++++++++++++++++++


#*******************************************************************************
#    Cleaning Corpus with preprocessing Abstract      ##########################
#*******************************************************************************

#+++++++++++++++++++++++
tic("cleaning abstract")
#+++++++++++++++++++++++

df_data <-
  df_data %>%
  #*******************************************************************************
  # remove internet content
  #*******************************************************************************
  # replace html tags
  #+++++++++++++++++++++++++++++++++++++++++++++++++
  mutate(ab_pp = replace_html(ab_cl)) %>%
  # replace url
  #+++++++++++++++++++++++++++++++++++++++++++++++++
  mutate(ab_pp = replace_url(ab_pp)) %>%
  # replace mails
  #+++++++++++++++++++++++++++++++++++++++++++++++++
  mutate(ab_pp = replace_email(ab_pp)) %>%
  #*******************************************************************************
  # adding content
  #*******************************************************************************
  # add space after comma
  #+++++++++++++++++++++++++++++++++++++++++++++++++
  mutate(ab_pp = add_comma_space(ab_pp)) %>%
  # add space after slash
  #+++++++++++++++++++++++++++++++++++++++++++++++++
  mutate(ab_pp = add_point_slash(ab_pp)) %>%
  # add space after point
  #+++++++++++++++++++++++++++++++++++++++++++++++++
  mutate(ab_pp = add_point_space(ab_pp)) %>%
  # e.g., "1st" becomes "first"
  #+++++++++++++++++++++++++++++++++++++++++++++++++
  mutate(ab_pp = replace_ordinal(ab_pp)) %>%
  # replace dates
  #+++++++++++++++++++++++++++++++++++++++++++++++++
  mutate(ab_pp = replace_date(ab_pp)) %>%
  # remove numbers, alternative: replace numbers to words 1001 becomes one thousand one
  #+++++++++++++++++++++++++++++++++++++++++++++++++
  mutate(ab_pp = removeNumbers(ab_pp)) %>%  # mutate(ab_pp = replace_number(ab_pp, remove = TRUE)) %>%
  # replace Abkürzungen: what's -> what is
  #+++++++++++++++++++++++++++++++++++++++++++++++++
  mutate(ab_pp = replace_contraction(ab_pp)) %>%
  # replace symbols
  #+++++++++++++++++++++++++++++++++++++++++++++++++
  mutate(ab_pp = replace_symbol(ab_pp, pound = F)) %>%
  # replace ascii
  #+++++++++++++++++++++++++++++++++++++++++++++++++
  mutate(ab_pp = replace_non_ascii(ab_pp)) %>%
  # replace B O M B to BOMB
  #+++++++++++++++++++++++++++++++++++++++++++++++++
  mutate(ab_pp = replace_kern(ab_pp)) %>%
  # Replace Abkürzungen: in. -> inch
  #+++++++++++++++++++++++++++++++++++++++++++++++++

  # mutate(ab_pp = qdap::replace_abbreviation(ab_pp)) %>%
  #*******************************************************************************
  # manipulate content
  #*******************************************************************************
  # Groß - Kleinschrebung
  #+++++++++++++++++++++++++++++++++++++++++++++++++
  mutate(ab_pp = tolower(ab_pp)) %>%
  #*******************************************************************************
  # remove contemt
  #*******************************************************************************
  # remove punctuations
  #+++++++++++++++++++++++++++++++++++++++++++++++++
  mutate(ab_pp = removePunctuation(ab_pp,
    preserve_intra_word_contractions = FALSE,
    preserve_intra_word_dashes = FALSE
  )) %>%
  # remove whitespaces
  #+++++++++++++++++++++++++++++++++++++++++++++++++
  mutate(ab_pp = replace_white(ab_pp))

#+++++++++++++++++++++++
toc()
#+++++++++++++++++++++++

#*******************************************************************************
######    Combining preprocessed title and abstract      #######################
#*******************************************************************************

df_data %<>%
  unite("tiab_pp", c("ti_pp", "ab_pp"), sep = " ", remove = F) 

#*******************************************************************************
######    lemmatization      ###################################################
#*******************************************************************************

#+++++++++++++++++++++++
tic("lemmatization")
#+++++++++++++++++++++++

df_data <- 
  df_data %>% 
  mutate(ti_pp_lemma = textstem::lemmatize_strings(ti_pp),
         ab_pp_lemma = textstem::lemmatize_strings(ab_pp),
         tiab_pp_lemma = textstem::lemmatize_strings(tiab_pp))

#+++++++++++++++++++++++
toc() # lemmatization: 9.65 sec elapsed
#+++++++++++++++++++++++

# table(df_data$ti_pp == df_data$ti_pp_lemma)
# table(df_data$ab_pp == df_data$ab_pp_lemma)
# table(df_data$tiab_pp == df_data$tiab_pp_lemma)

#*******************************************************************************
######    remove stopwords      ################################################
#*******************************************************************************
#*******************************************************************************
# remove stopwords
#*******************************************************************************

#+++++++++++++++++++++++
tic("remove Stopwords")
#+++++++++++++++++++++++
df_data <- 
  df_data %>% 
  mutate(ti_pp_stop = tm::removeWords(ti_pp,stop_words$word),
         ti_pp_lemma_stop = tm::removeWords(ti_pp_lemma,stop_words$word),
         ab_pp_stop = tm::removeWords(ab_pp,stop_words$word),
         ab_pp_lemma_stop = tm::removeWords(ab_pp_lemma,stop_words$word),
         tiab_pp_stop = tm::removeWords(tiab_pp,stop_words$word),
         tiab_pp_lemma_stop = tm::removeWords(tiab_pp_lemma,stop_words$word))

#+++++++++++++++++++++++
toc() # remove stopwords: 9.65 sec elapsed
#+++++++++++++++++++++++

#*******************************************************************************
######    stemming for DNN      ################################################
#*******************************************************************************
#*******************************************************************************
# word stemming
#*******************************************************************************

df_data <- 
  df_data %>% 
  mutate(ti_pp_stem_stop = stem_strings(x = ti_pp_stop, language = "en"),
       ab_pp_stem_stop = stem_strings(x = ab_pp_stop, language = "en"),
       tiab_pp_stem_stop = stem_strings(x = tiab_pp_stop, language = "en"))


#*******************************************************************************
########################      remove objects     ###############################
#*******************************************************************************

rm(list = ls()[ls() %in% 
                 c(
                   "add_point_slash",
                   "add_point_space"
                 )])

#*******************************************************************************
########################      save image     ###################################
#*******************************************************************************

#+++++++++++++++++++++++
tic("save data")
#+++++++++++++++++++++++

save.image("00data/rdata/01preprocessing/06_cleaning.RData")

#+++++++++++++++++++++++
toc()
toc()
#+++++++++++++++++++++++

