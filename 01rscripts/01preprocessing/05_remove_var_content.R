#*******************************************************************************
#*******************************************************************************
##########################     remove_add_content     ##########################
#*******************************************************************************
#*******************************************************************************

# remove additional content

#*******************************************************************************

#+++++++++++++++++++++++
tic("total script: 05_remove_var_content.R")
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

load("00data/rdata/01preprocessing/03_data_import.RData")

#+++++++++++++++++++++++
toc()
#+++++++++++++++++++++++

#*******************************************************************************
#######################    Prepare data      ###################################
#*******************************************************************************

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# set encoding
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

df_data <-
  df_data %>%
  mutate(
    title = enc2utf8(title),
    abstract = enc2utf8(abstract)
  )


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# replace empty entries with xxx in title
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# -> NA und Jahres zahlen raus.
df_data$title[str_length(df_data$title) == 4] <- "xxx"
df_data$title[df_data$title == ""] <- "xxx"

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# replace empty entries with xxx in abstract
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# prepare empty abstracts
df_data$abstract[which(df_data$abstract == "")] <- "xxx"

#*******************************************************************************
#######################    Prepare title      ##################################
#*******************************************************************************

#+++++++++++++++++++++++
tic("title preparation")
#+++++++++++++++++++++++

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Delete [countries] oder [additional titles] from the title
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# delete content with pattern: . [?  OR .[?
#+++++++++++++++++++++++++++++++++++++++++++++++++
df_data %<>%
  mutate(ti_cl = str_replace(string = title, pattern = "(\\. \\[.+)|(\\.\\[.+)", replacement = "")) %>%
  # delete content with pattern: ] [? OR ][? 
  #+++++++++++++++++++++++++++++++++++++++++++++++++
  mutate(ti_cl = str_replace(ti_cl, "(\\] \\[.+)|(\\]\\[.+)", replacement = "")) %>% 
  # delete content with pattern: ) [? OR )[? 
  #+++++++++++++++++++++++++++++++++++++++++++++++++
  mutate(ti_cl = str_replace(.$ti_cl, "(\\) \\[.+)|(\\)\\[.+)", replacement = "" ))
  
# delete content with pattern: ^[a-zA-Z].*(\\[.*\\]$) --> start with text and end with [?] or [?].
#+++++++++++++++++++++++++++++++++++++++++++++++++
filter <- 
  df_data %>% 
  pull(ti_cl) %>% 
  grep(pattern = "(^[a-zA-Z].*(\\[.*\\]$))|(^[a-zA-Z].*(\\[.*\\]\\.$))")
df_data$ti_cl[filter] <- str_replace(df_data$ti_cl[filter], "\\[.+\\]", replacement = "" )


df_data %<>%
  # delete content with ...includ
  #+++++++++++++++++++++++++++++++++++++++++++++++++
  mutate(ti_cl = str_replace(.$ti_cl, "\\.\\.\\.includ.*", replacement = "" )) %>% 
  # delete content with (Provisional abstract)|(Structured abstract)
  #+++++++++++++++++++++++++++++++++++++++++++++++++
  mutate(ti_cl = str_replace(string = .$ti_cl, pattern = "(Provisional abstract)|(Structured abstract)",replacement = ""))

# delete content with abstract
#+++++++++++++++++++++++++++++++++++++++++++++++++
filter <- 
  df_data %>% 
  pull(ti_cl) %>% 
  grep(pattern = "Abstract")
df_data$ti_cl[filter] <- "xxx"

df_data %<>%
  # delete content with [corrected
  #+++++++++++++++++++++++++++++++++++++++++++++++++
  mutate(ti_cl = str_replace(string = .$ti_cl, pattern = "\\[corrected$",replacement = "")) %>% 
  # delete content with [1] or [2]
  #+++++++++++++++++++++++++++++++++++++++++++++++++
  mutate(ti_cl = str_replace(string = .$ti_cl, pattern = "(\\[1\\].*)|(\\[2\\].*)",replacement = "")) %>% 
  # delete content with ... presented
  #+++++++++++++++++++++++++++++++++++++++++++++++++
  mutate(ti_cl = str_replace(.$ti_cl, "\\.\\.\\. presented.*", replacement = "" )) %>% 
  # delete content with ... including
  #+++++++++++++++++++++++++++++++++++++++++++++++++
  mutate(ti_cl = str_replace(.$ti_cl, "\\.\\.\\. including.*", replacement = "" )) %>% 
  # delete content with ...reprinted
  #+++++++++++++++++++++++++++++++++++++++++++++++++

  mutate(ti_cl = str_replace(.$ti_cl, "\\.\\.\\.reprinted.*", replacement = "" )) %>% 
  # delete content with Knee Surg...
  #+++++++++++++++++++++++++++++++++++++++++++++++++
  mutate(ti_cl = str_replace(.$ti_cl, "\\.\\.\\.Knee.*", replacement = "" )) %>% 
  # delete content with Procedings
  #+++++++++++++++++++++++++++++++++++++++++++++++++
  mutate(ti_cl = str_replace(.$ti_cl, "\\.\\.\\.Proceedings.*", replacement = "" )) %>% 
  # delete content with multiple letters
  #+++++++++++++++++++++++++++++++++++++++++++++++++
  mutate(ti_cl = str_replace(.$ti_cl, "multiple letters", replacement = "" ))

#+++++++++++++++++++++++
toc()
#+++++++++++++++++++++++

#*******************************************************************************
####################    Prepare abstract      ##################################
#*******************************************************************************

#+++++++++++++++++++++++
tic("abstract preparation")
#+++++++++++++++++++++++

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Delete [countries] oder [additional titles] from the abstract
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# delete references
#+++++++++++++++++++++++++++++++++++++++++++++++++
df_data <-
  df_data %>%
  mutate(ab_cl = str_replace(string = abstract, pattern = "\\[Ref.+\\]", replacement = "")) %>%

  # delete copyrights
  #+++++++++++++++++++++++++++++++++++++++++++++++++
  mutate(ab_cl = str_replace(string = ab_cl, pattern = "Copyright.+", replacement = "")) %>%

  # delete Trial registration
  #+++++++++++++++++++++++++++++++++++++++++++++++++
  mutate(ab_cl = str_replace(string = ab_cl, pattern = "(TRIAL REGISTRATION.+)|(TRIAL REGISTRATION.+)|(Trial registration.+)", replacement = "")) %>%

  # delete licensee
  #+++++++++++++++++++++++++++++++++++++++++++++++++
  mutate(ab_cl = str_replace(string = ab_cl, pattern = "licensee.+", replacement = "")) %>%

  # delete Elsevier, Springer, Wiley., BMJ, Oxford, BioMed Central
  #+++++++++++++++++++++++++++++++++++++++++++++++++
  mutate(ab_cl = str_replace(string = ab_cl, pattern = "(Wiley.+)|(Springer.+)|(Elsevier.+)|(BMJ Publish.+)|(Oxford University.+)|(BioMed Central.+)", replacement = "")) %>%

  # delete \\[English Summary\\]
  #+++++++++++++++++++++++++++++++++++++++++++++++++
  mutate(ab_cl = str_replace(string = ab_cl, pattern = "\\[English Summary\\]", replacement = "")) %>%

  # delete \\(ABSTRACT TRUNCATED\\)
  #+++++++++++++++++++++++++++++++++++++++++++++++++
  mutate(ab_cl = str_replace(string = ab_cl, pattern = "\\(ABSTRACT TRUNCATED.+", replacement = "")) %>%

  # delete "¶" - different Versions for Windows and unix
  #+++++++++++++++++++++++++++++++++++++++++++++++++
  # windows
  # mutate(ab_cl = str_replace(string = ab_cl, pattern = "¶.+",replacement = "")) %>%
  # unix
  # mutate(ab_cl = str_replace(string = ab_cl, pattern = "\266\270.+", replacement = "")) %>%
  {
    if (.Platform$OS.type == "windows") {
      mutate(., ab_cl = str_replace(string = ab_cl, pattern = "¶.+", replacement = ""))
    } else {
      .
    }
  } %>%
  {
    if (.Platform$OS.type == "unix") {
      mutate(., ab_cl = str_replace(string = ab_cl, pattern = "\266\270.+", replacement = ""))
    } else {
      .
    }
  } %>%

  # delete \\(Table presented\\)
  #+++++++++++++++++++++++++++++++++++++++++++++++++
  mutate(ab_cl = str_replace(string = ab_cl, pattern = "\\(Table presented\\)", replacement = "")) %>%

  # delete copyright symbol
  #+++++++++++++++++++++++++++++++++++++++++++++++++
  mutate(ab_cl = str_replace(string = ab_cl, pattern = "(\\(c\\))|(\\(C\\))|(©) \\d{4}.*", replacement = "")) %>%

  # delete funded by
  #+++++++++++++++++++++++++++++++++++++++++++++++++
  mutate(ab_cl = str_replace(string = ab_cl, pattern = "Funded by.*", replacement = "")) %>%
  mutate(ab_cl = str_replace(string = ab_cl, pattern = "Funding", replacement = "")) %>%

  # delete level of evidence
  #+++++++++++++++++++++++++++++++++++++++++++++++++  

  mutate(ab_cl = str_replace(string = ab_cl, pattern = "\\: Level I(?!( trauma)|( Trauma)|( urban)).*", replacement = "")) %>%
  mutate(ab_cl = str_replace(string = ab_cl, pattern = "((Therapeutic)|(Diagnostic)|(Prognostic))(?= Level I).*", replacement = "")) %>%
  mutate(ab_cl = str_replace(string = ab_cl, pattern = "(Level I-1b)|(Level I\\.)|(Level II\\.)|(Level III\\.)|(Level IV\\.).*", replacement = "")) %>%
  mutate(ab_cl = str_replace(string = ab_cl, pattern = "(Level I, therapeutic.*)|(Level II, therapeutic.*)|(Level III, therapeutic.*)|(Level IV, therapeutic.*)", replacement = "")) %>%
  mutate(ab_cl = str_replace(string = ab_cl, pattern = "((Therapeutic)|(Diagnostic)|(Prognostic))(?= study, Level I).*", replacement = "")) %>%
  mutate(ab_cl = str_replace(string = ab_cl, pattern = "(LEVEL OF EVIDENCE)(?!( III\\: 34)).*", replacement = "")) %>%
  mutate(ab_cl = str_replace(string = ab_cl, pattern = "((Level of evidence)|(Level of Evidence))(?!((( 1)|( 2)|( 3)|( 4)|( of)|( I))|(, ((1)|(2)|(3)|(4))))).*", replacement = "")) %>%
  mutate(ab_cl = str_replace(string = ab_cl, pattern = "(Level of evidence 1\\.)|(Level of evidence 2\\.)|(Level of evidence 3\\.)|(Level of evidence 4\\.)|(Level of evidence, 1\\.)|(Level of evidence, 2\\.)|(Level of evidence, 3\\.)|(Level of evidence, 4\\.)", replacement = "")) %>%
  mutate(ab_cl = str_replace(string = ab_cl, pattern = "(Level of Evidence III.*)|(Level of evidence III.*)|(Level of evidence IV.*)", replacement = "")) %>%
  mutate(ab_cl = str_replace(string = ab_cl, pattern = "(Levels of Evidence.*)|(Levels of evidence.*)|(Level Of Evidence.*)|(Level of Evidence II\\.*)|(Level of evidence II\\.*)|(Level of evidence III\\.*)|(Level of evidence IV\\.*)", replacement = ""))

df_data %<>% 
  # replace a.c.l and p.c.l
  #+++++++++++++++++++++++++++++++++++++++++++++++++    
  mutate(ab_cl = str_replace_all(string = ab_cl, pattern = "(A\\.C\\.L)|(a\\.c\\.l)", replacement = "acl")) %>%
  mutate(ab_cl = str_replace_all(string = ab_cl, pattern = "(P\\.C\\.L)|(P\\.c\\.l)", replacement = "pcl"))

#+++++++++++++++++++++++
toc()
#+++++++++++++++++++++++

#*******************************************************************************
######    Combining preprocessed title and abstract      #######################
#*******************************************************************************

df_data %<>%
  unite("tiab_cl", c("ti_cl", "ab_cl"), sep = " ", remove = F)

#*******************************************************************************
########################      remove objects     ###############################
#*******************************************************************************

rm(filter)

#*******************************************************************************
########################      save image     ###################################
#*******************************************************************************

#+++++++++++++++++++++++
tic("save data")
#+++++++++++++++++++++++

save.image("00data/rdata/01preprocessing/05_remove_add_content.RData")

#+++++++++++++++++++++++
toc()
toc()
#+++++++++++++++++++++++

