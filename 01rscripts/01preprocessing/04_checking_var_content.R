#*******************************************************************************
#*******************************************************************************
##########################     checking_var_content     ########################
#*******************************************************************************
#*******************************************************************************

# check data for additional content - only use to develop script 05_remove_var_content

#*******************************************************************************

#*******************************************************************************
# set encoding
#*******************************************************************************

df_data <- 
  df_data %>% 
  mutate(title = enc2utf8(title),
         abstract = enc2utf8(abstract))

df_data$title <- enc2utf8(df_data$title)

# #*******************************************************************************
# # check if is NA
# #*******************************************************************************
# 
# # title
# df_data %>% 
#   pull(title) %>% 
#   is.na() %>% 
#   table() 
# # -> no empty title
# 
# # abstract
# df_data %>% 
#   pull(abstract) %>% 
#   is.na() %>% 
#   table() 
# # -> 630 empty abstracts
# 
# 
# 
# 
# # title
# df_data$title[str_length(df_data$title) < 10]
# 
# # abstract
# df_data$abstract[str_length(df_data$abstract) < 10]


#***********
# solution *
#***********

# title:
# -> NA und Jahres zahlen raus.
df_data$title[str_length(df_data$title) == 4] <- "xxx"
df_data$title[df_data$title == ""] <- "xxx"

# replace all NA
df_data$abstract[which(df_data$abstract=="")] <- "xxx"

#*******************************************************************************
####################    Prepare title      #####################################
#*******************************************************************************

# #*******************************************************************************
# # check for additional content in title
# #*******************************************************************************
# 
# df_data %>% 
#   pull(title) %>% 
#   grep(pattern = "\\[|\\{|\\(") %>% 
#   df_data$title[.]
# # -> contains brackets with additional content
# 
# # check if the pattern \\. \\[ is always existent
# 
# # prepate vector which NOT contains points
# without.points <- 
#   df_data %>% 
#   pull(title) %>% 
#   grepl(.,pattern = "\\.") %>%
#   not %>%
#   which()
# 
# # prepare vector which contains brackets
# with.bracket <- 
#   df_data %>% 
#   pull(title) %>% 
#   grepl(.,pattern = "\\[") %>%
#   which()
# 
# # check if always points before brackets
# without.points[without.points %in% with.bracket]

# -> NO there are 

#***********
# solution *
#***********

#+++++++++++++++++++++++++++++++++++++++++++++++++
# delete content with pattern: . [?  OR .[?
#+++++++++++++++++++++++++++++++++++++++++++++++++
df_data <- 
  df_data %>% 
  mutate(str_extract_ti1 = str_extract_all(string = .$title, pattern = "(\\. \\[.+)|(\\.\\[.+)")) %>% 
  mutate(ti_cl = str_replace(string = .$title, pattern = "(\\. \\[.+)|(\\.\\[.+)",replacement = ""))


# df_data %>%
#   pull(str_extract_ti1) %>%
#   table() %>%
#   names()
# # -> deletion is correct
#+++++++++++++++++++++++++++++++++++++++++++++++++
# delete content with pattern: ] [? OR ][? 
#+++++++++++++++++++++++++++++++++++++++++++++++++
df_data <- 
  df_data %>% 
  mutate(str_extract_ti2 = str_extract(.$ti_cl, "(\\] \\[.+)|(\\]\\[.+)")) %>% 
  mutate(ti_cl = str_replace(.$ti_cl, "(\\] \\[.+)|(\\]\\[.+)", replacement = "" ))


# df_data %>% 
#   pull(str_extract_ti2) %>% 
#   table() %>% 
#   names()
# # -> deletion is correct

#+++++++++++++++++++++++++++++++++++++++++++++++++
# delete content with pattern: ) [? OR )[? 
#+++++++++++++++++++++++++++++++++++++++++++++++++
df_data <- 
  df_data %>% 
  mutate(str_extract_ti3 = str_extract(.$ti_cl, "(\\) \\[.+)|(\\)\\[.+)")) %>% 
  mutate(ti_cl = str_replace(.$ti_cl, "(\\) \\[.+)|(\\)\\[.+)", replacement = "" ))


# df_data %>% 
#   pull(str_extract_ti3) %>% 
#   table() %>% 
#   names()
# # # -> deletion is correct

#+++++++++++++++++++++++++++++++++++++++++++++++++
# delete content with pattern: ^[a-zA-Z].*(\\[.*\\]$) --> start with text and end with [?] or [?].
#+++++++++++++++++++++++++++++++++++++++++++++++++
filter <- 
  df_data %>% 
  pull(ti_cl) %>% 
  grep(pattern = "(^[a-zA-Z].*(\\[.*\\]$))|(^[a-zA-Z].*(\\[.*\\]\\.$))")

df_data$str_extract_ti4 <- NA
df_data$str_extract_ti4[filter] <- str_extract(df_data$ti_cl[filter], "\\[.+\\]")

df_data$ti_cl[filter] <- str_replace(df_data$ti_cl[filter], "\\[.+\\]", replacement = "" )

# df_data %>% 
#   pull(str_extract_ti4) %>% 
#   table() %>% 
#   names()
# # -> deletion is correct

#+++++++++++++++++++++++++++++++++++++++++++++++++
# delete content with ...includ
#+++++++++++++++++++++++++++++++++++++++++++++++++
df_data <- 
  df_data %>% 
  mutate(str_extract_ti5 = str_extract(.$ti_cl, "\\.\\.\\.includ.*")) %>% 
  mutate(ti_cl = str_replace(.$ti_cl, "\\.\\.\\.includ.*", replacement = "" ))


# df_data %>% 
#   pull(str_extract_ti5) %>% 
#   table() %>% 
#   names()
# # -> deletion is correct

#+++++++++++++++++++++++++++++++++++++++++++++++++
# delete content with (Provisional abstract)|(Structured abstract)
#+++++++++++++++++++++++++++++++++++++++++++++++++
df_data <- 
  df_data %>% 
  mutate(str_extract_ti6 = str_extract(string = .$ti_cl, pattern = "(Provisional abstract)|(Structured abstract)")) %>% 
  mutate(ti_cl = str_replace(string = .$ti_cl, pattern = "(Provisional abstract)|(Structured abstract)",replacement = ""))


# df_data %>% 
#   pull(str_extract_ti6) %>% 
#   table() %>% 
#   names()
# # -> deletion is correct

#+++++++++++++++++++++++++++++++++++++++++++++++++
# delete content with abstract
#+++++++++++++++++++++++++++++++++++++++++++++++++
filter <- 
  df_data %>% 
  pull(ti_cl) %>% 
  grep(pattern = "Abstract")

df_data$str_extract_ti7 <- NA
df_data$str_extract_ti7[filter] <- df_data$ti_cl[filter]

df_data$ti_cl[filter] <- "xxx"

# df_data %>% 
#   pull(str_extract_ti7) %>% 
#   table() %>% 
#   names()
# # -> deletion is correct

#+++++++++++++++++++++++++++++++++++++++++++++++++
# delete content with [corrected
#+++++++++++++++++++++++++++++++++++++++++++++++++
df_data <- 
  df_data %>% 
  mutate(str_extract_ti8 = str_extract(string = .$ti_cl, pattern = "\\[corrected$")) %>% 
  mutate(ti_cl = str_replace(string = .$ti_cl, pattern = "\\[corrected$",replacement = ""))


# df_data %>% 
#   pull(str_extract_ti8) %>% 
#   table() %>% 
#   names()
# # -> deletion is correct

#+++++++++++++++++++++++++++++++++++++++++++++++++
# delete content with [1] or [2]
#+++++++++++++++++++++++++++++++++++++++++++++++++
df_data <- 
  df_data %>% 
  mutate(str_extract_ti9 = str_extract(string = .$ti_cl, pattern = "(\\[1\\].*)|(\\[2\\].*)")) %>% 
  mutate(ti_cl = str_replace(string = .$ti_cl, pattern = "(\\[1\\].*)|(\\[2\\].*)",replacement = ""))


# df_data %>% 
#   pull(str_extract_ti9) %>% 
#   table() %>% 
#   names()
# # -> deletion is correct

#+++++++++++++++++++++++++++++++++++++++++++++++++
# delete content with ... presented
#+++++++++++++++++++++++++++++++++++++++++++++++++
df_data <- 
  df_data %>% 
  mutate(str_extract_ti10 = str_extract(.$ti_cl, "\\.\\.\\. presented.*")) %>% 
  mutate(ti_cl = str_replace(.$ti_cl, "\\.\\.\\. presented.*", replacement = "" ))


# df_data %>% 
#   pull(str_extract_ti10) %>% 
#   table() %>% 
#   names()
# # -> deletion is correct

#+++++++++++++++++++++++++++++++++++++++++++++++++
# delete content with ... including
#+++++++++++++++++++++++++++++++++++++++++++++++++
df_data <- 
  df_data %>% 
  mutate(str_extract_ti11 = str_extract(.$ti_cl, "\\.\\.\\. including.*")) %>% 
  mutate(ti_cl = str_replace(.$ti_cl, "\\.\\.\\. including.*", replacement = "" ))


# df_data %>% 
#   pull(str_extract_ti11) %>% 
#   table() %>% 
#   names()
# # -> deletion is correct

#+++++++++++++++++++++++++++++++++++++++++++++++++
# delete content with ...reprinted
#+++++++++++++++++++++++++++++++++++++++++++++++++
df_data <- 
  df_data %>% 
  mutate(str_extract_ti12 = str_extract(.$ti_cl, "\\.\\.\\.reprinted.*")) %>% 
  mutate(ti_cl = str_replace(.$ti_cl, "\\.\\.\\.reprinted.*", replacement = "" ))


# df_data %>% 
#   pull(str_extract_ti12) %>% 
#   table() %>% 
#   names()
# # -> deletion is correct

#+++++++++++++++++++++++++++++++++++++++++++++++++
# delete content with Knee Surg...
#+++++++++++++++++++++++++++++++++++++++++++++++++
df_data <- 
  df_data %>% 
  mutate(str_extract_ti13 = str_extract(.$ti_cl, "\\.\\.\\.Knee.*")) %>% 
  mutate(ti_cl = str_replace(.$ti_cl, "\\.\\.\\.Knee.*", replacement = "" ))

# df_data %>%
#   pull(str_extract_ti13) %>%
#   table() %>%
#   names()
# # -> deletion is correct

#+++++++++++++++++++++++++++++++++++++++++++++++++
# delete content with Procedings
#+++++++++++++++++++++++++++++++++++++++++++++++++
df_data <- 
  df_data %>% 
  mutate(str_extract_ti14 = str_extract(.$ti_cl, "\\.\\.\\.Proceedings.*")) %>% 
  mutate(ti_cl = str_replace(.$ti_cl, "\\.\\.\\.Proceedings.*", replacement = "" ))

# df_data %>%
#   pull(str_extract_ti14) %>%
#   table() %>%
#   names()
# # -> deletion is correct

#+++++++++++++++++++++++++++++++++++++++++++++++++
# delete content with multiple letters
#+++++++++++++++++++++++++++++++++++++++++++++++++
df_data <- 
  df_data %>% 
  mutate(str_extract_ti15 = str_extract(.$ti_cl, "multiple letters")) %>% 
  mutate(ti_cl = str_replace(.$ti_cl, "multiple letters", replacement = "" ))

# df_data %>%
#   pull(str_extract_ti15) %>%
#   table() %>%
#   names()
# # -> deletion is correct

#+++++++++++++++++++++++++++++++++++++++++++++++++
# delete further content within brackets 1
#+++++++++++++++++++++++++++++++++++++++++++++++++
filter <- 
  df_data %>% 
  pull(ti_cl) %>% 
  grep(pattern = "^[a-zA-Z].*(\\[[:a-z:]*)")

df_data$ti_cl[filter]
# -> no deletion is necessary

#*******************************************************************************
####################    Prepare abstract      ##################################
#*******************************************************************************

#*******************************************************************************
# check for additional content in abstract
#*******************************************************************************

#+++++++++++++++++++++++++++++++++++++++++++++++++
# delete references
#+++++++++++++++++++++++++++++++++++++++++++++++++
df_data <- 
  df_data %>% 
  mutate(str_extract_ab1 = str_extract(string = abstract, pattern = "\\[Ref.+\\]")) %>% 
  mutate(ab_cl = str_replace(string = abstract, pattern = "\\[Ref.+\\]",replacement = ""))

# df_data %>% 
#   pull(str_extract_ab1) %>% 
#   table() %>% 
#   names()
# # -> deletion is correct

#+++++++++++++++++++++++++++++++++++++++++++++++++
# delete copyrights
#+++++++++++++++++++++++++++++++++++++++++++++++++
df_data <- 
  df_data %>% 
  mutate(str_extract_ab2 = str_extract(string = ab_cl, pattern = "Copyright.+")) %>% 
  mutate(ab_cl = str_replace(string = ab_cl, pattern = "Copyright.+",replacement = ""))

# df_data %>% 
#   pull(str_extract_ab2) %>% 
#   table() %>% 
#   names()
# # -> deletion is correct

#+++++++++++++++++++++++++++++++++++++++++++++++++
# delete Trial registration
#+++++++++++++++++++++++++++++++++++++++++++++++++
df_data <- 
  df_data %>% 
  mutate(str_extract_ab3 = str_extract(string = ab_cl, pattern = "(TRIAL REGISTRATION.+)|(TRIAL REGISTRATION.+)|(Trial registration.+)")) %>% 
  mutate(ab_cl = str_replace(string = ab_cl, pattern = "(TRIAL REGISTRATION.+)|(TRIAL REGISTRATION.+)|(Trial registration.+)",replacement = ""))

# df_data %>% 
#   pull(str_extract_ab3) %>% 
#   table() %>% 
#   names()
# # -> deletion is correct

#+++++++++++++++++++++++++++++++++++++++++++++++++
# delete licensee
#+++++++++++++++++++++++++++++++++++++++++++++++++
df_data <- 
  df_data %>% 
  mutate(str_extract_ab4 = str_extract(string = ab_cl, pattern = "licensee.+")) %>% 
  mutate(ab_cl = str_replace(string = ab_cl, pattern = "licensee.+",replacement = ""))

# df_data %>% 
#   pull(str_extract_ab4) %>% 
#   table() %>% 
#   names()
# # -> deletion is correct

#+++++++++++++++++++++++++++++++++++++++++++++++++
# delete Elsevier, Springer, Wiley., BMJ, Oxford, BioMed Central
#+++++++++++++++++++++++++++++++++++++++++++++++++
df_data <- 
  df_data %>% 
  mutate(str_extract_ab5 = str_extract(string = ab_cl, pattern = "(Wiley.+)|(Springer.+)|(Elsevier.+)|(BMJ Publish.+)|(Oxford University.+)|(BioMed Central.+)")) %>% 
  mutate(ab_cl = str_replace(string = ab_cl, pattern = "(Wiley.+)|(Springer.+)|(Elsevier.+)|(BMJ Publish.+)|(Oxford University.+)|(BioMed Central.+)",replacement = ""))

# df_data %>% 
#   pull(str_extract_ab5) %>% 
#   table() %>% 
#   names()
# # -> deletion is correct

#+++++++++++++++++++++++++++++++++++++++++++++++++
# delete \\[English Summary\\]
#+++++++++++++++++++++++++++++++++++++++++++++++++
df_data <- 
  df_data %>% 
  mutate(str_extract_ab6 = str_extract(string = ab_cl, pattern = "\\[English Summary\\]")) %>% 
  mutate(ab_cl = str_replace(string = ab_cl, pattern = "\\[English Summary\\]",replacement = ""))

# df_data %>% 
#   pull(str_extract_ab6) %>% 
#   table() %>% 
#   names()
# # -> deletion is correct

#+++++++++++++++++++++++++++++++++++++++++++++++++
# delete \\(ABSTRACT TRUNCATED\\)
#+++++++++++++++++++++++++++++++++++++++++++++++++
df_data <- 
  df_data %>% 
  mutate(str_extract_ab7 = str_extract(string = ab_cl, pattern = "\\(ABSTRACT TRUNCATED.+")) %>% 
  mutate(ab_cl = str_replace(string = ab_cl, pattern = "\\(ABSTRACT TRUNCATED.+",replacement = ""))

# df_data %>% 
#   pull(str_extract_ab7) %>% 
#   table() %>% 
#   names()
# # -> deletion is correct

#+++++++++++++++++++++++++++++++++++++++++++++++++
# delete "¶" - Bdifferent Versions for Windows and unix
#+++++++++++++++++++++++++++++++++++++++++++++++++
# # windows
df_data <-
  df_data %>%
  mutate(str_extract_ab8 = str_extract(string = ab_cl, pattern = "¶.+")) %>%
  mutate(ab_cl = str_replace(string = ab_cl, pattern = "¶.+",replacement = ""))

df_data %>%
  pull(str_extract_ab8) %>%
  table() %>%
  names()

# # unix
# df_data <- 
#   df_data %>% 
#   mutate(str_extract_ab8 = str_extract(string = ab_cl, pattern = "\266\270.+")) %>% 
#   mutate(ab_cl = str_replace(string = ab_cl, pattern = "\266\270.+",replacement = ""))

# df_data %>% 
#   pull(str_extract_ab8) %>% 
#   table() %>% 
#   names()
# # -> deletion is correct

#+++++++++++++++++++++++++++++++++++++++++++++++++
# delete \\(Table presented\\)
#+++++++++++++++++++++++++++++++++++++++++++++++++
df_data <- 
  df_data %>% 
  mutate(str_extract_ab10 = str_extract(string = ab_cl, pattern = "\\(Table presented\\)")) %>% 
  mutate(ab_cl = str_replace(string = ab_cl, pattern = "\\(Table presented\\)",replacement = ""))

# df_data %>%
#   pull(str_extract_ab10) %>%
#   table() %>%
#   names() %>%
#   data.frame()
# # -> deletion is correct

#+++++++++++++++++++++++++++++++++++++++++++++++++
# delete copyright symbol
#+++++++++++++++++++++++++++++++++++++++++++++++++
df_data <- 
  df_data %>% 
  mutate(str_extract_ab11 = str_extract(string = ab_cl, pattern = "(\\(c\\))|(\\(C\\))|(©) \\d{4}.*")) %>% 
  mutate(ab_cl = str_replace(string = ab_cl, pattern = "(\\(c\\))|(\\(C\\))|(©) \\d{4}.*",replacement = ""))

# df_data %>%
#   pull(str_extract_ab11) %>%
#   table() %>%
#   names() %>%
#   data.frame()
# # -> deletion is correct

#+++++++++++++++++++++++++++++++++++++++++++++++++
# delete funded by
#+++++++++++++++++++++++++++++++++++++++++++++++++
df_data <- 
  df_data %>% 
  mutate(str_extract_ab12 = str_extract(string = ab_cl, pattern = "Funded by.*")) %>% 
  mutate(ab_cl = str_replace(string = ab_cl, pattern = "Funded by.*",replacement = ""))

# df_data %>% 
#   pull(str_extract_ab12) %>% 
#   table() %>% 
#   names() %>%
#   data.frame()
# # -> deletion is correct

df_data <- 
  df_data %>% 
  mutate(str_extract_ab13 = str_extract(string = ab_cl, pattern = "Funding*")) %>% 
  mutate(ab_cl = str_replace(string = ab_cl, pattern = "Funding",replacement = ""))

# df_data %>% 
#   pull(str_extract_ab13) %>% 
#   table() %>% 
#   names() %>%
#   data.frame()
# # -> deletion is correct

#+++++++++++++++++++++++++++++++++++++++++++++++++
# delete Level I
#+++++++++++++++++++++++++++++++++++++++++++++++++
df_data <- 
  df_data %>% 
  mutate(str_extract_ab14 = str_extract(string = ab_cl, pattern = "\\: Level I(?!( trauma)|( Trauma)|( urban)).*")) %>% 
  mutate(ab_cl = str_replace(string = ab_cl, pattern = "\\: Level I(?!( trauma)|( Trauma)|( urban)).*",replacement = ""))

# df_data %>% 
#   pull(str_extract_ab14) %>% 
#   table() %>% 
#   names() %>%
#   data.frame()
# # -> deletion is correct

#+++++++++++++++++++++++++++++++++++++++++++++++++
# delete Therapeutic Diagnostic Prognostic Level I
#+++++++++++++++++++++++++++++++++++++++++++++++++
df_data <- 
  df_data %>% 
  mutate(str_extract_ab15 = str_extract(string = ab_cl, pattern = "((Therapeutic)|(Diagnostic)|(Prognostic))(?= Level I).*")) %>% 
  mutate(ab_cl = str_replace(string = ab_cl, pattern = "((Therapeutic)|(Diagnostic)|(Prognostic))(?= Level I).*",replacement = ""))

# df_data %>% 
#   pull(str_extract_ab15) %>% 
#   table() %>% 
#   names() %>%
#   data.frame()
# # -> deletion is correct

#+++++++++++++++++++++++++++++++++++++++++++++++++
# delete Level I.
#+++++++++++++++++++++++++++++++++++++++++++++++++
df_data <- 
  df_data %>% 
  mutate(str_extract_ab16 = str_extract(string = ab_cl, pattern = "(Level I-1b)|(Level I\\.)|(Level II\\.)|(Level III\\.)|(Level IV\\.).*")) %>% 
  mutate(ab_cl = str_replace(string = ab_cl, pattern = "(Level I-1b)|(Level I\\.)|(Level II\\.)|(Level III\\.)|(Level IV\\.).*",replacement = ""))

# df_data %>%
#   pull(str_extract_ab16) %>%
#   table() %>%
#   names() %>%
#   data.frame()
# # -> deletion is correct

#+++++++++++++++++++++++++++++++++++++++++++++++++
# delete Level I, therapeutic
#+++++++++++++++++++++++++++++++++++++++++++++++++
df_data <- 
  df_data %>% 
  mutate(str_extract_ab17 = str_extract(string = ab_cl, pattern = "(Level I, therapeutic.*)|(Level II, therapeutic.*)|(Level III, therapeutic.*)|(Level IV, therapeutic.*)")) %>% 
  mutate(ab_cl = str_replace(string = ab_cl, pattern = "(Level I, therapeutic.*)|(Level II, therapeutic.*)|(Level III, therapeutic.*)|(Level IV, therapeutic.*)",replacement = ""))

# df_data %>% 
#   pull(str_extract_ab17) %>% 
#   table() %>% 
#   names() %>%
#   data.frame()
# # -> deletion is correct

#+++++++++++++++++++++++++++++++++++++++++++++++++
# delete Level I, therapeutic
#+++++++++++++++++++++++++++++++++++++++++++++++++
df_data <- 
  df_data %>% 
  mutate(str_extract_ab19 = str_extract(string = ab_cl, pattern = "((Therapeutic)|(Diagnostic)|(Prognostic))(?= study, Level I).*")) %>% 
  mutate(ab_cl = str_replace(string = ab_cl, pattern = "((Therapeutic)|(Diagnostic)|(Prognostic))(?= study, Level I).*",replacement = ""))

# df_data %>% 
#   pull(str_extract_ab19) %>% 
#   table() %>% 
#   names() %>%
#   data.frame()
# # -> deletion is correct

#+++++++++++++++++++++++++++++++++++++++++++++++++
# (LEVEL OF EVIDENCE)(?!( III\\: 34)).*
#+++++++++++++++++++++++++++++++++++++++++++++++++
df_data <- 
  df_data %>% 
  mutate(str_extract_ab20 = str_extract(string = ab_cl, pattern = "(LEVEL OF EVIDENCE)(?!( III\\: 34)).*")) %>% 
  mutate(ab_cl = str_replace(string = ab_cl, pattern = "(LEVEL OF EVIDENCE)(?!( III\\: 34)).*",replacement = ""))

# df_data %>% 
#   pull(str_extract_ab20) %>% 
#   table() %>% 
#   names() %>%
#   data.frame()
# # -> deletion is correct

#+++++++++++++++++++++++++++++++++++++++++++++++++
# delete Level of Evidence but not if in the middle
#+++++++++++++++++++++++++++++++++++++++++++++++++
df_data <- 
  df_data %>% 
  mutate(str_extract_ab9 = str_extract(string = ab_cl, pattern = "((Level of evidence)|(Level of Evidence))(?!((( 1)|( 2)|( 3)|( 4)|( of)|( I\\-III\\) eva))|(, ((1)|(2)|(3)|(4))))).*")) %>% 
  mutate(ab_cl = str_replace(string = ab_cl, pattern = "((Level of evidence)|(Level of Evidence))(?!((( 1)|( 2)|( 3)|( 4)|( of)|( I))|(, ((1)|(2)|(3)|(4))))).*",replacement = ""))

# df_data %>% 
#   pull(str_extract_ab9) %>% 
#   table() %>% 
#   names() %>%
#   data.frame()
# # -> deletion is correct

#+++++++++++++++++++++++++++++++++++++++++++++++++
# (Level of evidence, 1\\.)|(Level of evidence, 2\\.)|(Level of evidence, 3\\.)|(Level of evidence, 4\\.)
#+++++++++++++++++++++++++++++++++++++++++++++++++
df_data <-
  df_data %>%
  mutate(str_extract_ab21 = str_extract(string = ab_cl, pattern = "(Level of evidence 1\\.)|(Level of evidence 2\\.)|(Level of evidence 3\\.)|(Level of evidence 4\\.)|(Level of evidence, 1\\.)|(Level of evidence, 2\\.)|(Level of evidence, 3\\.)|(Level of evidence, 4\\.)")) %>%
  mutate(ab_cl = str_replace(string = ab_cl, pattern = "(Level of evidence 1\\.)|(Level of evidence 2\\.)|(Level of evidence 3\\.)|(Level of evidence 4\\.)|(Level of evidence, 1\\.)|(Level of evidence, 2\\.)|(Level of evidence, 3\\.)|(Level of evidence, 4\\.)",replacement = ""))

# df_data %>%
#   pull(str_extract_ab21) %>%
#   table() %>%
#   names() %>%
#   data.frame()
# # -> deletion is correct


#+++++++++++++++++++++++++++++++++++++++++++++++++
# (Level of evidence, 1\\.)|(Level of evidence, 2\\.)|(Level of evidence, 3\\.)|(Level of evidence, 4\\.)
#+++++++++++++++++++++++++++++++++++++++++++++++++
df_data <-
  df_data %>%
  mutate(str_extract_ab22 = str_extract(string = ab_cl, pattern = "(Level of Evidence III.*)|(Level of evidence III.*)|(Level of evidence IV.*)")) %>%
  mutate(ab_cl = str_replace(string = ab_cl, pattern = "(Level of Evidence III.*)|(Level of evidence III.*)|(Level of evidence IV.*)",replacement = ""))

# df_data %>%
#   pull(str_extract_ab22) %>%
#   table() %>%
#   names() %>%
#   data.frame()
# # -> deletion is correct

#+++++++++++++++++++++++++++++++++++++++++++++++++
# (Levels of evidence
#+++++++++++++++++++++++++++++++++++++++++++++++++
df_data <-
  df_data %>%
  mutate(str_extract_ab23 = str_extract(string = ab_cl, pattern = "(Level of evidence, 1.*)|(Levels of Evidence.*)|(Levels of evidence.*)|(Level Of Evidence.*)|(Level of Evidence II\\.*)|(Level of evidence II\\.*)|(Level of evidence III\\.*)|(Level of evidence IV\\.*)")) %>%
  mutate(ab_cl = str_replace(string = ab_cl, pattern = "(Levels of Evidence.*)|(Levels of evidence.*)|(Level Of Evidence.*)|(Level of Evidence II\\.*)|(Level of evidence II\\.*)|(Level of evidence III\\.*)|(Level of evidence IV\\.*)",replacement = ""))

# df_data %>%
#   pull(str_extract_ab23) %>%
#   table() %>%
#   names() %>%
#   data.frame()
# # -> deletion is correct

#+++++++++++++++++++++++++++++++++++++++++++++++++
# acl
#+++++++++++++++++++++++++++++++++++++++++++++++++
df_data <-
  df_data %>%
  mutate(str_extract_ab24 = str_extract(string = ab_cl, pattern = "(A\\.C\\.L)|(a\\.c\\.l)")) %>%
  mutate(ab_cl = str_replace_all(string = ab_cl, pattern = "(A\\.C\\.L)|(a\\.c\\.l)",replacement = "acl"))

df_data %>%
  pull(str_extract_ab24) %>%
  table() %>%
  names() %>%
  data.frame()

#+++++++++++++++++++++++++++++++++++++++++++++++++
# pcl
#+++++++++++++++++++++++++++++++++++++++++++++++++
df_data <-
  df_data %>%
  mutate(str_extract_ab25 = str_extract(string = ab_cl, pattern = "(P\\.C\\.L)|(P\\.c\\.l)")) %>%
  mutate(ab_cl = str_replace_all(string = ab_cl, pattern = "(P\\.C\\.L)|(P\\.c\\.l)",replacement = "acl"))

df_data %>%
  pull(str_extract_ab25) %>%
  table() %>%
  names() %>%
  data.frame()


