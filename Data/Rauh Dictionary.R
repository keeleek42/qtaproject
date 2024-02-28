install.packages("quanteda")

# Load the quanteda package
library(quanteda)
library(quanteda.textstats)
library(dplyr)
getwd()

# load dictionary dataframes (downloaded here: https://doi.org/10.7910/DVN/BKBX)
load("/Users/sire/Downloads/JITP-Replication-Final/1_Dictionaries/Rauh_SentDictionaryGerman_Negation.Rdata")
load("/Users/sire/Downloads/JITP-Replication-Final/1_Dictionaries/Rauh_SentDictionaryGerman.Rdata")

# new column where NOT and word are divided with a space
neg.sent.dictionary <- neg.sent.dictionary %>% 
  mutate(word = gsub("NOT_", "NOT ", feature)) %>% 
  mutate(sentiment = ifelse(sentiment == 1, "neg_negative", "neg_positive"))

sent.dictionary <- sent.dictionary %>% 
  mutate(word = feature) %>% 
  mutate(sentiment = ifelse(sentiment == -1, "negative", "positive"))

# bind both dataframes
sent_dictionary_rauh <- bind_rows(sent.dictionary, neg.sent.dictionary)

# save as quanteda dictionary (word and sentiment column)
data_dictionary_Rauh <- quanteda::as.dictionary(sent_dictionary_rauh)

saveRDS(data_dictionary_Rauh, file = "/Users/sire/Documents/ASDS Sem 2/QTA/Project/data_dictionary_Rauh.rds")



