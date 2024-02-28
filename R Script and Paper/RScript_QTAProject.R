#remove all lists
rm(list=ls())

#detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

#set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

#load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

lapply(c("tidyverse",
         "ggplot2",
         "quanteda", 
         "lubridate",
         "quanteda.dictionaries",
         "quanteda.textmodels", 
         "quanteda.textstats",
         "quanteda.textplots",
         "dplyr",
         "gridExtra",
         "xtable"), # For parallel processing
       pkgTest)

#load data
data <- read.csv("data/vote_data.csv", 
                 stringsAsFactors=FALSE,
                 encoding = "utf-8")

#prepare
data$Arguments <- str_replace_all(data$Arguments, "[✓|✗]", ".")
data$Arguments <- str_replace_all(data$Arguments, "<|>|\n|\n-|«|»|\"", " ")



#create corpus
corp <- corpus(data,
               text_field = "Arguments",
               include = docvars)


#clean and prepare
prep_toks <- function(text_corpus){
  toks <- tokens(text_corpus,
                 include_docvars = TRUE) %>%
    tokens_tolower() %>%
    tokens_remove(stopwords("german"), padding = TRUE) %>%
    tokens_remove('[\\p{P}\\p{S}]', valuetype = 'regex', padding = TRUE)
  return(toks)
}

#collocations
get_coll <-function(tokens){
  unsup_col <- textstat_collocations(tokens,
                                     method = "lambda",
                                     size = 2,
                                     min_count = 5,
                                     smoothing = 0.5)
  unsup_col <- unsup_col[order(-unsup_col$count),] 
  return(unsup_col)
}

prepped_toks <- prep_toks(corp) #basic token cleaning
collocations <- get_coll(prepped_toks) #get collocations

#collocations cleaning no2
get_coll <- function(tokens, keep_col) {
  unsup_col <- textstat_collocations(tokens,
                                     method = "lambda",
                                     size = 2,
                                     min_count = 5,
                                     smoothing = 0.5)
  unsup_col <- unsup_col[order(-unsup_col$count),] 
  unsup_col <- unsup_col[unsup_col$collocation %in% keep_col, ]
  return(unsup_col)
}

#defining collocations by hand
collocations_to_keep <- c("service public", "parla ment",
                          "schweizer bevölkerung", "erneuerbaren energien",
                          "direkte demokratie", "wirtschaftsstandort schweiz",
                          "ahv 21", "erleichterte einbürgerung", 
                          "direkte bundessteuer", "schweizer pass",
                          "zweite röhre", "sicher heit", "berufliche vorsorge", 
                          "bilateralen abkommen", "energiestrategie 2020")

#filterin and allocating
filtered_collocations <- get_coll(prepped_toks, collocations_to_keep)
collocations <- filtered_collocations

#creating toks
toks <- tokens_compound(prepped_toks, pattern = collocations_to_keep) 
toks <- tokens_remove(tokens(toks), "")

####
##analysis2: getting an overview
corpsum_all <- summary(corp, 
                     n = nrow(docvars(corp)) #note: the default is n=100
) 
summary(corpsum_all)

corpsum_all$ttr <- corpsum_all$Types / corpsum_all$Tokens
sum(corpsum_all$Types)
sum(corpsum_all$Tokens)

#mean mttr for each category
mean_ttr_overall <- mean(corpsum_all$ttr)
mean_ttr_government <- mean(corpsum_all$ttr[corpsum_all$Body == "Government"])
mean_ttr_initiative <- mean(corpsum_all$ttr[corpsum_all$Body == "Initiative"])
mean_ttr_referenda <- mean(corpsum_all$ttr[corpsum_all$Body == "Referenda"])

#printing means
print(mean_ttr_overall)
print(mean_ttr_government)
print(mean_ttr_initiative)
print(mean_ttr_referenda)

#plot mttr
#transformin date
corpsum_all$Date <- as.Date(corpsum_all$Date, format = "%d.%m.%Y")

plot_ttr <- ggplot(corpsum_all, aes(x = Date, y = ttr, color = Body)) +
  geom_point() +
  geom_smooth(method = "lm", size = 1, level = 0.90) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(x = "Date", y = "Mean Type-Token Ratio (MTTR)", 
       title = "Mean Type-Token Ratio Over Time by Category") +
  scale_color_manual(values = c("blue", "darkgreen", "red")) +
  theme_minimal()

ggsave("plot_ttr.png", plot = plot_ttr, width = 10, height = 6, units = "in", dpi = 300)


#create dfm
docfm <- dfm(toks,
             remove_numbers = TRUE,
             remove_punct = TRUE,
             remove_symbols = TRUE,
             remove_hyphens = TRUE,
             remove_separators = TRUE,
             remove_url = TRUE)

docfm <- dfm_select(docfm, pattern = stopwords("german"), selection = "remove")

#check toks
freq_toks <- textstat_frequency(docfm)
freq_toks

#cleaning out obsolote toks in the dfm
tokens_to_remove <- c("dass", "all", "ab", "zudem")

#remove tokens from DFM and assign again
docfm <- dfm_remove(docfm, pattern = tokens_to_remove)

#Convert Date to datetime object
docfm@docvars[["Date"]] <- dmy(docfm@docvars[["Date"]])
is.Date(docfm@docvars[["Date"]])

#check
freq_toks <- textstat_frequency(docfm)
freq_toks

#########
# Plotting FRQ
########

#overall
dfm_all <- dfm_subset(docfm) 
dfm_freq_all <- textstat_frequency(dfm_all, n = 30) 
dfm_freq_all$feature <- with(dfm_freq_all, reorder(feature, -frequency))

ggplot(dfm_freq_all, aes(x = feature, y = frequency)) + 
  ggtitle("Feature frequency of All Argumentation") +
  geom_point(stat = "identity", color = "purple") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#govt
dfm_govt <- dfm_subset(docfm, Body == "Government") 
dfm_freq_govt <- textstat_frequency(dfm_govt, n = 30) 
dfm_freq_govt$feature <- with(dfm_freq_govt, reorder(feature, -frequency))

ggplot(dfm_freq_govt, aes(x = feature, y = frequency)) +
  ggtitle("Feature frequency of Govt. Argumentation") +
  geom_point(stat = "identity", color = "blue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#initiative
dfm_ini <- dfm_subset(docfm, Body == "Initiative") 
dfm_freq_ini <- textstat_frequency(dfm_ini, n = 30) 
dfm_freq_ini$feature <- with(dfm_freq_ini, reorder(feature, -frequency))

ggplot(dfm_freq_ini, aes(x = feature, y = frequency)) + 
  ggtitle("Feature frequency of Initiative Argumentation") +
  geom_point(stat = "identity", color = "darkgreen") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#referenda
dfm_ref <- dfm_subset(docfm, Body == "Referenda") 
dfm_freq_ref <- textstat_frequency(dfm_ref, n = 30) 
dfm_freq_ref$feature <- with(dfm_freq_ref, reorder(feature, -frequency))

ggplot(dfm_freq_ref, aes(x = feature, y = frequency)) + 
  ggtitle("Feature frequency of Referenda Argumentation") +
  geom_point(stat = "identity", color = "red") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#########
# Keyness
########

#govt vs. initiative
dfm_comp <- dfm_subset(docfm, Body %in% c("Government", 
                                                  "Initiative",
                                                  "Referenda")) 

dfm_comp_govt_ini <- dfm_subset(docfm, Body %in% c("Government", 
                                          "Initiative"))

dfm_comp_govt_ref <- dfm_subset(docfm, Body %in% c("Government", 
                                                   "Referenda")) 

dfm_keyness <- dfm_group(dfm_comp, groups = Body) 
dfm_keyness_govt_ini <- dfm_group(dfm_comp_govt_ini, groups = Body) 
dfm_keyness_govt_ref <- dfm_group(dfm_comp_govt_ref, groups = Body) 

#comparison full body
keyness_stat_govt <- textstat_keyness(dfm_keyness, target = "Government")
keyness_stat_ini <- textstat_keyness(dfm_keyness, target = "Initiative")
keyness_stat_ref <- textstat_keyness(dfm_keyness, target = "Referenda") 

textplot_keyness(keyness_stat_govt, labelsize = 3, color = c("blue", "grey"))
textplot_keyness(keyness_stat_ini, labelsize = 3, color = c("darkgreen", "grey"))
textplot_keyness(keyness_stat_ref, labelsize = 3, color = c("red", "grey")) 

#comparison govt vs. ini
keyness_stat_govt_ini <- textstat_keyness(dfm_keyness_govt_ini, target = "Government")
plot_keyness_govt_ini <- 
  textplot_keyness(keyness_stat_govt_ini, labelsize = 3, 
                   color = c("blue", "darkgreen")) 

#comparison govt vs. ref
keyness_stat_govt_ref <- textstat_keyness(dfm_keyness_govt_ref, target = "Government")
plot_keyness_govt_ref <- 
  textplot_keyness(keyness_stat_govt_ref, labelsize = 3, 
                   color = c("blue", "red")) 

key_combined_plots <- grid.arrange(plot_keyness_govt_ini, 
                                   plot_keyness_govt_ref, ncol = 2)

ggsave("key_combined_plots.png", plot = key_combined_plots, 
       width = 12, height = 6, dpi = 300)


#text stats
govt_corp <- corpus_subset(corp, Body %in% "Government")
govt_corp <- tokens(govt_corp)

govt_kwic1 <- kwic(govt_corp, pattern = phrase("änderung"), window = 5, case_insensitive = TRUE)
govt_kwic2 <- kwic(govt_corp, pattern = phrase("unternehmen"), window = 5, case_insensitive = TRUE)
govt_kwic3 <- kwic(govt_corp, pattern = phrase("schweiz"), window = 5, case_insensitive = TRUE)
head(govt_kwic1)
head(govt_kwic2)
head(govt_kwic3)

#initiative
ini_corp <- corpus_subset(corp, Body %in% "Initiative")
ini_corp <- tokens(ini_corp)

ini_kwic1 <- kwic(ini_corp, pattern = phrase("unsere"), window = 5, case_insensitive = TRUE)
ini_kwic2 <- kwic(ini_corp, pattern = phrase("jahr"), window = 5, case_insensitive = TRUE)
ini_kwic3 <- kwic(ini_corp, pattern = phrase("endlich"), window = 5, case_insensitive = TRUE)
head(ini_kwic1)
head(ini_kwic2)
head(ini_kwic3)

#referenda
ref_corp <- corpus_subset(corp, Body %in% "Referenda")
ref_corp <- tokens(ref_corp)

ref_kwic1 <- kwic(ref_corp, pattern = phrase("franken"), window = 5, 
                  case_insensitive = TRUE)
ref_kwic2 <- kwic(ref_corp, pattern = phrase("sagen"), window = 5, 
                  case_insensitive = TRUE)
ref_kwic3 <- kwic(ref_corp, pattern = phrase("bürger"), window = 5, 
                  case_insensitive = TRUE)
head(ref_kwic1)
head(ref_kwic2)
head(ref_kwic3)


#### /// GOVT sentiment

#load dictionary rauh
data_dictionary_Rauh <- readRDS("data/data_dictionary_Rauh.rds")

#sentimentoverall
sent_dfm <- quanteda::dfm_lookup(docfm, data_dictionary_Rauh)
sent_dfm

#net sentiment as new docvars
net_sent_values <- rowSums(sent_dfm[, c("positive")]) - 
  rowSums(sent_dfm[, c("negative")])

str(net_sent_values)

#new docvars with net sent values
docvars(sent_dfm)$net_sent <- net_sent_values

str(sent_dfm)

#sentiment plot
plot_sent_loess <- ggplot(data = docvars(sent_dfm), 
                      aes(x = Date, y = net_sent_values, color = Body)) + 
  geom_point(stat = "identity") +
  geom_smooth(method = "loess", size = 1, level = .95) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(breaks = seq(-20, 100, by = 5), limits = c(-20, 70)) +
  labs(x = "Date", y = "Net Sentiment") +
  scale_color_manual(values = c("blue", "darkgreen", "red")) +
  theme_minimal() +
  ggtitle("Net Sentiment Over Time: Loess")

plot_sent_lm <- ggplot(data = docvars(sent_dfm), 
                    aes(x = Date, y = net_sent_values, color = Body)) + 
  geom_point(stat = "identity") +
  geom_smooth(method = "lm", size = 1, na.rm = TRUE, level = .95) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(breaks = seq(-20, 100, by = 5), limits = c(-20, 70)) +
  labs(x = "Date", y = "Net Sentiment") +
  scale_color_manual(values = c("blue", "darkgreen", "red")) +
  theme_minimal() +
  ggtitle("Net Sentiment Over Time: LM")

#combine plots
sent_combined_plots <- grid.arrange(plot_sent_loess, 
                                    plot_sent_lm, nrow = 2)

ggsave("sent_combined_plots.png", plot = sent_combined_plots, 
       width = 10, height = 10, dpi = 300)

#### /// GOVERNMENT sentiment
sent_dfm_govt <- dfm_subset(sent_dfm, docvars(sent_dfm)[["Body"]] == "Government")
#subset net sentiment govt
net_sentiment_govt <- sent_dfm_govt@docvars[["net_sent"]]

#### /// INITIATIVE sentiment
sent_dfm_ini <- dfm_subset(sent_dfm, docvars(sent_dfm)[["Body"]] == "Initiative")
#subset net sentiment initiative
net_sentiment_ini <- sent_dfm_ini@docvars[["net_sent"]]

#### /// REFERENDA sentiment
sent_dfm_ref <- dfm_subset(sent_dfm, docvars(sent_dfm)[["Body"]] == "Referenda")
#subset net sentiment referenda
net_sentiment_ref <- sent_dfm_ref@docvars[["net_sent"]]


#####statisticial analysis
#converting to dataframe
sent_df <- convert(sent_dfm, to = "data.frame")

#docvars into dataframe
sent_df <- cbind(sent_df, docvars(sent_dfm))

#date
sent_df$Date <- as.numeric(sent_df$Date)

#filtration
government_df <- sent_df %>%
  filter(Body == "Government")

initiative_df <- sent_df %>%
  filter(Body == "Initiative")

referenda_df <- sent_df %>%
  filter(Body == "Referenda")

combined_df <- rbind(
  mutate(government_df, Category = "Government"),
  mutate(initiative_df, Category = "Initiative"),
  mutate(referenda_df, Category = "Referenda")
)

#lm
sent_lm_model_government <- lm(net_sent ~ Date, data = government_df)
sent_lm_model_initiative <- lm(net_sent ~ Date, data = initiative_df)
sent_lm_model_referenda <- lm(net_sent ~ Date, data = referenda_df)

#summaries
summary(sent_lm_model_government)
summary(sent_lm_model_initiative)
summary(sent_lm_model_referenda)




#######
### READABILITY for each category

rd <- textstat_readability(corp, measure = "SMOG.de")

#for plotting
df_rd <- cbind(docvars(corp), as.data.frame(rd))
str(df_rd)

#mean excluding outliers for smog.de


###mean for each body
#grouping to calculate smog.de
mean_scores <- df_rd %>%
  group_by(Body) %>%
  summarize(smog.de_mean = mean(SMOG.de, na.rm = TRUE))

#print mean scores
print(mean_scores)

######
###Plotting smog.de

#date transformation
df_rd$Date <- as.Date(df_rd$Date, format = "%d.%m.%Y")

#plotting
plot_smog_loess <- ggplot(df_rd, aes(x = Date, y = SMOG.de, color = Body)) +
  geom_point() +
  geom_smooth(method = "loess", size = 1, level = .95) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(x = "Date", y = "SMOG.de Readability Score: Loess") +
  scale_color_manual(values = c("blue", "darkgreen", "red")) +
  ggtitle("SMOG.de Index over Time: Loess") +
  theme_minimal()

plot_smog_lm <- ggplot(df_rd, aes(x = Date, y = SMOG.de, color = Body)) +
  geom_point() +
  geom_smooth(method = "lm", size = 1, level = .95) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(x = "Date", y = "SMOG.de Readability Score: LM") +
  scale_color_manual(values = c("blue", "darkgreen", "red")) +
  ggtitle("SMOG.de Index over Time: LM")
  theme_minimal()

#combine plots
smog_combined_plots <- grid.arrange(plot_smog_loess, 
                                    plot_smog_lm, nrow = 2)

ggsave("smog_combined_plots.png", plot = smog_combined_plots, 
       width = 10, height = 10, dpi = 300)

#####statisticial analysis
#Filter data for each body
smogde_government <- df_rd %>% filter(Body == "Government")
smogde_initiative <- df_rd %>% filter(Body == "Initiative")
smogde_referenda <- df_rd %>% filter(Body == "Referenda")

#fitting linear model
lm_model_government <- lm(SMOG.de ~ Date, data = smogde_government)
lm_model_initiative <- lm(SMOG.de ~ Date, data = smogde_initiative)
lm_model_referenda <- lm(SMOG.de ~ Date, data = smogde_referenda)

#summaries
summary(lm_model_government)
summary(lm_model_initiative)
summary(lm_model_referenda)



