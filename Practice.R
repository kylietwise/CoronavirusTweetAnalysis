# Homework 2
# Kylie Wise - Jordan Rodu

install.packages("readtext")
install.packages("tm")
install.packages("NLP")
install.packages("quanteda")
install.packages("topicmodels")
install.packages("stopwords")
install.packages("partition")
install.packages("tidytext")
library(readtext)
library(tm)
library(NLP)
library(tidyverse)
library(quanteda)
library(topicmodels)
library(stopwords)
library(partition)
library(tidytext)
library(ggplot2)
library(dplyr)

library(tidyverse)
library(lubridate)

setwd("~/Desktop/Text Analysis/dataverse_files")


# Retried this from the first time that I did a similar thing in homework 2. Didnt work when I tried it in homework two, however
# it worked this time, which is nice. However opted with a different strategy in the second part just so that it would align 
# with the homework I did in project 2 to make the entire proccess much smoother. 

all_files <- list.files("~/Desktop/Text Analysis/dataverse_files", pattern = "*.txt", full.names = TRUE, recursive = TRUE)
texts1 <- sapply(all_files, function(x) readLines(x, encoding = "UTF-8"))
texts1 <- lapply(texts, function(x) paste(x, collapse = " " ))

# This is the option I ended up using to load the data (see explanation above to as why I did this).
texts_final1 <- readtext(all_files, ignore_missing_files = FALSE, docvarsfrom = "filepaths", dvsep = "/")
texts_final_as_string <- as.String(as.character(texts_final1))

# In this section of code I am cleaning up the tweets, hopefully (if I'm doing it correctly) I'm removing all numbers, 
# punctuation, symbols, and urls. I understand many of these things may be important to an analysis, particularly of tweets
# however with my limited knowledge of R it is important that I am using the most simplified version of what I want to do to avoid errors
# and making things more complicated than they need to be. 
corpus_list <- Corpus(texts_final1$text, texts_final1$doc_id)
token_list <- tokens(corpus_list, remove_numbers = TRUE, remove_punct = TRUE,
                     remove_symbols = TRUE, remove_separators = TRUE, remove_url = TRUE)
tokens_lower <- tokens_tolower(token_list)

# Here I am going to go in and take out some stop words. Especially because I am going to be doing some topic modeling
# I think it will be important to remove some of these words so we can actually get a feel for what is genuinely being discussed. 
# I know have done the analysis and am going back to add additional stop words that came up. 

sw <- stopwords(language = "en", source = "nltk") # stopwards from pythons nltk package
token_list_wosw <- tokens_remove(tokens_lower, sw) 
additional_stop <- c("also","could","year","people","said","first","last","people","would","one",
                     "time","years","would", "best","get","made","make",
                     "like","many","new","next","told","two","three","world","take","way","set","back","added","bbc",
                     "says","well","good","may","going","number","still") # went back and did this after I looked at my dtm


texts_final_as_vector <- VectorSource(token_list_wosw)
texts_final_as_corpus <- VCorpus(texts_final_as_vector)
dt_matrix <- DocumentTermMatrix(texts_final_as_corpus)

dtm <- inspect(dt_matrix)
rowTotals <- apply(dtm , 1, sum) #Find the sum of words in each Document
dtm.new   <- dtm[rowTotals> 0, ]   


k <- 10
LDA_attempt <- LDA(dtm, k =k, control = list(seed = 10000000))
# found this next part online and copied and pasted it basically
ap_topics <- tidy(LDA_attempt, matrix = "beta")
ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()
