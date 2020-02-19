
library(rtweet)        
library(tm)            
library(stringr)       
library(qdapRegex)     
library(wordcloud2)    
library(tidytext)
library(dplyr)
library(stopwords)
library(PersianStemmer)
library(tidyr)
library(dplyr)
library(stringr)
library(igraph)
library(ggraph)
token=create_token(
  app = "....",
  consumer_key = "....",
  consumer_secret = ".....",
  access_token = ".....",
  access_secret = "......")
token
rtweet::trends_available()
tweets_tal <- get_timelines(c("talyarkoni"), n = 5000)
tweets_tal2=as.data.frame(tweets_tal[,c(1,5)])

tweets_tal2$text <- 
  tweets_tal2$text %>%
  str_remove("\\n") %>%                  
  rm_twitter_url() %>%                   
  rm_url() %>%
  str_remove_all("#\\S+") %>%             
  str_remove_all("@\\S+") %>%             
  removeWords(c(stopwords("english"))) %>%  
  removeNumbers() %>%
  stripWhitespace() %>%
  removeWords(c("amp", "the"))          

d <- tibble(txt = tweets_tal2$text)
tal_bigrams <- d %>% unnest_tokens(bigram, txt, token = "ngrams", n = 2)


bigrams_separated <- tal_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)


bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

bigram_counts


bigram_graph <- bigram_counts %>%
  filter(n > 5) %>%
  graph_from_data_frame()

bigram_graph

set.seed(2017)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)
