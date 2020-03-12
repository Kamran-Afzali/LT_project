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

tweets_Corona <- search_tweets("Coronavirus OR Corona", n=10000)

tweets_Corona2=as.data.frame(tweets_Corona[,c(1,5)])

tweets_Corona2$text <- 
  tweets_Corona2$text %>%
  str_remove("\\n") %>%                  
  rm_twitter_url() %>%                   
  rm_url() %>%
  str_remove_all("#\\S+") %>%             
  str_remove_all("@\\S+") %>%             
  removeWords(c(stopwords("english"))) %>%  
  removeNumbers() %>%
  stripWhitespace() %>%
  removeWords(c("amp", "the"))          

d <- tibble(txt = tweets_Corona2$text)
Corona_bigrams <- d %>% unnest_tokens(bigram, txt, token = "ngrams", n = 2)


bigrams_separated <- Corona_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

bigrams_filtered=bigrams_filtered [-c(which(bigrams_filtered$word1%in%c('de','del','por','en','el','la')| bigrams_filtered$word2%in%c('de','del','por','en','el','la'))), ]

bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

bigram_counts


bigram_graph <- bigram_counts %>%
  filter(n > 65) %>%
  graph_from_data_frame()

bigram_graph

set.seed(2017)
pdf("ggraph.pdf",width = 10,height = 10)
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)
dev.off()

