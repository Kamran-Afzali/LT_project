library(rtweet)
library(httpuv)
library(rtweet)       
library(tm)           
library(stringr)      
library(qdapRegex)    
library(wordcloud2)   
library(tidytext)
library(dplyr)

token=create_token(
  app = "...",
  consumer_key = ".....",
  consumer_secret = "......",
  access_token = "....",
  access_secret = ".....")
token

tweets_luke <- get_timelines(c("lthomasnews"), n = 5000)
text <- str_c(tweets_luke$text, collapse = "")

text <- 
  text %>%
  str_remove("\\n") %>%                   
  rm_twitter_url() %>%                    
  rm_url() %>%
  str_remove_all("#\\S+") %>%             
  str_remove_all("@\\S+") %>%             
  removeWords(c(stopwords("english"))) %>%   
  removeNumbers() %>%
  stripWhitespace() %>%
  removeWords(c("amp", "the"))                  
textCorpus <- 
  Corpus(VectorSource(text)) %>%
  TermDocumentMatrix() %>%
  as.matrix()

textCorpus <- sort(rowSums(textCorpus), decreasing=TRUE)
textCorpus <- data.frame(word = names(textCorpus), freq=textCorpus, row.names = NULL)
textCorpus=textCorpus[textCorpus$freq>4,]
textCorpus=textCorpus[-c(which(as.vector(textCorpus$word)%in%stopwordslangs$word[stopwordslangs$lang=="en"][1:250])),]


yellowPalette <- c("black", "gold")
url = #"....logo_url"
lt <- "lt.jpg"
download.file(url, lt) # download file
lt="lt.jpg"
wordcloud <-wordcloud2(data = textCorpus, figPath = lt, color=rep_len( yellowPalette, nrow(textCorpus)))
wordcloud
