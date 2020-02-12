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
url = "https://www.google.com/imgres?imgurl=http%3A%2F%2Fi1.sndcdn.com%2Favatars-000700723486-vbjkza-original.jpg&imgrefurl=https%3A%2F%2Fwww.stitcher.com%2Fpodcast%2Fluke-thomas%2Fpromotional-malpractice&tbnid=gT6f6VKjDtpDrM&vet=12ahUKEwii_fO_0cznAhWPC98KHe8gBEgQMygCegUIARDQAQ..i&docid=cQkhH1cbPhk4FM&w=1400&h=1400&q=luke%20thomas%20logo&ved=2ahUKEwii_fO_0cznAhWPC98KHe8gBEgQMygCegUIARDQAQ"
lt <- "lt.jpg"
download.file(url, lt) # download file
lt="lt.jpg"
wordcloud <-wordcloud2(data = textCorpus, figPath = lt, color=rep_len( yellowPalette, nrow(textCorpus)))
wordcloud