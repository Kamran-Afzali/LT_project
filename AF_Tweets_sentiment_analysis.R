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
tweets_Allen <- get_timelines(c("AllenFrancesMD"), n = 5000)
tweets_Allen2=as.data.frame(tweets_Allen[,c(1,5)])

tweets_Allen2$text <- 
  tweets_Allen2$text %>%
  str_remove("\\n") %>%                  
  rm_twitter_url() %>%                   
  rm_url() %>%
  str_remove_all("#\\S+") %>%             
  str_remove_all("@\\S+") %>%             
  removeWords(c(stopwords("english"))) %>%  
  removeNumbers() %>%
  stripWhitespace() %>%
  removeWords(c("amp", "the"))        


mat=as.data.frame(as.matrix(TermDocumentMatrix(Corpus(VectorSource(tweets_Allen2$text)))))
mat$word=row.names(mat)
mat2=data.table::melt(mat,id.vars="word")
mat3= mat2 %>% 
  inner_join(get_sentiments("nrc") %>% 
               filter(sentiment %in% c("positive", 
                                       "negative"))) 

pp=aggregate(value~word, data=mat3[mat3$sentiment=="positive",],sum)
pp=pp[rev(order(pp$value)),]

pp=pp[-c(which(as.vector(pp$word)%in%stopwordslangs$word[stopwordslangs$lang=="en"][1:500])),]

nn=aggregate(value~word, data=mat3[mat3$sentiment=="negative",],sum)
nn=nn[rev(order(nn$value)),]
nn=nn[-c(which(as.vector(nn$word)%in%stopwordslangs$word[stopwordslangs$lang=="en"][1:500])),]

greyPalette <- c("#2B2B2B", "#373D3F", "#333333", "#303030", "#404040", "#484848", "#505050", "#606060", 
                   "#444444", "#555555", "#666666", "#777777", "#888888", "#999999")

lt="af.jpg"
wordcloud <-wordcloud2(data = pp, figPath = lt, color=rep_len( greyPalette, nrow(textCorpus)))
wordcloud

wordcloud <-wordcloud2(data = nn, figPath = lt, color=rep_len( greyPalette, nrow(textCorpus)))
wordcloud
