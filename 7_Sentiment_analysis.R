#' Ted Kwartler
#' twitter: @tkwartler
#' ODSC Workshop: Intro to Text Mining using R
#' 5-4-17
#' 7.0 Sentiment Analysis

#libraries
library(qdap)
library(tidyr)
library(tidytext)
library(dplyr)
library(radarchart)

#Set the working directory
setwd('C:/Users/Edward/Desktop/odsc blogs/presentation/EAST 2017/data')

##Qdap's Polarity
polarity('R is the best')
polarity('python scripting is ok')

conversation<- data.frame(words=c('R is the best','python is the worst',
                                    'DNNs will solve everything amazing',
                                    'DNNs in python are very cool'),
                          speaker=c('Ted','Ted','Hamel','Hamel'))

polarity(conversation$words)

polarity(conversation$words,conversation$speaker)

##Tidy Sentiment Analysis
data(sentiments)
sentiments

#Stopwords
data(stop_words)
stop_words

#Add stopwords
custom.stopwords<-data.frame(word=c('amp','beer'),lexicon='custom')
stop_words<-rbind(stop_words,custom.stopwords)

#Text
chardonnay<-read.csv('chardonnay.csv')
beer<-read.csv('beer.csv')
coffee<-read.csv('coffee.csv')

#Tibble
tidy.chardonnay<-chardonnay %>%
  unnest_tokens(word, text) %>% mutate(tweet='chardonnay')

tidy.beer<-beer %>%
  unnest_tokens(word, text) %>% mutate(tweet='beer')

tidy.coffee<-coffee %>%
  unnest_tokens(word, text) %>% mutate(tweet='coffee')

all.tidy<-rbind(tidy.chardonnay,tidy.beer,tidy.coffee)

head(all.tidy$word,10)

#Remove stopwords
all.tidy <- all.tidy %>%
  anti_join(stop_words)

#Subset Sentiment Lexicons
nrc.lexicon <- sentiments %>%
  filter(lexicon == "nrc") %>%
  select(-score) 

nrc.lexicon

#Drop Pos/Neg
nrc.lexicon<-nrc.lexicon[!grepl('positive|negative',
                                nrc.lexicon$sentiment),]

#inner join
all.sentiment <- all.tidy %>%
  inner_join(nrc.lexicon) %>%  
  count(tweet,sentiment) %>%
  spread(tweet, n, fill = 0)

#Review
all.sentiment

#Now proportion by column
prop.drinks<-all.sentiment[,2:4] 
for(i in 1:length(prop.drinks[1,])) 
  {prop.drinks[,i] <- prop.table(prop.drinks[,i])}


#check prop
colSums(prop.drinks)

#review
prop.drinks

#Add emotion
prop.drinks$sentiment<-all.sentiment$sentiment


final.df<-data.frame(prop.drinks[,4],
                  prop.drinks[,c('beer',
                                 'chardonnay',
                                 'coffee')])

chartJSRadar(final.df,width = "450", height = "200")

#End