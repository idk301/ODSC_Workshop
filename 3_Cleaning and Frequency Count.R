#' Ted Kwartler
#' twitter: @tkwartler
#' ODSC Workshop: Intro to Text Mining using R
#' 5-4-17
#' 3.0 Basics: Cleaning and Frequency Count

#Set the working directory
setwd('C:/Users/Edward/Desktop/odsc blogs/presentation/EAST 2017/data')

#libraries
library(tm)
library(qdap)
library(lexicon)

#options, functions
options(stringsAsFactors = FALSE) 
Sys.setlocale('LC_ALL','C') 

tryTolower <- function(x){
  # return NA when there is an error
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error = function(e) e)
  # if not an error
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}

clean.corpus<-function(corpus){
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, removeWords, custom.stopwords)
  return(corpus)
}

#Create custom stop words
custom.stopwords <- c(stopwords('english'), 'lol', 'smh')

#bring in some text
text<-read.csv('coffee.csv', header=TRUE)

#Poor Man's Lemmatization
data(hash_lemmas)
#text$text <- mgsub(hash_lemmas$token,hash_lemmas$lemma,text$text)
#write.csv(text,'lemma_text.csv', row.names = F)
text<-read.csv('lemma_text.csv')

#Keep the meta data, apply the functions to make a clean corpus
custom.reader <- readTabular(mapping=list(content="text", id="id"))
corpus <- VCorpus(DataframeSource(text), 
                  readerControl=list(reader=custom.reader))
corpus<-clean.corpus(corpus)

#Check Meta Data
corpus[[1]]
corpus[[1]][1]
corpus[[1]][2]

#Make a Document Term Matrix or Term Document Matrix depending on analysis
dtm<-DocumentTermMatrix(corpus)
tdm<-TermDocumentMatrix(corpus)
dtm.tweets.m<-as.matrix(dtm)
tdm.tweets.m<-as.matrix(tdm)

dtm.tweets.m[15:16,1275:1278]
tdm.tweets.m[1275:1278,15:16]

#End