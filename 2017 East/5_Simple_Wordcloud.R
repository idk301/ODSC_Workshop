#' Ted Kwartler
#' twitter: @tkwartler
#' ODSC Workshop: Intro to Text Mining using R
#' 5-4-17
#' 5.0 Simple Wordcloud

#Set the working directory
setwd('C:/Users/Edward/Desktop/odsc blogs/presentation/EAST 2017/data')

#libraries
library(tm)
library(wordcloud)
library(RColorBrewer)

#options, functions
options(stringsAsFactors = FALSE) 
Sys.setlocale('LC_ALL','C') 

tryTolower <- function(x){
  y = NA
  try_error = tryCatch(tolower(x), error = function(e) e)
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
custom.stopwords <- c(stopwords('english'), 'lol', 'smh', 'amp', 'chardonnay')

#bigram token maker
bigram.tokenizer <-function(x){
  unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
  }

#bring in some text
text<-read.csv('chardonnay.csv', header=TRUE)

#Keep the meta data, apply the functions to make a clean corpus
custom.reader <- readTabular(mapping=list(content="text", id="id"))
corpus <- VCorpus(DataframeSource(text), 
                  readerControl=list(reader=custom.reader))
corpus<-clean.corpus(corpus)

#Make a Document Term Matrix or Term Document Matrix depending on analysis
tdm<-TermDocumentMatrix(corpus, control=list(tokenize=bigram.tokenizer))
tdm.m <- as.matrix(tdm)
tdm.m[1000:1005,1:10]

tdm.v <- sort(rowSums(tdm.m),decreasing=TRUE)
tdm.df <- data.frame(word = names(tdm.v),freq=tdm.v)

#look at all available color pallettes & choose one
display.brewer.all()
pal <- brewer.pal(8, "Blues")
pal <- pal[-(1:2)]

#create a simple word cloud
set.seed(1234)
wordcloud(tdm.df$word,tdm.df$freq,max.words=50, random.order=FALSE, colors=pal)

#End