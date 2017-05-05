#' Ted Kwartler
#' twitter: @tkwartler
#' ODSC Workshop: Intro to Text Mining using R
#' 5-4-17
#' 6.0 Other Wordclouds

#Set the working directory
setwd("~/odsc/EAST 2017/data")

#libraries
library(tm)
library(wordcloud)
library(RColorBrewer)

#options, functions
options(stringsAsFactors = FALSE)
Sys.setlocale('LC_ALL','C')

#try to lower function
tryTolower <- function(x){
  y = NA
  try_error = tryCatch(tolower(x), error = function(e) e)
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}
#Stopwords
custom.stopwords <- c(stopwords('english'), 'chardonnay', 
                      'beer', 'coffee', 'amp','rt')

#Clean & Collapse
corp.collapse<-function(csv.name, text.column.name){
  x <- read.csv(file=csv.name, head=TRUE, sep=",")
  x <-iconv(x[,text.column.name], "latin1","ASCII",sub='')
  x <- VCorpus(VectorSource(x))
  x <- tm_map(x, removePunctuation)
  x <- tm_map(x, removeNumbers)
  x <- tm_map(x, tryTolower)
  x <- tm_map(x, removeWords, custom.stopwords)
  x <- paste(x, collapse=" ")
}

#Apply
chardonnay<-corp.collapse('chardonnay.csv','text')
coffee<-corp.collapse('coffee.csv','text')
beer<-corp.collapse('beer.csv','text')

#combine the columns into a single vector and make into a single corpus
all <- c(chardonnay, coffee, beer)
all.corpus <- VCorpus(VectorSource(all))

#build the combined TDM
all.tdm <- TermDocumentMatrix(all.corpus)

#convert the vector to a matrix
all.tdm <- as.matrix(all.tdm)

#label the matrix columns
colnames(all.tdm) = c("Chardonnay", "Coffee", "Beer")
all.tdm[50:55,1:3]

#Select Color
pal <- brewer.pal(8, "Purples")
pal <- pal[-(1:2)]

#create the word in common word cloud
commonality.cloud(all.tdm, max.words=150, random.order=FALSE,colors=pal)

#create the comparison word cloud
set.seed(1237)
comparison.cloud(all.tdm, max.words=150, random.order=FALSE,
                 title.size=1.0,
                 colors=brewer.pal(ncol(all.tdm),"Dark2"))

#End