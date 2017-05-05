#' Ted Kwartler
#' twitter: @tkwartler
#' ODSC Workshop: Intro to Text Mining using R
#' 5-4-17
#' 1.0 Install Packages

install.packages(c('tm', 'stringi','lexicon',
                   'ggplot2','ggthemes','wordcloud',
                   'RColorBrewer','plyr','stringr',
                   'topicmodels','portfolio','openNLP',
                   'tidyr','tidytext','dplyr','radarchart',
                   'qdap', 'SnowballC', 'dendextend'), 
                 repos = "http://cran.r-project.org", 
                 dependencies = c("Depends", "Imports", "Suggests"))

install.packages('openNLPmodels.en', 
                 repos = "http://datacube.wu.ac.at/", 
                 type = "source")

#Depending on your machine, you may be able to use parallel processing. 
#For windows users, add the package "doParallel" to the list above. 
#Mac and Linux uses can add "doMC" instead.

#End