## Prepare data.tables

#library(tm)
#library(readr)
library(quanteda)
library(tidytext)
library(stringr)
#library(dplyr)
library(data.table)


load(file = "../../Data/quingramDT.RData")
load(file = "../../Data/quadgramDT.RData")
load(file = "../../Data/trigramDT.RData")
load(file = "../../Data/bigramDT.RData")
load(file = "../../Data/unigramDT.RData")

load(file = "../../Data/profanity.RData")

quingramDT

setkey(quingramDT, phrase, ngram)
setkey(quadgramDT, phrase, ngram)
setkey(trigramDT, phrase, ngram)
setkey(bigramDT, phrase, ngram)
setkey(unigramDT, ngram)


# Inner join of data tables
quingramDT[quadgramDT, nomatch=0, on = c(phrase = "ngram")]

#add a new column
quingramDT[quadgramDT, prob := docfreq/i.docfreq, on = c(phrase = "ngram")]
quadgramDT[trigramDT, prob := docfreq/i.docfreq, on = c(phrase = "ngram")]
trigramDT[bigramDT, prob := docfreq/i.docfreq, on = c(phrase = "ngram")]
bigramDT[unigramDT, prob := docfreq/i.docfreq, on = c(phrase = "ngram")]

save(quingramDT, file = "ShinyTextPrediction/quingramDT.RData")
save(quadgramDT, file = "ShinyTextPrediction/quadgramDT.RData")
save(trigramDT, file = "ShinyTextPrediction/trigramDT.RData")
save(bigramDT, file = "ShinyTextPrediction/bigramDT.RData")
save(unigramDT, file = "ShinyTextPrediction/unigramDT.RData")

quingramDT
quadgramDT[phrase == "is the time"]
dim(quingramDT)
dim(quadgramDT)

