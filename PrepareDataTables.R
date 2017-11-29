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

quingramDT
quadgramDT[phrase == "is the time"]
dim(quingramDT)
dim(quadgramDT)


## Predict

# Test input text
txt = "The guy in front of me just bought a pound of bacon, a bouquet, and a case of"
txt = "#greatday this is a tweet fuck"

## Convert to ngrams using the last words in the sentence
txtCorpus <- corpus(txt)

txtTokens <- tokens(txtCorpus
                    #, tolower = TRUE
                    , remove_numbers = TRUE
                    , remove_punct = TRUE
                    , remove_separators = TRUE
                    , remove_twitter = TRUE
                    , remove_url = TRUE
                    , verbose = TRUE)

head(txtTokens)

txtTokens <- tokens_select(txtTokens
                           , profanity
                           , selection = "remove"
                           , verbose = quanteda_options("verbose"))

wordCount <- ntoken(txtTokens)

# Prepare the text
# Need to match the ngram data.tables
txtQuad <- tokens_ngrams(txtTokens, n = 4, concatenator = " ")
txtTri <- tokens_ngrams(txtTokens, n = 3, concatenator = " ")
txtBi <- tokens_ngrams(txtTokens, n = 2, concatenator = " ")
txtQuad
txtTri
txtUni
txtQuad <- tail(txtQuad[[1]], 1)
txtTri <- tail(txtTri[[1]], 1)
txtBi <- tail(txtBi[[1]], 1)
txtUni <- tail(txtTokens[[1]], 1)


quinPredict <- quingramDT[phrase == txtQuad][order(-prob)]
quadPredict <- quadgramDT[phrase == txtTri][order(-prob)]
triPredict <- trigramDT[phrase == txtBi][order(-prob)]
biPredict <- bigramDT[phrase == txtUni][order(-docfreq)]

quinPredict[, c("ngram", "phrase", "docfreq") := NULL]
quadPredict[, c("ngram", "phrase", "docfreq") := NULL]
triPredict[, c("ngram", "phrase", "docfreq") := NULL]
biPredict[, c("ngram", "phrase", "docfreq") := NULL]

quadPredict[, prob := 0.4 * prob]
triPredict[, prob := 0.4 * 0.4 * prob]
biPredict[, prob := 0.4 * 0.4 * 0.4 * prob]

quinPredict
quadPredict
triPredict
biPredict

l = list(quinPredict, quadPredict, triPredict, biPredict)
predictWordTemp = as.data.table(rbindlist(l))
predictWord = predictWordTemp[, .(prob = max(prob)), by = .(predict)][1:5]
predictWord




class(predictWord)
key(predictWord)
key(quinPredict)
key(quingramDT)
