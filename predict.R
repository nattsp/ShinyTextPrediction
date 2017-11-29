## Predict

#library(tm)
#library(readr)
library(quanteda)
library(tidytext)
library(stringr)
#library(dplyr)
library(data.table)

## Predict

tokensFun <- function(xt){
    txtCorp <- corpus(xt)
    txtTok <- tokens(txtCorp
                        #, tolower = TRUE
                        , remove_numbers = TRUE
                        , remove_punct = TRUE
                        , remove_separators = TRUE
                        , remove_twitter = TRUE
                        , remove_url = TRUE
                        , verbose = TRUE)
    txtTok <- tokens_select(txtTok
                               , profanity
                               , selection = "remove"
                               , verbose = quanteda_options("verbose"))
    return(txtTok)
    
}

preceedingWords <- function(xt, num){
    txtWords <- tokens_ngrams(xt, n = num, concatenator = " ")
    txtWords <- tail(txtWords[[1]], 1)
    return(txtWords)
}

ngramPredict <- function(xt, ngramDT){
    ngramPredict <- ngramDT[phrase == xt][order(-prob)]
    ngramPredict[, c("ngram", "phrase", "docfreq") := NULL]
    return(ngramPredict)
}

predictWordFromNgrams <- function(quin, quad, tri, bi){
    l = list(quin, quad, tri, bi)
    noGuess <- 10
    wordsTemp = as.data.table(rbindlist(l))
    words = wordsTemp[, .(score = max(prob)), by = .(predict)][1:noGuess]
    words
}

ngramGenerator <- function(xt){
    txtCorpus <- tokensFun(xt)
    nWords <- ntoken(txtTokens)[[1]]
    if (nWords > 3){
        txtQuad <- preceedingWords(txtCorpus, 4)
    } else { txtQuad <- "a a a a"}
    if (nWords >2){
        txtTri <- preceedingWords(txtCorpus, 3)
    } else {txtTri <- "a a a"}
    txtBi <- preceedingWords(txtCorpus, 2)
    txtUni <- preceedingWords(txtCorpus, 1)
    list(txtQuad = txtQuad, txtTri = txtTri, txtBi = txtBi, txtUni = txtUni)
}

ngramsPredict <- function(ngrams, quingramDT, quadgramDT, trigramDT, bigramDT){
    quinPredict <- ngramPredict(ngrams$txtQuad, quingramDT)
    quadPredict <- ngramPredict(ngrams$txtTri, quadgramDT)
    triPredict <- ngramPredict(ngrams$txtBi, trigramDT)
    biPredict <- ngramPredict(ngrams$txtUni, bigramDT)
    
    print(quinPredict)
    print(quadPredict)
    print(triPredict)
    print(biPredict)
    
    quadPredict[, prob := 0.4 * prob]
    triPredict[, prob := 0.4 * 0.4 * prob]
    biPredict[, prob := 0.4 * 0.4 * 0.4 * prob]
    
    predictWord <- predictWordFromNgrams(quinPredict, quadPredict, triPredict, biPredict)
    return(predictWord)
}




# Test input text
txt = "The guy in front of me just bought a pound of bacon, a bouquet, and a case of"
txt = "#greatday this is a tweet fuck"
txt = "You're the reason why I smile everyday. Can you follow me please? It would mean the"
txt = "Hey sunshine, can you follow me and make me the"
txt = "Very early observations on the Bills game: Offense still struggling but the"
txt = "Go on a romantic date at the"
txt = "Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my"
txt = "Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some"
txt = "After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little"
txt = "Be grateful for the good times and keep the faith during the"

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

txtTokens

wordCount <- ntoken(txtTokens)

# Prepare the text
# Need to match the ngram data.tables
txtQuad <- tokens_ngrams(txtTokens, n = 4, concatenator = " ")
txtTri <- tokens_ngrams(txtTokens, n = 3, concatenator = " ")
txtBi <- tokens_ngrams(txtTokens, n = 2, concatenator = " ")


txtQuad <- tail(txtQuad[[1]], 1)
txtTri <- tail(txtTri[[1]], 1)
txtBi <- tail(txtBi[[1]], 1)
txtUni <- tail(txtTokens[[1]], 1)

txtQuad
txtTri
txtBi
txtUni


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
predictWord = predictWordTemp[, .(score = max(prob)), by = .(predict)][1:5]
predictWord




class(predictWord)
key(predictWord)
key(quinPredict)
key(quingramDT)

### Steps using functions

txtCorpus <- tokensFun(txt)

wordCount <- ntoken(txtTokens)[[1]]

if (wordCount > 3){
    txtQuad <- preceedingWords(txtCorpus, 4)
}
if (wordCount >2){
    txtTri <- preceedingWords(txtCorpus, 3)
}
txtBi <- preceedingWords(txtCorpus, 2)
txtUni <- preceedingWords(txtCorpus, 1)

if (wordCount > 3){
    quinPredict <- ngramPredict(txtQuad, quingramDT)
}
if (wordCount > 2){
    quadPredict <- ngramPredict(txtTri, quadgramDT)
}
triPredict <- ngramPredict(txtBi, trigramDT)
biPredict <- ngramPredict(txtUni, bigramDT)

predictWord <- predictWordFromNgrams(quinPredict, quadPredict, triPredict, biPredict)

### Even more steps in functions

l <- ngramGenerator(txt)

predictWord <- ngramsPredict(l, quingramDT, quadgramDT, trigramDT, bigramDT)

predictWord
