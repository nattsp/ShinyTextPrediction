library(quanteda)
library(tidytext)
library(stringr)
library(data.table)


# load the phrase and prediction data
load(file = "./Data/babyDT.RData")
load(file = "./Data/quingramDT.RData")
load(file = "./Data/quadgramDT.RData")
load(file = "./Data/trigramDT.RData")
load(file = "./Data/bigramDT.RData")


# Module UI function
textPhraseInput <- function(id) {
    # Create a namespace function using the provided id
    ns <- NS(id)
    tagList(
        textInput(ns("text"), "Input text here", "input text"),
        br(),
        actionButton(ns("goButton"), "Go!"),
        p("Click here to predict the next word in your sentence.")
    )
}

#
# define my functions
#

tokensFun <- function(input, output, id, xt){
    txtCorp <- corpus(xt)
    txtTok <- tokens(txtCorp
                     #, tolower = TRUE
                     , remove_numbers = TRUE
                     , remove_punct = TRUE
                     , remove_separators = TRUE
                     , remove_twitter = TRUE
                     , remove_url = TRUE
                     , verbose = TRUE)
    txtTok <- tokens_tolower(txtTok)
    txtTok <- tokens_select(txtTok
                            , profanity
                            , selection = "remove"
                            , verbose = quanteda_options("verbose"))
    return(txtTok)
    
}

preceedingWords <- function(input, output, session, xt, num){
    txtWords <- tokens_ngrams(xt, n = num, concatenator = " ")
    txtWords <- tail(txtWords[[1]], 1)
    return(txtWords)
}

ngramPredict <- function(input, output, id, xt, ngramDT){
    ngramPredict <- ngramDT[phrase == xt][order(-prob)]
    ngramPredict[, c("ngram", "phrase", "docfreq") := NULL]
    return(ngramPredict)
}

predictWordFromNgrams <- function(input, output, id, quin, quad, tri, bi){
    l = list(quin, quad, tri, bi)
    noGuess <- 6
    wordsTemp <- as.data.table(rbindlist(l))
    words <- wordsTemp[, .(score = max(prob)), by = .(predict)][1:noGuess]
    words
}

ngramGenerator <- function(input, output, id, xt){
    txtCorpus <- callModule(tokensFun, "inner8", xt)
    nWords <- ntoken(txtTokens)[[1]]
    # if (nWords > 3){
    #     txtQuad <- preceedingWords(txtCorpus, 4)
    # } else { txtQuad <- "a a a a"}
    # if (nWords >2){
    #     txtTri <- preceedingWords(txtCorpus, 3)
    # } else {txtTri <- "a a a"}
    # txtBi <- preceedingWords(txtCorpus, 2)
    # txtUni <- preceedingWords(txtCorpus, 1)
    
    if (nWords > 3){
        txtQuad <- callModule(preceedingWords, "inner9",
                              txtCorpus, 4)
    } else { txtQuad <- "a a a a"}
    if (nWords >2){
        txtTri <- callModule(preceedingWords, "inner10",
                             txtCorpus, 3)
    } else {txtTri <- "a a a"}
    txtBi <- callModule(preceedingWords, "inner11",
                        txtCorpus, 2)
    txtUni <- callModule(preceedingWords, "inner12",
                         txtCorpus, 1)
    list(txtQuad = txtQuad, txtTri = txtTri, txtBi = txtBi, txtUni = txtUni)
}

ngramsPredict <- function(input, output, id, 
                          ngrams, quingramDT, quadgramDT, trigramDT, bigramDT){
    # quinPredict <- ngramPredict(ngrams$txtQuad, quingramDT)
    # quadPredict <- ngramPredict(ngrams$txtTri, quadgramDT)
    # triPredict <- ngramPredict(ngrams$txtBi, trigramDT)
    # biPredict <- ngramPredict(ngrams$txtUni, bigramDT)
    quinPredict <- callModule(ngramPredict, "inner3",
                              ngrams$txtQuad, quingramDT)
    quadPredict <- callModule(ngramPredict, "inner4",
                              ngrams$txtTri, quadgramDT)
    triPredict <- callModule(ngramPredict, "inner5",
                             ngrams$txtBi, trigramDT)
    biPredict <- callModule(ngramPredict, "inner6",
                            ngrams$txtUni, bigramDT)
    
    
    quadPredict[, prob := 0.4 * prob]
    triPredict[, prob := 0.4 * 0.4 * prob]
    biPredict[, prob := 0.4 * 0.4 * 0.4 * prob]
    
    # predictWord <- predictWordFromNgrams(quinPredict, quadPredict, triPredict, biPredict)
    
    predictWord <- callModule(predictWordFromNgrams, "inner7",
                              quinPredict, quadPredict, triPredict, biPredict)
    return(predictWord)
}

textPhrase <- function(input, output, id, quingramDT, quadgramDT, trigramDT, bigramDT){
    #ntext <- eventReactive(input$goButton, {
    #    input$text
    #})
    
    #l <- callModule(ngramGenerator, "inner1", ntext)
    
    predictWord <- eventReactive(input$goButton, {
        ntext <- input$text
        l <- callModule(ngramGenerator, "inner1", ntext)
        callModule(ngramsPredict, "inner2", 
                              l, quingramDT, quadgramDT, trigramDT, bigramDT)
    })
    
    return(predictWord)
}

