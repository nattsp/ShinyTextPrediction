library(shiny)
library(quanteda)
library(tidytext)
library(stringr)
library(data.table)

## Action buttons should always be used with one of eventReactive() or observeEvent(). 

function(input, output) {
    # load the phrase and prediction data
    load(file = "babyDT.RData")
    load(file = "quingramDT.RData")
    load(file = "quadgramDT.RData")
    load(file = "trigramDT.RData")
    load(file = "bigramDT.RData")
    
    # define my functions
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
        txtTok <- tokens_tolower(txtTok)
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
        
        
        quadPredict[, prob := 0.4 * prob]
        triPredict[, prob := 0.4 * 0.4 * prob]
        biPredict[, prob := 0.4 * 0.4 * 0.4 * prob]
        
        predictWord <- predictWordFromNgrams(quinPredict, quadPredict, triPredict, biPredict)
        return(predictWord)
    }
    
    mainPrediction <- function(txt, quingramDT, quadgramDT, trigramDT, bigramDT){
        l <- ngramGenerator(txt)
        
        predictWord <- ngramsPredict(l, quingramDT, quadgramDT, trigramDT, bigramDT)
        return(predictWord)
    }

    
    # builds a reactive expression that only invalidates 
    # when the value of input$goButton becomes out of date 
    # (i.e., when the button is pressed)
    ntext <- eventReactive(input$goButton, {
        input$text
    })
    
    wordPredict <- mainPrediction(ntext, quingramDT, quadgramDT, trigramDT, bigramDT)[, .(predict)]
    
    output$nText <- renderText({
        ntext()
    })
    
    output$predTable <- renderTable({
        as.data.frame(
            wordPredict
        )
    })
    
#    output$predTable <- renderTable({
#        as.data.frame(
#            babyDT[phrase == ntext()][order(-prop)][, .(predict)]
#        )
#    })
}