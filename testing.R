DT <- data.table(x = sample(LETTERS[1:5], 20, TRUE), key = "x"); DT
DT[  , .I[x == "E"] ] 
DT[J("E")  , .I]


quingramTrain <- dfm(
    TrainSentences
    , tolower = TRUE
    , remove_numbers = TRUE
    , remove_punct = TRUE
    , stem = FALSE
    , ngrams = 5
    , remove_twitter = TRUE
    , remove_url = TRUE
    , concatenator = " "
    , verbose = TRUE)

# Test input text
txt = "The guy in front of me just bought a pound of bacon, a bouquet, and a case of"
txt = "#greatday this is a tweet fuck"

## Convert to ngrams using the last words in the sentence
txtCorpus <- corpus(txt)

txtTokens <- tokens(txtCorpus
                    , remove_numbers = TRUE
                    , remove_punct = TRUE
                    , remove_separators = TRUE
                    , remove_twitter = TRUE)
head(txtTokens)