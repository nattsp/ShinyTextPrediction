DT <- data.table(x = sample(LETTERS[1:5], 20, TRUE), key = "x"); DT
DT[  , .I[x == "E"] ] 
DT[J("E")  , .I]
