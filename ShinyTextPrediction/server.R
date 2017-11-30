library(shiny)


## Action buttons should always be used with one of eventReactive() or observeEvent(). 

function(input, output) {

    
    # builds a reactive expression that only invalidates 
    # when the value of input$goButton becomes out of date 
    # (i.e., when the button is pressed)
    ntext <- eventReactive(input$goButton, {
        input$text
    })
    
    #wordPredict <- mainPrediction(ntext, quingramDT, quadgramDT, trigramDT, bigramDT)[, .(predict)]
    
    output$nText <- renderText({
        ntext()
    })
    
#    output$predTable <- renderTable({
#        as.data.frame(
#            wordPredict
#        )
#    })
    
    output$predTable <- renderTable({
        as.data.frame(
            babyDT[phrase == ntext()][order(-prop)][, .(predict)]
        )
    })
}