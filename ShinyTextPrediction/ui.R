library(shiny)

pageWithSidebar(
    headerPanel("Predict your next word"),
    sidebarPanel(
        textPhraseInput("session1")
    ),
#    sidebarPanel(
#        textPhraseInput("text", "Input text here", "input text"),
#        br(),
#        actionButton("goButton", "Go!"),
#        p("Click the button to update the value displayed in the main panel.")
#    ),
    mainPanel(
        splitLayout(
            #verbatimTextOutput("nText"),
            tableOutput("predTable")
            
        )
    )
)