pageWithSidebar(
    headerPanel("actionButton test"),
    sidebarPanel(
        #numericInput("n", "N:", min = 0, max = 100, value = 50),
        textInput("text", "Input text here", "input text"),
        br(),
        actionButton("goButton", "Go!"),
        p("Click the button to update the value displayed in the main panel.")
    ),
    mainPanel(
        splitLayout(
            verbatimTextOutput("nText"),
            tableOutput("predTable")
            
        )
    )
)