library(shiny)
library(DT)

shinyServer(function(input, output, session) {
    donnees <- reactive({
        get(input$choix)
    })
    output$tbl_a = DT::renderDataTable(donnees(), server = FALSE)
    output$tbl_b = DT::renderDataTable(donnees())
}
)
