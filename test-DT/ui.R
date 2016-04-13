library(shiny)

shinyUI(navbarPage(
        title = 'Use the DT package in shiny',
        tabPanel("choix",
                 selectInput("choix", "Données",
                             c("mtcars", "iris", "LifeCycleSavings"))),
        tabPanel("client",
                h1('A Table Using Client-side Processing'),
                DT::dataTableOutput('tbl_a')
        ),
        tabPanel("server",
                 h1('A Table Using Server-side Processing'),
                 DT::dataTableOutput('tbl_b')
        )
    ))
