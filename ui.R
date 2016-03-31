library(shiny)

shinyUI(navbarPage(
    "Explore Data",
    
    # Choix des données
    tabPanel(
        "Données",
        sidebarLayout(
            sidebarPanel(
                selectInput(
                    "donnees.choix", 
                    label = "Choix du jeu de données",
                    choices = c("mtcars", "LifeCycleSavings", "iris")
                    )
            ),
            mainPanel(
                dataTableOutput("donnees.rendu")
            )
        )
    ),
    
    tabPanel("Univarié",
        h3("Description de variable"),
        tabsetPanel(
            tabPanel("Quantitative",
                sidebarLayout(
                    sidebarPanel(
                        selectInput(
                            "quanti.var",
                            label = "Variable",
                            choices = NULL
                        ),
                        selectInput(
                            "quanti.type", 
                            label = "Représentation", 
                            choices = c("Numérique" = 0, "Histogramme" = 1, "Boîte à moustache" = 2, "QQ plot" = 3)
                            ),
                        uiOutput("quanti.ui")
                        ),
                    mainPanel(
                        dataTableOutput("quanti.table"),
                        plotOutput("quanti.plot")
                        )
                    )
                ),
            tabPanel("Qualitative",
                sidebarLayout(
                    sidebarPanel(
                        selectInput(
                            "quali.var",
                            label = "Variable",
                            choices = NULL
                        ),
                        selectInput(
                            "quali.type", 
                            label = "Représentation", 
                            choices = c("Numérique" = 0, "Diagramme en barres" = 1, "Diagramme circulaire" = 2)
                        ),
                        uiOutput("quali.ui")
                    ),
                    mainPanel(
                        dataTableOutput("quali.table"),
                        plotOutput("quali.plot")
                    )
                )
            )
        )
    ),
    
    # Plus d'informations
    navbarMenu(
        "Plus",
        tabPanel(
            "Informations",
            includeMarkdown("README.md")
            )
        )
))
