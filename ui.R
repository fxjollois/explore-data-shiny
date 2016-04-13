library(shiny)

shinyUI(navbarPage(
    "Explore Data",
    
    #############################################
    # Choix des données
    tabPanel(
        "Données",
        h3("Choix des données"),
        p("Choississez le jeu de données que vous souhaitez analyser dans le menu ci-dessous. Vous verrez un aperçu de celui-ci dans la partie droite."),
        sidebarLayout(
            sidebarPanel(
                selectInput(
                    "donnees.choix", 
                    label = "Jeu de données",
                    choices = c("mtcars", "LifeCycleSavings", "iris", "msleep (ggplot2)" = "msleep")
                    )
            ),
            mainPanel(
                dataTableOutput("donnees.rendu")
            )
        )
    ),
    
    #############################################
    # Description univariée
    tabPanel(
        "Univarié",
        h3("Description de variable"),
        tabsetPanel(
            #############################################
            tabPanel(
                "Quantitative",
                p("Vous pouvez choisir ce-dessous la variable numérique à analyser."),
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
                        dataTableOutput("quanti.info"),
                        dataTableOutput("quanti.table"),
                        plotOutput("quanti.plot")
                        )
                    )
                ),
            #############################################
            tabPanel(
                "Qualitative",
                p("Vous pouvez choisir ce-dessous la variable à analyser."),
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
                        dataTableOutput("quali.info"),
                        dataTableOutput("quali.table"),
                        plotOutput("quali.plot")
                    )
                )
            )
        )
    ),
    
    #############################################
    tabPanel(
        "Bivarié",
        h3("Lien entre deux variables"),
        tabsetPanel(
            #############################################
            tabPanel(
                "Quanti-Quanti",
                p("Vous pouvez choisir ci-dessous les deux variables numériques à analyser."),
                sidebarLayout(
                    sidebarPanel(
                        selectInput(
                            "quantiquanti.var1",
                            label = "Variable 1",
                            choices = NULL
                        ),
                        selectInput(
                            "quantiquanti.var2",
                            label = "Variable 2",
                            choices = NULL
                        ),
                        selectInput(
                            "quantiquanti.type", 
                            label = "Représentation", 
                            choices = c("Numérique" = 0, "Nuages de points" = 1, "Heatmap" = 2)
                        ),
                        uiOutput("quantiquanti.ui")
                    ),
                    mainPanel(
                        dataTableOutput("quantiquanti.info"),
                        dataTableOutput("quantiquanti.table"),
                        plotOutput("quantiquanti.plot")
                    )
                )
            ),
            #############################################
            tabPanel(
                "Quali-Quali",
                p("Vous pouvez choisir ci-dessous les deux variables à analyser"),
                sidebarLayout(
                    sidebarPanel(
                        selectInput(
                            "qualiquali.var1",
                            label = "Variable 1",
                            choices = NULL
                        ),
                        selectInput(
                            "qualiquali.var2",
                            label = "Variable 2",
                            choices = NULL
                        ),
                        selectInput(
                            "qualiquali.type", 
                            label = "Représentation", 
                            choices = c("Numérique" = 0, "Diagramme en barres" = 1, "Heatmap" = 2)
                        ),
                        uiOutput("qualiquali.ui")
                    ),
                    mainPanel(
                        dataTableOutput("qualiquali.table"),
                        plotOutput("qualiquali.plot")
                    )
                )
            ),
            #############################################
            tabPanel(
                "Quali-Quanti",
                p("Vous pouvez choisir ci-dessous la variable numérique et la variable à analyser."),
                sidebarLayout(
                    sidebarPanel(
                        selectInput(
                            "qualiquanti.varQl",
                            label = "Variable qualitative",
                            choices = NULL
                        ),
                        selectInput(
                            "qualiquanti.varQt",
                            label = "Variable quantitative",
                            choices = NULL
                        ),
                        selectInput(
                            "qualiquanti.type", 
                            label = "Représentation", 
                            choices = c("Numérique" = 0, "Histogramme" = 1, "Boîtes à moustaches" = 2)
                        ),
                        uiOutput("qualiquanti.ui")
                    ),
                    mainPanel(
                        dataTableOutput("qualiquanti.table"),
                        plotOutput("qualiquanti.plot")
                    )
                )        
            )
            
        )
    ),
    
    #############################################
    # Plus d'informations
    navbarMenu(
        "Plus",
        tabPanel(
            "Informations",
            includeMarkdown("README.md")
            )
        )
))
