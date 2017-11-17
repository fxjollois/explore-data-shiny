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
                    selectize = TRUE,
                    choices = c("mtcars", 
                                "Recensement 85" = "recensement85",
                                "LifeCycleSavings", 
                                "iris", 
                                "msleep (ggplot2)" = "msleep",
                                "diamonds (ggplot2)" = "diamonds",
                                "economics (ggplot2)" = "economics",
                                "txhousing (ggplot2)" = "txhousing",
                                "Fichier à charger" = "fichier")
                    ),
                uiOutput("donnees.fichier.ui"),
                textOutput("donnees.nblignes"),
                textOutput("donnees.nbcolonnes")
            ),
            mainPanel(
                dataTableOutput("donnees.rendu"),
                style = "overflow: scroll;"
            )
        )
    ),
    
    #############################################
    # Description des variables
    tabPanel(
        "Variables",
        tabsetPanel(
            tabPanel(
                "Existantes",
                # dataTableOutput("variables"),
                htmlOutput("variables2"),
                style = "overflow: scroll;"
            ),
            tabPanel(
                "Nouvelles ordinales",
                sidebarLayout(
                    sidebarPanel(
                        actionButton("nouvord.ajout", "Ajout d'une nouvelle variable"),
                        helpText(
                            p("Le découpage d'une variable peut se faire de deux façon :"),
                            tags$ul(
                                tags$li("un entier : nombre d'intervalles (qui seront donc de même taille"),
                                tags$li("une liste de valeurs (séparées par des ',') : valeurs limites des intervalles")
                            )
                        )
                    ),
                    mainPanel(
                        uiOutput("nouvord.ui"),
                        tags$div(id = "nouvord")
                    )
                )
            ),
            tabPanel(
                "Nouvelles qualitatives",
                helpText("En cours de développement")
            )
        )
    ),

    #############################################
    # Choix des données
    tabPanel(
        "Sous-population",
        h3("Choix d'une sous-population"),
        p("Vous pouvez indiquer ici des critères de sélection d'une sous-population des données"),
        sidebarLayout(
            sidebarPanel(
                textInput("restrict", 
                          label = "Condition(s)", 
                          width = "100%",
                          placeholder = "écrire vos critères ici"),
                htmlOutput("restrict.ok"),
                textOutput("restrict.nblignes")
            ),
            mainPanel(
                dataTableOutput("donnees.restrict"),
                style = "overflow: scroll;"
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
                            choices = c("Numérique" = 0, "Histogramme" = 1, "Boîte à moustache" = 2, "QQ plot" = 3, "Fréquences cumulées" = 4, "Courbe cumulée" = 5)
                            ),
                        uiOutput("quanti.ui")
                        ),
                    mainPanel(
                        dataTableOutput("quanti.info"),
                        dataTableOutput("quanti.table"),
                        plotOutput("quanti.plot"),
                        style = "overflow: scroll;"
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
                        plotOutput("quali.plot"),
                        style = "overflow: scroll;"
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
                        plotOutput("quantiquanti.plot"),
                        style = "overflow: scroll;"
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
                        dataTableOutput("qualiquali.indicateurs"),
                        plotOutput("qualiquali.plot"),
                        style = "overflow: scroll;"
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
                            choices = c("Numérique" = 0, "Histogramme" = 1, "Boîtes à moustaches" = 2, "Fréquences cumulées" = 3)
                        ),
                        uiOutput("qualiquanti.ui")
                    ),
                    mainPanel(
                        dataTableOutput("qualiquanti.table"),
                        dataTableOutput("qualiquanti.tablebis"),
                        dataTableOutput("qualiquanti.indicateurs"),
                        plotOutput("qualiquanti.plot"),
                        style = "overflow: scroll;"
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
        ),
        tabPanel(
            "Aide sur les données",
            uiOutput("aide")
        )
    )
))
