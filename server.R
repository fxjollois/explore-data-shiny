shinyServer(function(input, output, session) {
    
    #############################################
    # Choix des données
    donnees.originales <- reactive({
        if (input$donnees.choix != "fichier") {
            # Données internes
            data.frame(get(input$donnees.choix))
        } else {
            # Données à charger
            if (is.null(input$donnees.fichier.input)) return (NULL)
            don = NULL
            try({
                don = read.table(
                    input$donnees.fichier.input$datapath, 
                    header = input$donnees.fichier.header == "oui", 
                    sep = input$donnees.fichier.sep,
                    dec = input$donnees.fichier.dec,
                    stringsAsFactors = FALSE)
            }, silent = TRUE)
            don
        }
    })
    output$donnees.fichier.ok <- renderUI({
        don = donnees()
        if (is.null(don) & !is.null(input$donnees.fichier.input))
            p(class = "bg-danger", "Impossible de charger le fichier")
    })
    output$donnees.fichier.ui <- renderUI({
        if (input$donnees.choix == "fichier") {
            list(
                fileInput("donnees.fichier.input", "Fichier"),
                radioButtons("donnees.fichier.header", 
                             "Noms de variables",
                             c("oui", "non")),
                radioButtons("donnees.fichier.sep", 
                             "Séparateur", 
                             c("point-virgule" = ";", 
                               "virgule" = ",", 
                               "espace" = " ", 
                               "tabulation" = "\t")),
                radioButtons("donnees.fichier.dec", 
                             "Séparateur de décimales",
                             c("point" = ".", "virgule" = ",")),
                uiOutput("donnees.fichier.ok")
            )
        }
    })
    output$donnees.rendu <- renderDataTable({
        don = donnees.originales()
        if (!is.null(don))
            cbind(" " = rownames(don), don)
    })
    output$donnees.nblignes <- renderText({
        don = donnees.originales()
        if (!is.null(don))
            paste("Nombre de lignes : ", nrow(don))
    })
    output$donnees.nbcolonnes <- renderText({
        don = donnees.originales()
        if (!is.null(don))
            paste("Nombre de colonnes : ", ncol(don))
    })
    
    output$aide <- renderUI({
        if (input$donnees.choix != "fichier") {
            includeHTML(paste("aide/", input$donnees.choix, ".html", sep = ""))
        } else {
            p("Aucune aide à afficher (données externes)")
        }
    })

    #############################################
    # Création de nouvelles variables
    
    nouvord.liste <- list()
    
    observeEvent(
        input$nouvord.ajout,
        {
            e = input$nouvord.ajout
            don = donnees.originales()
            nom.quanti = names(don)[unlist(lapply(don, is.numeric))]
            if (length(nom.quanti) > 0) {
                insertUI(
                    paste0("#nouvord"),
                    "beforeEnd",
                    list(
                        fluidRow(
                            id = paste0("nouvord", e),
                            column(width = 3, textInput(paste0("nouvord", e, ".nom"), "Nom de la variable", paste0("nouvelle", e), placeholder = "donner le nom de la nouvelle variable")),
                            column(width = 3, selectInput(paste0("nouvord", e, ".var"), "Variable de départ", nom.quanti)),
                            column(width = 6, textInput(paste0("nouvord", e, ".cut"), "Découpage", 5, placeholder = "indiquer ici les bornes des intervalles"))
                        ),
                        fluidRow(
                            column(width = 3, tableOutput(paste0("nouvord", e, ".tab"))),
                            column(width = 9, plotOutput(paste0("nouvord", e, ".bar")))
                        )
                    )
                )
            
                output[[paste0("nouvord", e, ".tab")]] <- renderTable({
                    don = donneesAug.ord()
                    table(don[,input[[paste0("nouvord", e, ".nom")]]], useNA = "ifany")
                })
                output[[paste0("nouvord", e, ".bar")]] <- renderPlot({
                    don = donneesAug.ord()
                    barplot(table(don[,input[[paste0("nouvord", e, ".nom")]]], useNA = "ifany"))
                })
            }
        }
    )
    
    donneesAug.ord <- reactive({
        don = donnees.originales()
        
        vars = sub(".var", "", names(input)[grep("nouvord[0-9]+.var", names(input))])
        
        if (length(vars) == 0) { return (don)}
        
        for (v in vars) {
            variable = input[[paste0(v, ".var")]]
            decoupage = as.numeric(strsplit(input[[paste0(v, ".cut")]], ",")[[1]])
            don[input[[paste0(v, ".nom")]]] = cut(don[[variable]], decoupage)
        }
        
        don
    })

    #############################################
    # Mise à jour des variables dans les sélecteurs
    
    observe({
        don = donnees()
        
        if (!is.null(don)) {
            
            nom.quanti = names(don)[unlist(lapply(don, is.numeric))]
            nom.quali = names(don)
            
            # univarié
            updateSelectInput(session, "quanti.var", choices = nom.quanti)
            updateSelectInput(session, "quali.var", choices = nom.quali)
            
            # quanti-quanti
            updateSelectInput(session, "quantiquanti.var1", choices = nom.quanti)
            updateSelectInput(session, "quantiquanti.var2", choices = nom.quanti)
            
            # quali-quali
            updateSelectInput(session, "qualiquali.var1", choices = nom.quali)
            updateSelectInput(session, "qualiquali.var2", choices = nom.quali)
            
            # quali-quanti
            updateSelectInput(session, "qualiquanti.varQl", choices = nom.quali)
            updateSelectInput(session, "qualiquanti.varQt", choices = nom.quanti)
            
            # création de variables
            updateSelectInput(session, "new.var", choices = nom.quanti)
        }
    })
    
    #############################################
    # Description des variables
    output$variables <- renderDataTable({
        don = donnees()
        if (is.null(don)) return(NULL)
        
        res = data.frame(
            check.names = FALSE,
            "Variable" = names(don),
            "Type" = sapply(sapply(don, class), paste, collapse = " "),
            "Nombre de valeurs distinctes" = unlist(lapply(don, function (v) { return (length(unique(v))) })),
            "Valeurs" = unlist(lapply(don, function(v) { 
                    if (is.factor(v) | is.character(v)) {
                        vv = levels(factor(v))
                    } else {
                        vv = v
                    }
                    if (length(vv) > 10)
                        return(paste(paste(vv[1:10], collapse = " "), "..."))
                    else
                        return(paste(vv, collapse = " "))
                }))
        )
        res
    }, options = opt.DT.simple)

    #############################################
    # Sous-populations
    
    donnees.souspop <- reactive({
        don = donneesAug.ord()
        don2 = NULL
        try({
            don2 = eval(parse(text = paste("subset(don, subset =", input$restrict, ")")))
        }, silent = TRUE)
        don2
    })
    output$restrict.ok <- renderUI({
        don = donnees.souspop()
        if (is.null(don))
            p(class="bg-danger", "Vos critères de restriction sont incompatibles avec les données")
        else
            p(class="bg-success", "Critères compatibles")
    })
    output$donnees.restrict <- renderDataTable({
        don = donnees.souspop()
        cbind(" " = rownames(don), don)
    })
    output$restrict.nblignes <- renderText({
        don = donnees.souspop()
        paste("Nombre de lignes : ", nrow(don))
    })

    #############################################
    # Données finales
    
    donnees <- reactive({
        don = donnees.souspop()
        don
    })
    
    #############################################
    # Quantitative
    
    output$quanti.ui <- renderUI({ 
        if (input$quanti.type == 0) {
            # Numérique
            sliderInput("quanti.arrondi", label = "Arrondi", min = 0, max = 5, value = 2)
        } else if (input$quanti.type == 1) {
            # Histogramme
            list(
                radioButtons("quanti.bins.type", "Choix de la répartition", c("Par nombre de bins" = 1, "Par largeur de bins" = 2, "Répartition personnalisée" = 3)),
                uiOutput("quanti.bins.ui")
            )
        } else if (input$quanti.type == 2) {
            # Boîte à moustache
        } else if (input$quanti.type == 3) {
            ## QQ-plot
        } else if (input$quanti.type == 4) {
            ## Fréquences cumulées
        } else if (input$quanti.type == 5) {
            ## Courbe cumulée
            list(
                radioButtons("quanti.courbe.bins.type", "Choix de la répartition", c("Par nombre de bins" = 1, "Par largeur de bins" = 2, "Répartition personnalisée" = 3)),
                checkboxInput("quanti.courbe.hist", "Histogramme empilé"),
                uiOutput("quanti.courbe.bins.ui")
            )
        } 
    })
    output$quanti.bins.ui <- renderUI({
        x = donnees()[,input$quanti.var]
        if (is.null(input$quanti.bins.type)) return(NULL)
        if (input$quanti.bins.type == 1) {
            # Par nombre de bins
            sliderInput("quanti.bins", label = "Nombre de barres", min = 1, max = 50, value = 10)            
        } else if (input$quanti.bins.type == 2) {
            # Par largeur de bins
            textInput("quanti.binwidth", label = "Largeurs de barres", value = ceiling(diff(range(x, na.rm = TRUE)) / 10))
        } else if (input$quanti.bins.type == 3) {
            # Répartition personnalisée
            list(
                helpText(paste("Rappel, l'étendue des données est : ", paste(range(x), collapse = ", "))),
                textInput("quanti.breaks", "Bornes des intervalles")
            )
        }
    })
    output$quanti.courbe.bins.ui <- renderUI({
        x = donnees()[,input$quanti.var]
        if (is.null(input$quanti.courbe.bins.type)) return(NULL)
        if (input$quanti.courbe.bins.type == 1) {
            # Par nombre de bins
            sliderInput("quanti.courbe.bins", label = "Nombre de barres", min = 1, max = 50, value = 10)            
        } else if (input$quanti.courbe.bins.type == 2) {
            # Par largeur de bins
            textInput("quanti.courbe.binwidth", label = "Largeurs de barres", value = ceiling(diff(range(x, na.rm = TRUE)) / 10))
        } else if (input$quanti.courbe.bins.type == 3) {
            # Répartition personnalisée
            list(
                helpText(paste("Rappel, l'étendue des données est : ", paste(range(x), collapse = ", "))),
                textInput("quanti.courbe.breaks", "Bornes des intervalles")
            )
        }
    })
    output$quanti.info <- renderDataTable({
        don = donnees()
        if (is.null(don)) return(NULL)
        if (input$quanti.type != 0) return(NULL)
        x = don[,input$quanti.var]
        mat = rbind(
            c("Nombre de valeurs", length(x)),
            c("Nombre de valeurs manquantes", sum(is.na(x))),
            c("Proportion de valeurs manquantes", to.pct(sum(is.na(x)) / length(x)))
        )
        setNames(data.frame(mat), c("Informations", ""))
    }, options = opt.DT.simple)
    output$quanti.table <- renderDataTable({
        if (input$quanti.type != 0) return(NULL)
        if (is.null(input$quanti.arrondi)) return(NULL)
        x = donnees()[,input$quanti.var]
        res = data.frame(
            "Moyenne" = mean(x, na.rm = TRUE),
            "Ecart-Type" = sd(x, na.rm = TRUE),
            "Variance" = var(x, na.rm = TRUE),
            "Minimum" = min(x, na.rm = TRUE),
            "Q1" = quantile(x, .25, na.rm = TRUE),
            "Médiane" = median(x, na.rm = TRUE),
            "Q3" = quantile(x, .75, na.rm = TRUE),
            "Maximum" = max(x, na.rm = TRUE)
        )
        res = setNames(melt(round(res, input$quanti.arrondi)), c("Statistique", "Valeur"))
        res
    }, options = opt.DT.simple)
    output$quanti.plot <- renderPlot({
        if (input$quanti.type == 1) {
            # Histogramme
            if (is.null(input$quanti.bins.type)) return(NULL)
            if (is.null(input$quanti.bins)) return(NULL)
            if (input$quanti.bins.type == 1) {
                # Par nombre de bins
                ggplot(donnees(), aes_string(input$quanti.var)) + geom_histogram(bins = input$quanti.bins)
            } else if (input$quanti.bins.type == 2) {
                # Par largeur de bins
                if (is.null(input$quanti.binwidth)) return(NULL)
                binwidth = as.numeric(input$quanti.binwidth)
                if (is.na(binwidth)) return(NULL)
                ggplot(donnees(), aes_string(input$quanti.var)) + geom_histogram(binwidth = binwidth)
            } else if (input$quanti.bins.type == 3) {
                # Répartition personnalisée
                if (is.null(input$quanti.breaks)) return(NULL)
                breaks = as.numeric(strsplit(input$quanti.breaks, ",")[[1]])
                x = donnees()[,input$quanti.var]
                xmin = min(x, na.rm = TRUE)
                xmax = max(x, na.rm = TRUE)
                if (is.numeric(breaks) & length(breaks) > 1) 
                    if (max(breaks, na.rm = TRUE) > xmin & min(breaks, na.rm = TRUE) < xmax)
                        ggplot(donnees(), aes_string(input$quanti.var)) + geom_histogram(aes(y = ..density..), breaks = breaks)
            }
        } else if (input$quanti.type == 2) {
            # Boîte à moustache
            ggplot(donnees(), aes_string(1, input$quanti.var)) + geom_boxplot() + xlab("") + ylab("")
        } else if (input$quanti.type == 3) {
            ## QQ-plot
            x = donnees()[,input$quanti.var]
            xy = quantile(x, c(.25, .75), names = FALSE, type = 7, na.rm = TRUE)
            xx = qnorm(c(.25, .75))
            xslope = diff(xy)/diff(xx)
            xint = xy[1] - xslope*xx[1]
            ggplot(donnees(), aes_string(sample = input$quanti.var)) + 
                geom_abline(intercept = xint, slope = xslope, col = "gray80") +
                geom_qq()
        } else if (input$quanti.type == 4) {
            ## Fréquences cumulées
            ggplot(donnees(), aes_string(input$quanti.var)) + stat_ecdf() + xlab("") + ylab("")
        } else if (input$quanti.type == 5) {
            ## Courbes cumulées
            if (is.null(input$quanti.courbe.bins.type)) return(NULL)
            if (is.null(input$quanti.courbe.bins)) return(NULL)
            if (input$quanti.courbe.bins.type == 1) {
                # Par nombre de bins
                temp = print(ggplot(donnees(), aes_string(input$quanti.var)) + 
                                 stat_bin(aes(y=cumsum(..density..)), bins = input$quanti.courbe.bins))
            } else if (input$quanti.courbe.bins.type == 2) {
                # Par largeur de bins
                if (is.null(input$quanti.courbe.binwidth)) return(NULL)
                binwidth = as.numeric(input$quanti.courbe.binwidth)
                if (is.na(binwidth)) return(NULL)
                temp = print(ggplot(donnees(), aes_string(input$quanti.var)) + 
                                 stat_bin(aes(y=cumsum(..density..)), binwidth = binwidth))
            } else if (input$quanti.courbe.bins.type == 3) {
                # Répartition personnalisée
                if (is.null(input$quanti.courbe.breaks)) return(NULL)
                if (input$quanti.courbe.breaks == "") return(NULL)
                breaks = as.numeric(strsplit(input$quanti.courbe.breaks, ",")[[1]])
                x = donnees()[,input$quanti.var]
                xmin = min(x, na.rm = TRUE)
                xmax = max(x, na.rm = TRUE)
                if (is.numeric(breaks) & length(breaks) > 1) 
                    if (max(breaks, na.rm = TRUE) > xmin & min(breaks, na.rm = TRUE) < xmax)
                        temp = print(ggplot(donnees(), aes_string(input$quanti.var)) + 
                                         stat_bin(aes(y = cumsum(..density..)), breaks = breaks))
            }
            g = ggplot()
            data = temp$data[[1]]
            d = data.frame(x = c(data$xmin[1], data$xmax),
                           y = c(0, cumsum(data$density * (data$xmax - data$xmin))))
            if (input$quanti.courbe.hist == 1) {
                # Ajout de l'histogramme empilé en fond
                g = g + geom_rect(data = temp$data[[1]], 
                                  aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = cumsum(density * (xmax - xmin))), 
                                  fill = "gray50")
            }
            g + geom_line(data = d, aes(x, y)) +
                xlab(input$quanti.var) +
                ylab("cumulative freq.")
        }
    })

    #############################################
    # Qualitative
    output$quali.ui <- renderUI({ 
        if (input$quali.type == 0) {
            # Numérique
            list(
                radioButtons("quali.cum", label = "Proportions cumulés", choices = c("Non" = 0, "Oui" = 1)),
                sliderInput("quali.arrondi", label = "Arrondi", min = 0, max = 5, value = 2)
            )
        }
    })
    output$quali.info <- renderDataTable({
        if (input$quali.type != 0) return(NULL)
        x = donnees()[,input$quali.var]
        mat = rbind(
            c("Nombre de valeurs", length(x)),
            c("Nombre de valeurs manquantes", sum(is.na(x))),
            c("Proportion de valeurs manquantes", to.pct(sum(is.na(x)) / length(x)))
        )
        setNames(data.frame(mat), c("Informations", ""))
    }, options = opt.DT.simple)
    output$quali.table <- renderDataTable({
        if (input$quali.type != 0) return(NULL)
        if (is.null(input$quali.arrondi)) return(NULL)
        x = donnees()[,input$quali.var]
        t = table(x)
        m = names(t)
        p = prop.table(t)
        c = cumsum(p)
        res = data.frame(
            "Modalité" = m,
            "Effectifs" = as.vector(t),
            "Proportions" = round(as.vector(p) * 100, input$quali.arrondi)
        )
        if (input$quali.cum == 1)
            res$"Proportions cumulées" = round(as.vector(c) * 100, input$quali.arrondi)
        res
    }, options = opt.DT.simple)
    output$quali.plot <- renderPlot({
        df = data.frame(x = factor(donnees()[,input$quali.var]))
        if (input$quali.type == 1) {
            # Diagramme en barres (en effectifs)
            ggplot(na.omit(df), aes(x, fill = x)) + geom_bar() + 
                xlab("") + ylab("Effectifs") + labs(fill = "")
        } else if (input$quali.type == 2) {
            # Diagramme en barres (en fréquences)
            ggplot(na.omit(df), aes(x, fill = x)) + geom_bar(aes(y = (..count..)/sum(..count..))) + 
                xlab("") + ylab("proportions") + labs(fill = "") + scale_y_continuous(labels = percent)
        } else if (input$quali.type == 3) {
            # Diagramme circulaire
            ggplot(na.omit(df), aes("", fill = x)) + 
                scale_y_continuous(labels = percent) +
                geom_bar(aes(y = (..count..)/sum(..count..)), width = 1) + 
                coord_polar(theta = "y") + 
                xlab("") + ylab("") + labs(fill = input$quali.var) +
                theme(axis.ticks = element_blank())
        }
    })

    #############################################
    # Quanti-Quanti
    observe({
        don = donnees()
        bivar.quanti1 = input$quantiquanti.var1
        nom.quanti = names(don)[unlist(lapply(don, is.numeric))]
        
        updateSelectInput(session, "quantiquanti.var2", choices = nom.quanti[nom.quanti != bivar.quanti1])
    })
    output$quantiquanti.ui <- renderUI({
        if (input$quantiquanti.type == 0) {
            # Numérique
            sliderInput("quantiquanti.arrondi", label = "Arrondi", min = 0, max = 5, value = 2)
        }        
    })
    output$quantiquanti.info <- renderDataTable({
        if (input$quantiquanti.type != 0) return(NULL)
        x = donnees()[,input$quantiquanti.var1]
        y = donnees()[,input$quantiquanti.var2]
        mat = rbind(
            c("Nombre de valeurs", length(x)),
            c("Nombre de valeurs manquantes", sum(is.na(x+y))),
            c("Proportion de valeurs manquantes", to.pct(sum(is.na(x+y)) / length(x)))
        )
        setNames(data.frame(mat), c("Informations", ""))
    }, options = opt.DT.simple)
    output$quantiquanti.table <- renderDataTable({
        if (is.null(input$quantiquanti.arrondi)) return(NULL)
        if (input$quantiquanti.type == 0) {
            x = donnees()[,input$quantiquanti.var1]
            y = donnees()[,input$quantiquanti.var2]
            cov = cov(x, y, use = "complete")
            cor = cor(x, y, use = "complete")
            res = data.frame(
                "Statistique" = c("Covariance", "Corrélation"),
                "Valeur" = round(c(cov, cor), input$quantiquanti.arrondi)
            )
        }
    }, options = opt.DT.simple)
    output$quantiquanti.plot <- renderPlot({
        if (input$quantiquanti.type == 1) {
            # Nuage de points
            ggplot(donnees()) + geom_point(aes_string(input$quantiquanti.var1, input$quantiquanti.var2))
        } else if (input$quantiquanti.type == 2) {
            # Heatmap
            ggplot(donnees()) + geom_bin2d(aes_string(input$quantiquanti.var1, input$quantiquanti.var2))
        }
    })
    
    #############################################
    # Quali-Quali
    observe({
        don = donnees()
        bivar.quali1 = input$qualiquali.var1
        nom.quali = names(don)
        
        updateSelectInput(session, "qualiquali.var2", choices = nom.quali[nom.quali != bivar.quali1])
    })
    output$qualiquali.ui <- renderUI({
        if (input$qualiquali.type == 0) {
            # Numérique
            list(
                radioButtons("qualiquali.tab.type", "Type de tableau",
                             c("Effectifs" = 0, "Proportions" = 1, "Profils ligne" = 2, "Profils colonne" = 3, "Effectifs théoriques" = 4)),
                uiOutput("qualiquali.tab.ui")
            )
        } else if (input$qualiquali.type == 1) {
            # Diagramme en barres
            radioButtons("qualiquali.bar.type", "Type de diagramme",
                         c("Empilées en effectifs" = 0, "Empilées en fréquences" = 1, "Séparées en effectifs" = 2, "Séparées en fréquences" = 3))
        }
    })
    output$qualiquali.tab.ui <- renderUI({
        if (is.null(input$qualiquali.tab.type)) return(NULL)
        if (input$qualiquali.tab.type > 0)
            sliderInput("qualiquali.tab.arrondi", label = "Arrondi", min = 0, max = 5, value = 2)
    })
    output$qualiquali.table <- renderDataTable({
        if (input$qualiquali.type > 0) return(NULL)
        if (is.null(input$qualiquali.tab.type)) return(NULL)
        x = factor(donnees()[,input$qualiquali.var1])
        levels(x) = paste(input$qualiquali.var1, levels(x), sep = "=")
        y = factor(donnees()[,input$qualiquali.var2])
        levels(y) = paste(input$qualiquali.var2, levels(y), sep = "=")
        tab = table(x, y)
        if (input$qualiquali.tab.type == 0) {
            # Effectifs
            rendu = as.data.frame.matrix(tab)
        } else {
            if (is.null(input$qualiquali.tab.arrondi)) return(NULL)
            if (input$qualiquali.tab.type == 1) {
                # Proportions
                rendu = to.pct(as.data.frame.matrix(prop.table(tab)), input$qualiquali.tab.arrondi)
            } else if (input$qualiquali.tab.type == 2) {
                # Profils ligne
                tabprop = as.data.frame.matrix(prop.table(tab, margin = 1))
                tabprop$Total = apply(tabprop, 1, sum)
                rendu = to.pct(tabprop, input$qualiquali.tab.arrondi)
            } else if (input$qualiquali.tab.type == 3) {
                # Profils colonnes
                tabprop = as.data.frame.matrix(prop.table(tab, margin = 2))
                tabprop["Total",] = apply(tabprop, 2, sum)
                rendu = to.pct(tabprop, input$qualiquali.tab.arrondi)
            } else if (input$qualiquali.tab.type == 4) {
                # Effectifs théoriques
                rendu = round(as.data.frame.matrix(chisq.test(tab)$expected), input$qualiquali.tab.arrondi)
            }
        }
        cbind(" " = rownames(rendu), rendu)
    }, options = opt.DT.simple)
    output$qualiquali.plot <- renderPlot({
        if (input$qualiquali.type == 0) return(NULL)
        don = setNames(donnees()[,c(input$qualiquali.var1, input$qualiquali.var2)], c("x", "y"))
        if (is.numeric(don$x)) don$x = factor(don$x)
        if (is.numeric(don$y)) don$y = factor(don$y)
        if (input$qualiquali.type == 1) {
            if (is.null(input$qualiquali.bar.type)) return(NULL)
            g = ggplot(na.omit(don), aes(x, fill = y))
            # Diagramme en barres
            if (input$qualiquali.bar.type == 0)
                g = g + geom_bar()
            else if (input$qualiquali.bar.type == 1)
                g = g + geom_bar(position = "fill") +
                    scale_y_continuous(labels = percent)
            else if (input$qualiquali.bar.type == 2)
                g = g + geom_bar(position = "dodge")
            else if (input$qualiquali.bar.type == 3)
                g = g + geom_bar(aes(y=..count../sum(..count..)), position = "dodge") +
                    scale_y_continuous(labels = percent)
            g + xlab(input$qualiquali.var1) + ylab("") +
                labs(fill = input$qualiquali.var2)
        } else if (input$qualiquali.type == 2) {
            # Heatmap
            ggplot(na.omit(don)) + geom_bin2d(aes(x, y)) + 
                xlab(input$qualiquali.var1) +
                ylab(input$qualiquali.var2)
        }
    })
    
    #############################################
    # Quali-Quanti
    observe({
        don = donnees()
        bivar.ql = input$qualiquanti.varQl
        nom.quanti = names(don)[unlist(lapply(don, is.numeric))]
        
        updateSelectInput(session, "qualiquanti.varQt", choices = nom.quanti[nom.quanti != bivar.ql])
    })
    output$qualiquanti.ui <- renderUI({
        if (input$qualiquanti.type == 0) {
            # Numérique
            sliderInput("qualiquanti.arrondi", label = "Arrondi", min = 0, max = 5, value = 2)
        }
    })
    output$qualiquanti.table <- renderDataTable({
        if (input$qualiquanti.type > 0) return(NULL)
        if (is.null(input$qualiquanti.arrondi)) return(NULL)
        x = donnees()[,input$qualiquanti.varQt]
        z = factor(donnees()[,input$qualiquanti.varQl])
        data.frame(
            check.names = FALSE,
            "Modalités" = paste(input$qualiquanti.varQl, levels(z), sep = "="),
            "Effectifs" = tapply(x, z, length),
            "Moyenne" = round(tapply(x, z, mean, na.rm = TRUE), input$qualiquanti.arrondi),
            "Ecart-Type" = round(tapply(x, z, sd, na.rm = TRUE), input$qualiquanti.arrondi)
        )
    }, options = opt.DT.simple)
    output$qualiquanti.tablebis <- renderDataTable({
        if (input$qualiquanti.type > 0) return(NULL)
        if (is.null(input$qualiquanti.arrondi)) return(NULL)
        x = donnees()[,input$qualiquanti.varQt]
        z = factor(donnees()[,input$qualiquanti.varQl])
        data.frame(
            "Modalités" = paste(input$qualiquanti.varQl, levels(z), sep = "="),
            "Minimum" = round(tapply(x, z, min, na.rm = TRUE), input$qualiquanti.arrondi),
            "Q1" = round(tapply(x, z, quantile, na.rm = TRUE, .25), input$qualiquanti.arrondi),
            "Médiane" = round(tapply(x, z, median, na.rm = TRUE), input$qualiquanti.arrondi),
            "Q3" = round(tapply(x, z, quantile, na.rm = TRUE, .75), input$qualiquanti.arrondi),
            "Maximum" = round(tapply(x, z, max, na.rm = TRUE), input$qualiquanti.arrondi)
        )
    }, options = opt.DT.simple)
    output$qualiquanti.plot <- renderPlot({
        if (input$qualiquanti.type == 0) return(NULL)    
        don = setNames(donnees()[,c(input$qualiquanti.varQl, input$qualiquanti.varQt)], c("z", "x"))
        don$z = factor(don$z)
        if (input$qualiquanti.type == 1) {
            # Histogramme
            ggplot(na.omit(don)) + geom_histogram(aes(x, ..density.., fill=z)) +
                facet_grid(z~.) +
                xlab(input$qualiquanti.varQt) +
                labs(fill = input$qualiquanti.varQl)
        } else if (input$qualiquanti.type == 2) {
            # Boîtes à moustaches
            ggplot(na.omit(don)) + geom_boxplot(aes(z, x, fill=z)) +
                ylab(input$qualiquanti.varQt) + xlab("") +
                labs(fill = input$qualiquanti.varQl)
        } else if (input$qualiquanti.type == 3) {
            # Fréquences cumulées
            ggplot(na.omit(don), aes(x, color=z)) + stat_ecdf() +
                xlab(input$qualiquanti.varQt) +
                ylab("") +
                labs(color = input$qualiquanti.varQl)
        }
    })
    
})
