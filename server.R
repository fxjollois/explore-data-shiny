library(shiny)
library(reshape2)
library(ggplot2)
library(scales)

shinyServer(function(input, output, session) {
    
    #############################################
    # Choix des données
    donnees <- reactive({
        get(input$donnees.choix)
    })
    output$donnees.rendu <- renderDataTable({
        donnees()
    })
    observe({
        don = donnees()
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
        updateSelectInput(session, "qualiquanti.varQl", choices = names(don))
        updateSelectInput(session, "qualiquanti.varQt", choices = nom.quanti)
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
            sliderInput("quanti.bins", label = "Largeurs de barres", min = 1, max = max(x, na.rm = TRUE), value = 1)
        } else if (input$quanti.bins.type == 3) {
            # Répartition personnalisée
            textInput("quanti.breaks", "Bornes des intervalles")
        }
    })
    output$quanti.info <- renderDataTable({
        x = donnees()[,input$quanti.var]
        mat = rbind(
            c("Nombre de valeurs", length(x)),
            c("Nombre de valeurs manquantes", sum(is.na(x))),
            c("Proportion de valeurs manquantes", sum(is.na(x)) / length(x))
        )
        setNames(data.frame(mat), c("Informations", ""))
    }, options = list(paging = FALSE, searching = FALSE, ordering = FALSE, info = FALSE))
    output$quanti.table <- renderDataTable({
        if (is.null(input$quanti.arrondi)) return(NULL)
        if (input$quanti.type == 0) {
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
        }
    }, options = list(paging = FALSE, searching = FALSE, ordering = FALSE, info = FALSE))
    
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
                ggplot(donnees(), aes_string(input$quanti.var)) + geom_histogram(binwidth = input$quanti.bins)
            } else if (input$quanti.bins.type == 3) {
                # Répartition personnalisée
                if (is.null(input$quanti.breaks)) return(NULL)
                breaks = as.numeric(strsplit(input$quanti.breaks, ",")[[1]])
                if (is.numeric(breaks) & length(breaks) > 1) 
                    if (max(breaks, na.rm = TRUE) > min(x, na.rm = TRUE) & min(breaks, na.rm = TRUE) < max(x, na.rm = TRUE))
                        ggplot(donnees(), aes_string(input$quanti.var)) + geom_histogram(aes(y = ..density..), breaks = breaks)
            }
        } else if (input$quanti.type == 2) {
            # Boîte à moustache
            ggplot(donnees(), aes_string(1, input$quanti.var)) + geom_boxplot() + xlab("") + ylab("")
        } else if (input$quanti.type == 3) {
            ## QQ-plot
            ggplot(donnees(), aes_string(sample = input$quanti.var)) + geom_qq()
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
        x = donnees()[,input$quali.var]
        mat = rbind(
            c("Nombre de valeurs", length(x)),
            c("Nombre de valeurs manquantes", sum(is.na(x))),
            c("Proportion de valeurs manquantes", sum(is.na(x)) / length(x))
        )
        setNames(data.frame(mat), c("Informations", ""))
    }, options = list(paging = FALSE, searching = FALSE, ordering = FALSE, info = FALSE))
    output$quali.table <- renderDataTable({
        if (is.null(input$quali.arrondi)) return(NULL)
        if (input$quali.type == 0) {
            x = donnees()[,input$quali.var]
            t = table(x)
            m = names(t)
            p = prop.table(t)
            c = cumsum(p)
            res = data.frame(
                "Modalité" = m,
                "Occurences" = as.vector(t),
                "Proportions" = round(as.vector(p) * 100, input$quali.arrondi)
            )
            if (input$quali.cum == 1)
                res$"Proportions cumulées" = round(as.vector(c) * 100, input$quali.arrondi)
            res
        }
    }, options = list(paging = FALSE, searching = FALSE, ordering = FALSE, info = FALSE))
    output$quali.plot <- renderPlot({
        df = data.frame(x = factor(donnees()[,input$quali.var]))
        if (input$quali.type == 1) {
            # Diagramme en barres
            ggplot(df, aes(x, fill = x)) + geom_bar() + xlab("") + labs(fill = "")
        } else if (input$quali.type == 2) {
            # Diagramme circulaire
            ggplot(df, aes("", fill = x)) + 
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
    }, options = list(paging = FALSE, searching = FALSE, ordering = FALSE, info = FALSE))
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
    
    #############################################
    # Quali-Quanti
    observe({
        don = donnees()
        bivar.ql = input$qualiquanti.varQl
        nom.quanti = names(don)[unlist(lapply(don, is.numeric))]
        
        updateSelectInput(session, "qualiquanti.varQt", choices = nom.quanti[nom.quanti != bivar.ql])
    })
    
})
