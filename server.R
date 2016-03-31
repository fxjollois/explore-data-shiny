library(shiny)
library(reshape2)
library(ggplot2)

shinyServer(function(input, output, session) {
    
    # Choix des données
    donnees <- reactive({
        get(input$donnees.choix)
    })
    output$donnees.rendu <- renderDataTable({
        donnees()
    })
    observe({
        don = donnees()
        updateSelectInput(session, "quanti.var", choices = names(don)[unlist(lapply(don, is.numeric))])
        updateSelectInput(session, "quali.var", choices = names(don))
    })
    
    # Quantitative
    output$quanti.ui <- renderUI({ 
        if (input$quanti.type == 0) {
            # Numérique
            sliderInput("quanti.arrondi", label = "Arrondi", min = 0, max = 5, value = 2)
        } else if (input$quanti.type == 1) {
            # Histogramme
            sliderInput("quanti.bins", label = "Nombre de barres", min = 1, max = 50, value = 30)
        } else if (input$quanti.type == 2) {
            # Boîte à moustache
        } else if (input$quanti.type == 3) {
            ## QQ-plot
        }
    })
    output$quanti.table <- renderDataTable({
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
    }, options = list(paging = FALSE, searching = FALSE, ordering = FALSE))
    output$quanti.plot <- renderPlot({
        if (input$quanti.type == 1) {
            # Histogramme
            ggplot(donnees(), aes_string(input$quanti.var)) + geom_histogram(bins = input$quanti.bins)
        } else if (input$quanti.type == 2) {
            # Boîte à moustache
            ggplot(donnees(), aes_string(1, input$quanti.var)) + geom_boxplot() + xlab("") + ylab("")
        } else if (input$quanti.type == 3) {
            ## QQ-plot
            ggplot(donnees(), aes_string(sample = input$quanti.var)) + geom_qq()
        }
    })
    
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
    output$quali.table <- renderDataTable({
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
    }, options = list(paging = FALSE, searching = FALSE, ordering = FALSE))
    output$quali.plot <- renderPlot({
        df = data.frame(x = factor(donnees()[,input$quali.var]))
        if (input$quali.type == 1) {
            # Diagramme en barres
            ggplot(df, aes(x, fill = x)) + geom_bar() + xlab("") + labs(fill = "")
        } else if (input$quali.type == 2) {
            # Diagramme circulaire
            ggplot(df, aes(1, fill = x)) + geom_bar() + coord_polar(theta = "y") + xlab("") + ylab("") + labs(fill = "")
        }
    })
})
