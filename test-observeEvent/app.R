library(shiny)
library(reshape2)
library(ggplot2)

opt.DT.simple = list(paging = FALSE, searching = FALSE, ordering = FALSE, info = FALSE)
to.pct <- function(df, decimal = 0) {
    nom.lignes = rownames(df)
    res = sapply(round(df*100, decimal), paste, "%")
    rownames(res) = nom.lignes
    return(res)
}

ui = fluidPage(
    h1("Test de observeEvent"),
    selectInput("choix", "Choix", c("mtcars", "iris")),
    tableOutput("table"),

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
)

server = function(input, output, session) {
    donnees <- reactive({
        get(input$choix)
    })
    
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
    
    output$table <- renderTable({
        head(donnees())
    })
    
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
    
}

shinyApp(ui, server)