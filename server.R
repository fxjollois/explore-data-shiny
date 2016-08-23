library(shiny)
library(reshape2)
library(ggplot2)
library(scales)
library(httr)

# Options pour l'affichage d'un DataTable très basique
opt.DT.simple = list(paging = FALSE, searching = FALSE, ordering = FALSE, info = FALSE)

# Fonction permettant d'afficher des pourcentages au lieu de proportions
to.pct <- function(df, decimal = 0) {
    nom.lignes = rownames(df)
    res = sapply(round(df*100, decimal), paste, "%")
    rownames(res) = nom.lignes
    return(res)
}
# to.pct(as.data.frame.matrix(prop.table(table(mtcars$cyl, mtcars$am))))

shinyServer(function(input, output, session) {
    
    #############################################
    # Choix des données
    donnees <- reactive({
        get(input$donnees.choix)
    })
    output$donnees.rendu <- renderDataTable({
        don = donnees()
        cbind(" " = rownames(don), don)
    })
    output$donnees.nblignes <- renderText({
        don = donnees()
        paste("Nombre de lignes : ", nrow(don))
    })
    output$donnees.nbcolonnes <- renderText({
        don = donnees()
        paste("Nombre de colonnes : ", ncol(don))
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
    
    output$aide <- renderUI({
        includeHTML(paste("aide/", input$donnees.choix, ".html", sep = ""))
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
        if (input$quanti.type != 0) return(NULL)
        x = donnees()[,input$quanti.var]
        mat = rbind(
            c("Nombre de valeurs", length(x)),
            c("Nombre de valeurs manquantes", sum(is.na(x))),
            c("Proportion de valeurs manquantes", sum(is.na(x)) / length(x))
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
                ggplot(donnees(), aes_string(input$quanti.var)) + geom_histogram(binwidth = input$quanti.bins)
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
        } else if (input$quali.type == 1) {
            # Diagramme en barres
            radioButtons("quali.bar.type", label = "Représentation", choices = c("Occurences" = 1, "Proportions" = 2))
        }
    })
    output$quali.info <- renderDataTable({
        if (input$quali.type != 0) return(NULL)
        x = donnees()[,input$quali.var]
        mat = rbind(
            c("Nombre de valeurs", length(x)),
            c("Nombre de valeurs manquantes", sum(is.na(x))),
            c("Proportion de valeurs manquantes", sum(is.na(x)) / length(x))
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
            "Occurences" = as.vector(t),
            "Proportions" = round(as.vector(p) * 100, input$quali.arrondi)
        )
        if (input$quali.cum == 1)
            res$"Proportions cumulées" = round(as.vector(c) * 100, input$quali.arrondi)
        res
    }, options = opt.DT.simple)
    output$quali.plot <- renderPlot({
        df = data.frame(x = factor(donnees()[,input$quali.var]))
        if (input$quali.type == 1) {
            if (is.null(input$quali.bar.type)) return(NULL)
            # Diagramme en barres
            if (input$quali.bar.type == 1)
                ggplot(df, aes(x, fill = x)) + geom_bar() + 
                    xlab("") + ylab("occurences") + labs(fill = "")
            else
                ggplot(df, aes(x, fill = x)) + geom_bar(aes(y = (..count..)/sum(..count..))) + 
                    xlab("") + ylab("proportions") + labs(fill = "") + scale_y_continuous(labels = percent)
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
    output$quantiquanti.info <- renderDataTable({
        if (input$quantiquanti.type != 0) return(NULL)
        x = donnees()[,input$quantiquanti.var1]
        y = donnees()[,input$quantiquanti.var2]
        mat = rbind(
            c("Nombre de valeurs", length(x)),
            c("Nombre de valeurs manquantes", sum(is.na(x+y))),
            c("Proportion de valeurs manquantes", sum(is.na(x+y)) / length(x))
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
                             c("Occurences" = 0, "Proportions" = 1, "Profils ligne" = 2, "Profils colonne" = 3)),
                uiOutput("qualiquali.tab.ui")
            )
        } else if (input$qualiquali.type == 1) {
            # Diagramme en barres
            radioButtons("qualiquali.bar.type", "Type de diagramme",
                         c("Empilées" = 0, "Empilées à 100%" = 1, "Séparées" = 2))
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
            # Occurences
            as.data.frame.matrix(tab)
        } else {
            if (is.null(input$qualiquali.tab.arrondi)) return(NULL)
            if (input$qualiquali.tab.type == 1) {
                # Proportions
                to.pct(as.data.frame.matrix(prop.table(tab)), input$qualiquali.tab.arrondi)
            } else if (input$qualiquali.tab.type == 2) {
                # Profils ligne
                to.pct(as.data.frame.matrix(prop.table(tab, margin = 1)), input$qualiquali.tab.arrondi)
            } else if (input$qualiquali.tab.type == 3) {
                # Profils colonnes
                to.pct(as.data.frame.matrix(prop.table(tab, margin = 2)), input$qualiquali.tab.arrondi)
            }
        }
    }, options = opt.DT.simple)
    output$qualiquali.plot <- renderPlot({
        if (input$qualiquali.type == 0) return(NULL)
        don = setNames(donnees()[,c(input$qualiquali.var1, input$qualiquali.var2)], c("x", "y"))
        if (is.numeric(don$x)) don$x = factor(don$x)
        if (is.numeric(don$y)) don$y = factor(don$y)
        if (input$qualiquali.type == 1) {
            if (is.null(input$qualiquali.bar.type)) return(NULL)
            g = ggplot(don, aes(x, fill = y))
            # Diagramme en barres
            if (input$qualiquali.bar.type == 0)
                g = g + geom_bar()
            else if (input$qualiquali.bar.type == 1)
                g = g + geom_bar(position = "fill") +
                    scale_y_continuous(labels = percent)
            else if (input$qualiquali.bar.type == 2)
                g = g + geom_bar(position = "dodge")
            g + xlab(input$qualiquali.var1) + ylab("") +
                labs(fill = input$qualiquali.var2)
        } else if (input$qualiquali.type == 2) {
            # Heatmap
            ggplot(don) + geom_bin2d(aes(x, y)) + 
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
            "Modalités" = levels(z),
            "Moyenne" = round(tapply(x, z, mean, na.rm = TRUE), input$qualiquanti.arrondi),
            "Ecart-Type" = round(tapply(x, z, sd, na.rm = TRUE), input$qualiquanti.arrondi),
            "Minimum" = round(tapply(x, z, min, na.rm = TRUE), input$qualiquanti.arrondi),
            "Maximum" = round(tapply(x, z, max, na.rm = TRUE), input$qualiquanti.arrondi)
        )
    }, options = opt.DT.simple)
    output$qualiquanti.plot <- renderPlot({
        if (input$qualiquanti.type == 0) return(NULL)    
        don = setNames(donnees()[,c(input$qualiquanti.varQl, input$qualiquanti.varQt)], c("z", "x"))
        don$z = factor(don$z)
        if (input$qualiquanti.type == 1) {
            # Histogramme
            ggplot(don) + geom_histogram(aes(x, fill=z)) +
                facet_grid(z~.) +
                xlab(input$qualiquanti.varQt) +
                labs(fill = input$qualiquanti.varQl)
        } else if (input$qualiquanti.type == 2) {
            # Boîtes à moustaches
            ggplot(don) + geom_boxplot(aes(z, x, fill=z)) +
                ylab(input$qualiquanti.varQt) + xlab("") +
                labs(fill = input$qualiquanti.varQl)
        }
    })
})
