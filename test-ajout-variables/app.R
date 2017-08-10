library(shiny)

shinyApp(
    ui = fluidPage(
        sidebarLayout(
            sidebarPanel(
                actionButton("newVars.ajout", "Ajout d'une nouvelle variable")
            ),
            mainPanel(
                uiOutput("newVars.ui"),
                tags$div(id = "newVars"),
                tableOutput("newVars.res")
            )
        )
    ),
    server = function(input, output) {
        donnees <- reactive({ iris })
        
        newVars.liste <- list()
        
        # Premier essai : ca marche mais pas terrible
        # output$newVars.ui <- renderUI({
        #     if (input$newVars.ajout == 0) { return (p("Aucune variable ajoutée"))}
        #     
        #     lapply(1:input$newVars.ajout, function(e) {
        #         return(list(
        #             textInput(paste0("newVars", e, ".nom"), "Nom de la variable", placeholder = "donner le nom de la nouvelle variable"),
        #             selectInput(paste0("newVars", e, ".var"), "Variable de départ", names(donnees())),
        #             textInput(paste0("newVars", e, ".cut"), "Découpage", placeholder = "indiquer ici les bornes des intervalles"),
        #             plotOutput(paste0("newVars", e, ".bar"))
        #         ))
        #     })
        # })
        
        # Deuxième essai : cf aide de observeEvent
        observeEvent(
            input$newVars.ajout,
            {
                e = input$newVars.ajout
                insertUI(
                    paste0("#newVars"),
                    "beforeEnd",
                    list(
                        fluidRow(
                            id = paste0("newVars", e),
                            column(width = 3, textInput(paste0("newVars", e, ".nom"), "Nom de la variable", paste0("nouvelle", e), placeholder = "donner le nom de la nouvelle variable")),
                            column(width = 3, selectInput(paste0("newVars", e, ".var"), "Variable de départ", names(donnees()))),
                            column(width = 6, textInput(paste0("newVars", e, ".cut"), "Découpage", 5, placeholder = "indiquer ici les bornes des intervalles"))
                        ),
                        tableOutput(paste0("newVars", e, ".tab")),
                        plotOutput(paste0("newVars", e, ".bar"))
                    )
                )
                
                output[[paste0("newVars", e, ".tab")]] <- renderTable({
                    don = donneesAug()
                    table(don[,input[[paste0("newVars", e, ".nom")]]], useNA = "ifany")
                })
                output[[paste0("newVars", e, ".bar")]] <- renderPlot({
                    don = donneesAug()
                    barplot(table(don[,input[[paste0("newVars", e, ".nom")]]], useNA = "ifany"))
                })
            }
        )
        
        donneesAug <- reactive({
            don = donnees()
            
            vars = sub(".var", "", names(input)[grep("newVars[0-9]+.var", names(input))])
            
            if (length(vars) == 0) { return (don)}
            
            for (v in vars) {
                variable = input[[paste0(v, ".var")]]
                decoupage = as.numeric(strsplit(input[[paste0(v, ".cut")]], ",")[[1]])
                don[input[[paste0(v, ".nom")]]] = cut(don[[variable]], decoupage)
            }
            
            don
        })
        
        output$newVars.res <- renderTable({
            donneesAug()
        })
    }
)
