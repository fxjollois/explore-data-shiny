library(shiny)
library(reshape2)
library(ggplot2)
library(scales)
library(httr)
library(markdown)

library(magrittr)
library(tidyr)
library(dplyr)
library(formattable)
library(htmltools)

# mis en commentaires le 8/2/2019 pour suppression sparklines
#library(sparkline)
#library(htmlwidgets)


#################################################
# Présentation des variables

boxCell <- function(x){
    lapply(x, function(xx){
        as.character(as.tags(
            sparkline(xx, type="box", width = 500,
                      chartRangeMin = min(unlist(xx)),
                      chartRangeMax = max(unlist(xx)))
        ))
    })
}

barCell <- function(x){
    lapply(x, function(xx){
        t = table(xx)
        m = names(t)
        as.character(as.tags(
            sparkline(t, type="bar", width = 500,
                      barWidth = 50, barSpacing = 10,
                      chartRangeMin = 0,
                      chartRangeMax = max(t),
                      tooltipFormat= "{{offset:val}} ({{value}})",
                      tooltipValueLookups = htmlwidgets::JS(paste('{val:{', paste(paste(0:length(t), paste0("'", m, "'"), sep = ":"), collapse = ", "), '}}')))  
            # '{"val": {0: "A", 1: "B", 2:" C"}}'))
        ))
    })
}

toWidget <- function(f) {
    f %>%
        formattable::as.htmlwidget() %>%
        tagList() %>%
        attachDependencies(
            htmlwidgets:::widget_dependencies("sparkline", "sparkline")
        ) %>%
        browsable()
}

drawRepresentation <- function(df) {
    var.numeric = which(sapply(df, function(v) {
        ifelse(is.numeric(v) | any(class(v) == "Date"), length(unique(v)) > 15, FALSE)
    }))
    var.other = !(1:ncol(df) %in% var.numeric)
    
    df %>%
        gather(var, val) %>%
        group_by(var) %>%
        summarise(values = list(val)) %>%
        slice(apply(table(var, names(df)), 1, which.max)) %>%
        mutate(class = sapply(df, function(c) return(paste(class(c), collapse = ", ")))) %>%
        select(var, class, values) %>%
        rename(Variable = var, Type = class, Distribution = values) %>%
        formattable(
            formatters = list(
                area(row = var.numeric, col = 3) ~ boxCell,
                area(row = var.other, col = 3) ~ barCell
            )
        ) %>%        
        toWidget()
}

# Test pour présentation des variables
# drawRepresentation(ggplot2::diamonds)
# drawRepresentation(ggplot2::msleep)

