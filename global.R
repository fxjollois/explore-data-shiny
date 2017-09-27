library(shiny)
library(reshape2)
library(ggplot2)
library(scales)
library(httr)

# Chargement de Recensement85
recensement85 = read.table("donnees/Recensement85.csv", sep = ",", header = T)

# Fonction permettant d'afficher des pourcentages au lieu de proportions
to.pct <- function(df, decimal = 0) {
    nom.lignes = rownames(df)
    res = sapply(round(df*100, decimal), paste, "%")
    rownames(res) = nom.lignes
    return(res)
}
# to.pct(as.data.frame.matrix(prop.table(table(mtcars$cyl, mtcars$am))))
