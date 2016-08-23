# Explore Data

Application web d'exploration de données permettant de réaliser :

- des statistiques univariées
- des statistiques bivariées

## Informations

Cette interface web est une application `Shiny`, nécessitant donc le logiciel R et le package `shiny`, ainsi que les packages `reshape2`, `ggplot2` et `scales`.

Elle est écrite et maintenue par [FX Jollois](http://fxjollois.github.io), pour le département [STID](http://www.stid-paris.fr) de l'[IUT Paris Descartes](http://www.iut.parisdescartes.fr). Les fichiers sources sont disponbiles sur [Github](https://github.com/fxjollois/explore-data).

## Installation 

Pour utiliser ce package, vous pouvez faire au choix (en vous assurant que tous les outils nécessaires sont installés) :

- `runGithub("fxjollois", "explore-data")` dans la console R
- télécharger l'application avec ce [lien](https://github.com/fxjollois/explore-data/archive/master.zip), dans un répertoire, et de lancer
    - soit avec `runApp("chemin/vers/repertoire")`
    - soit avec `runApp()` si le répertoire de travail est celui où l'application est située
    - soit en ouvrant un des fichiers `ui.R` ou `server.R` avec RStudio et de cliquer sur *Run App*
- télécharger l'application avec le même lien pour l'installer sur un serveur *Shiny* directement et y accéder via l'URL `serveur:port/explore-data`


