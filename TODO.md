# A FAIRE

## Remarques de J. Dedecker

- sur quali-quanti: Pour les histogrammes conditionnels: ici, le graphe le plus
important est celui en densité de fréquence. Il n'y a qu'avec
celui-là qu'on peut faire une comparaisons des densités conditionnelles
si les populations sont de tailles très différentes. Donc je remplacerai
les histogrammes en effectifs par des histogrammes en densité (ou en
fréquence car les classes sont de même amplitude). Sinon on risque (et
c'est le cas sur certains exemples) de se retrouver avec un
"gros" et un "petit" histogramme. Ici, je ne suis même pas sûr qu'il
faille laisser les histogrammes en effectif; il peuvent être présentés
sur deux graphes séparés.

- sur quanti-quanti, je n'ai pas grand chose à ajouter. Je ne sais pas
si c'est facile d'ajouter des tableaux croisés par classes, car il y a
le problème du choix des classes; c'est la seule chose qui manquerait ici,
mais ce n'est pas forcément simple à faire.

- pour quali-quanti: calcul de vinter vintra rapport de corrélation
et indicateur de Fischer (et sa p-valeur). Tout cela peut a priori
(à renormalisations éventuelles près) s'extraire d'une sortie de la
commande anova.

- pour  quali-quali: tableau des effectifs théoriques; calcul de
l'indicateur du chi2 et sa p-valeur.

- pour quanti-quanti, outre le nuage et le tracé de la droite, calcul
de la covariance et du coefficient de corrélation.
