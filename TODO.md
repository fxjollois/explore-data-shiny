# A FAIRE

## Quelques petits trucs en plus

- Nombre de lignes et de variables du jeu de données choisis
- Intégrer plus de jeux de données

## Remarques de J. Dedecker

1) dans quali-quali, représentations, je mettrai
- empilées en effectifs
- empilées en fréquences
- séparées, en effectifs
- séparées en fréquences

Dans ces cas là, la plupart du temps, ce sont les distributions
en fréquences qu'on veut tracer sur le même graphe, pour voir si
les distributions conditionnelles sont différentes (avant de faire un
test du chi2 par exemple). Les distributions en effectifs peuvent être tracées
sur deux graphes différents, car visuellement il est très difficile de
comparer des distributions conditionnelles en effectifs si les
populations sont de tailles  très différentes. Bref, cela pour dire
qu'il faudrait qu'il y ait les deux graphes en fréquence, je pense.

Toujours sur quali-quali: dans les tableaux croisée, est il possible de faire
apparaître les deux variables et leurs modalités? Pour l'instant, seule la
seconde apparaît. C'est un détail, car on a choisi les variables, mais ce
serait plus logique.

2) sur quali-quanti:
- Est-ce possible de proposer deux tableaux d'indicateurs conditionnels?
Le premier tel quel, avec moyenne écart type, min et max, parfait.
Le second avec premier quartile, médiane, troisième quartile. C'est
celui là qu'on représente avec les box-plot, et il comporte plutôt
plus d'information que le premier tableau. Si ce n'est pas possible,
au moins ajouter la médiane dans le premier tableau.

- Pour les histogrammes conditionnels: ici, le graphe le plus
important est celui en densité de fréquence. Il n'y a qu'avec
celui-là qu'on peut faire une comparaisons des densités conditionnelles
si les populations sont de tailles très différentes. Donc je remplacerai
les histogrammes en effectifs par des histogrammes en densité (ou en
fréquence car les classes sont de même amplitude). Sinon on risque (et
c'est le cas sur certains exemples) de se retrouver avec un
"gros" et un "petit" histogramme. Ici, je ne suis même pas sûr qu'il
faille laisser les histogrammes en effectif; il peuvent être présentés
sur deux graphes séparés.

3) sur quanti-quanti, je n'ai pas grand chose à ajouter. Je ne sais pas
si c'est facile d'ajouter des tableaux croisés par classes, car il y a
le problème du choix des classes; c'est la seule chose qui manquerait ici,
mais ce n'est pas forcément simple à faire.

Voilà, comme je le disais, ce sont des détails, et l'outil est déjà très bien.
Les seuls deux points sur lesquels je me permets d'insister un peu sont
les représentation des distributions conditionnelles (quali-quali ou
quali-quanti) qui devraient être proposées aussi en fréquences et/ou
densité de fréquence.