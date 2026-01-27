
## 1. Arborescence de l'application 

Dans R/ :


- 
- on retrouve tous les modules qui construisent les graphiques. Ils sont rangés par onglets : ceux qui commencent pas '0.' font partie du premier onglet, 
ceux qui commencent par '1.' du deuxième onglet et ainsi de suite. 
- 


Dans www/ :

Dans archives/ :

Dans data_raw/ : 

- La base de données brutes sous la forme BDD.CLEAN.csv. Cette base de données est créées via une routine excel qui normalise le document. Ainsi, 
en intégrant une nouvelle base de données du même format sous le même nom, il est possible de modifier les données de l'application.  

Dans data/ : 

- Ce sont toutes les données en format .rds qui résultent de la base de données brutes. Ces tables font partie des documents envoyés à shinyapps.io pour la diffusion
(ils sont plus léger que la base de données bnrutes BDD.CLEAN.csv)


Dans renv/ :

## 2. Modifier le style de l'application 

Pour faire évoluer le styme de l'application : polices, couleurs, forme..., il y a 3 documents clés qui centralisent tout cela :

- R/00_utils_palette.R : Dans ce module, vous retrouverez toutesles couleurs appliquées dans les graphiques (barres empilés, camamberts, sankeys...)
Les modules font donc appel à cette palette de couleur centralisée. Toute modifications des couleurs de l'intérieur des graphiques doit passer par ce module

- R/99_utils_plotly_theme.R : Ce module a pour fonction de décider du style des plot (couleur des légendes, police des légendes, axes x et y...)
Il y a deux thèmes qui sont préparé : un thème foncé (dark) et un thème clair (light) qui peuvent être choisi au tout debut du script. Tous les modules
prendront ensuite la couleur décidé ici.

- www/app.css : C'est le module le ,plus important pur ce qu'il s'agit de la mise en forme de l'application, tout ce qui est en dehors des graphique est 
décidé dans ce document : forme des encadrés, polices des titres, couelrs des fond, bandeau du haut et du bas... C'est ici qu'il faut modifier si on veut
changer la police ou a forme d'un élément.