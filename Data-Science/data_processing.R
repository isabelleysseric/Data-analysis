# DATA PROCESSING: Data manipulation and analysis

# Downloading "diamonds" content from "ggplot2" package
library(ggplot2)
data("diamonds")
head(diamonds)


# Information on the "diamonds" database
str(diamonds)

# Nombre d'observations et nombre de variables
dim(diamonds)
# 53940 d'observations et 10 variables


# Le nom des différentes variables
names(diamonds)

# Calcul de la moyenne et les quartiles de la variable 'price'
# La moyenne et les trois quartiles
summary(diamonds$price)

# Diagramme en boîte du prix selon la coupe
boxplot(
  diamonds$price~diamonds$cut,
  col  = c('slategray1','skyblue1','steelblue1','steelblue3','steelblue4'),
  main = 'Prix selon la coupe',
  xlab = 'COUPE',
  ylab = 'PRIX'
)

# Distribution des variables 'price' et 'carat'
# Représentation visuelle de la distribution des deux variables
plot(
  diamonds$price ~ diamonds$carat,
  main = "Distribution des variables price et carat",
  xlab = 'CARAT', ylab = 'PRIX', col = c('blue')
)

# On peut voir que plus le nombre de carat augmente, plus le prix augmente.
# Le prix et le nombre de carats est fortement corrélé.
# On constate également que la majorité des points se situent entre un peu plus de 0 et un peu moins de 3 carats.


## Coefficients d'asymétrie et d'aplatissement des variables 'price' et 'carat'

# Fonctions diverses de statistiques
library( e1071 )

# Coefficient d'asymetrie
Price.asymetrie <- skewness(diamonds$price) # 1.618305
Price.aplattissement <- kurtosis(diamonds$carat) # 1.25625

# Coefficient d'applatissement
Carat.asymetrie <- kurtosis(diamonds$price) # 2.177191
Carat.aplattissement <- skewness(diamonds$carat) # 1.116584

# Tableau récapitulatif
tableauRecap <- data.frame(
  matrix(
    c(Price.asymetrie,Price.aplattissement,
    Carat.asymetrie, Carat.aplattissement),
    nrow = 2, ncol = 2, byrow = TRUE
  )
)

dimnames(tableauRecap) = list(
  c('Price','Carat'),
  c('Asymetrie','Aplattissement')
)

tableauRecap


## Comparaison des résultats

# Le coefficient d'asymétrie est supérieur à zéro pour les deux variables, donc la distribution de la variable 'price'
# et celle de 'carat' sont toutes les deux décalées vers la gauche et leur moyenne est donc supérieur à la médiane.
# Le coefficient d'aplatissement est supérieur à zéro pour les deux variables donc # la distribution est plus
# concentrée que la normale.

## Vérification de différence de prix entre deux diamants de couleur différentes : couleur 'D' et 'J'

## Formulation de l'hypothèse:
# H0: µD = µJ
# H1 : µD ≠ µJ
# seuil : 5%

##  Ensemble de données

# Diamants en fonction du prix et de la couleur
diamantsColor <- data.frame(diamonds$price, diamonds$color)
names(diamantsColor) <- c("Price", "Color")

# Prix en fonction des couleurs 'D' et 'J'
diamondsD <- diamantsColor$Price[diamantsColor$Color == 'D']
diamondsJ <- diamantsColor$Price[diamantsColor$Color == 'J']

# Test d'hypothèse
t.test( diamondsD, diamondsJ, mean = 0 )


# Interprétation des résultats:
# L'hypothèse nulle est rejetée car les deux moyennes sont différentes de 0. Ce qui veut dire qu'il y a une différence
# significative entre les deux moyennes. Il y a donc bien une différence entre les diamants de couleur D et ceux de
# couleur J. On peut voir que la moyenne de diamondsD est de 3169.954 et celle de diamondsJ de 5323.818, ce qui vient
# confirmer que les moyennes sont différentes.

## Regroupements de diamants de type Fair
## Préparation des données

# Diamants en fonction du prix, carat et coupe
diamantsCut <- data.frame( diamonds$price, diamonds$carat, diamonds$cut )
names(diamantsCut) <- c("Price", "Carat", "Cut")

# Prix et Carat pour les coupes 'Fair'
FairPrice <- diamantsCut$Price[diamantsCut$Cut == 'Fair']
FairCarat <- diamantsCut$Carat[diamantsCut$Cut == 'Fair']

# Échantillon des 500 premiers
diamondsFair <- data.frame(
  matrix(
    c(FairPrice[1:500],FairCarat[1:500]), ncol=2
  )
)
colnames(diamondsFair) = c('Price','Carat')

# Les premières données de l'échantillon 'diamondsFair'
head(diamondsFair)

str(diamondsFair)

# Regroupement avec l'algorithme K-moyenne
set.seed(20)
fairCluster <- kmeans(diamondsFair, 2, nstart = 20)

# Nombre d'échantillons par groupe
fairCluster$size

# Groupe 1: 263 échantillons
# Groupe 2: 237 échantillons

## Calcul de la silhouette: 0.63

# Calcul de la silhouette
library(cluster)
s <- silhouette( fairCluster$cluster, dist(diamondsFair) )

# Résultats de la silhouette
summary(s)

# Représentation graphique de la silhouette
plot(s)

## Observations sur les deux groupes
# L'indice de silhouette est positif pour les 2 groupes, ce qui montre l'homogénéité au sein des groupes.
# Aussi les 2 groupes ont le même coefficient, ce qui montre une bonne séparation des données.
# On peut voir sur l'axe des abscisses l'indice de silhouette et sur l'axe des ordonnées le regroupement des données
# des deux groupes.


## 10 Détermination de la corrélation entre les les variables 'price' et 'carat' pour une clarté de type VS1

## Préparation des données

# Tableau avec prix, carat et coupe
tableauVS1 <- data.frame(diamonds$price,diamonds$carat,diamonds$clarity)
names(tableauVS1) <- c("Price","Carat","Clarity")

# Prix et Carat pour les coupes 'Fair'
VS1Price <- tableauVS1$Price[tableauVS1$Clarity == 'VS1']
VS1Carat <- tableauVS1$Carat[tableauVS1$Clarity == 'VS1']

# L'ensemble de données 'diamondsVS1'
diamondsVS1 <- data.frame( matrix( c( VS1Carat, VS1Price ), ncol = 2 ) )
colnames(diamondsVS1) = c('Carat','Price')

# Les premières données de 'diamondsVS1'
head(diamondsVS1)

# Variables à corréler:
# Y : Variable dépendante: Price
# X : Variable indépendante: Carat

# Relation de régression entre les deux variables
require(stats)
regression <- lm(diamondsVS1$Price ~ diamondsVS1$Carat)
coeff = coefficients(regression)

# Information sur la regression
regression

# Equation de la droite de régression
equation = paste0( 'y = ', round(coeff[2],0),
' * x ', round(coeff[1],0) )

# Equation
equation

## Interprétation du résultat de la régression:

# Détail de la régression
summary(regression)


# Interprétation des résultats de la régression:
# Nous voyons le modèle price = α + β carat avec α = -2705,50 et β = 9000,73. Cette équation nous dit que le prix
# dépend linéairement des carats: le prix augmente de 9000,73$ par carat additionnelle.
# Puisque la valeur p = 2,2e-16 est inférieur à 0,05, nous pouvons conclure qu'il y a une relation significative entre
# les variables du modèle de régression linéaire et puisque R-squered est très élevé avec 0.9029, ça nous permet de
# conclure que le modèle de régression est de très bonne qualité.


## Représentation graphique de la droite de régression:

# Représentation graphique de la droite de régression
plot(diamondsVS1, main = equation, col = 'blue')
abline(regression, col = 'red', lwd = 2)