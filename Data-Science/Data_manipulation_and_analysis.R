# DATA PROCESSING: Data manipulation and analysis


### Downloading "diamonds" content from "ggplot2" package
library(ggplot2)
data("diamonds")
head(diamonds)


### Information on the "diamonds" database
str(diamonds)

# Number of observations and number of variables
dim(diamonds)
# 53940 observations and 10 variables

# The names of the different variables
names(diamonds)


### Calculation of the average and the quartiles of the variable 'price'

# The mean and the three quartiles
summary(diamonds$price)


### Box plot of price by cut
boxplot(
  diamonds$price~diamonds$cut,
  col  = c('slategray1','skyblue1','steelblue1','steelblue3','steelblue4'),
  main = 'Prix selon la coupe',
  xlab = 'COUPE',
  ylab = 'PRIX'
)


### Distribution of the 'price' and 'carat' variables

# Visual representation of the distribution of the two variables
plot(
  diamonds$price ~ diamonds$carat,
  main = "Distribution des variables price et carat",
  xlab = 'CARAT', ylab = 'PRIX', col = c('blue')
)

# We can see that the more the number of carat increases, the more the price increases.
# The price and the number of carats is strongly correlated.
# We also note that the majority of the points are between a little more than 0 and a little less than 3 carats.


### Asymmetry coefficients and Flattening coefficients of the 'price' and 'carat' variables

# Various statistics functions
library( e1071 )

# Asymmetry coefficient
Price.asymetrie <- skewness(diamonds$price) # 1.618305
Price.aplattissement <- kurtosis(diamonds$carat) # 1.25625

# Flattening coefficients
Carat.asymetrie <- kurtosis(diamonds$price) # 2.177191
Carat.aplattissement <- skewness(diamonds$carat) # 1.116584

# Summary table
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


### Comparaison des résultats

# The asymmetry coefficient is greater than zero for both variables, so the distribution of the variable 'price' and
# that of 'carat' are both shifted to the left and their mean is therefore greater than the median.
# The flattening coefficient is greater than zero for both variables so # the distribution is more concentrated than
# normal.


### Checking the price difference between two diamonds of different colors: color 'D' and 'J'


## Formulation of the hypothesis:
# H0: µD = µJ
# H1 : µD ≠ µJ
# seuil : 5%


##  Data set

# Diamonds based on price and color
diamantsColor <- data.frame(diamonds$price, diamonds$color)
names(diamantsColor) <- c("Price", "Color")

# Price based on 'D' and 'J' colors
diamondsD <- diamantsColor$Price[diamantsColor$Color == 'D']
diamondsJ <- diamantsColor$Price[diamantsColor$Color == 'J']

# Hypothesis test
t.test( diamondsD, diamondsJ, mean = 0 )

# Results interpretation:
# The null hypothesis is rejected because the two means are different from 0.
# This means that there is a significant difference between the two means. There is therefore a difference between
# diamonds of color D and those of color J. We can see that the average of diamondsD is 3169.954 and that of diamondsJ
# is 5323.818, which confirms that the averages are different.


### Fair type diamond groupings


## Data Preparation

# Diamonds by price, carat and cut
diamantsCut <- data.frame( diamonds$price, diamonds$carat, diamonds$cut )
names(diamantsCut) <- c("Price", "Carat", "Cut")

# Price and Carat for 'Fair' cuts
FairPrice <- diamantsCut$Price[diamantsCut$Cut == 'Fair']
FairCarat <- diamantsCut$Carat[diamantsCut$Cut == 'Fair']

# Sample of the top 500
diamondsFair <- data.frame(
  matrix(
    c(FairPrice[1:500],FairCarat[1:500]), ncol=2
  )
)
colnames(diamondsFair) = c('Price','Carat')

# The first 'diamondsFair' sample data
head(diamondsFair)

str(diamondsFair)

# Clustering with the K-mean algorithm
set.seed(20)
fairCluster <- kmeans(diamondsFair, 2, nstart = 20)

# Number of samples per group
fairCluster$size

# Group 1: 263 samples
# Group 2: 237 samples


## Calculation of the silhouette: 0.63

# Calculation of the silhouette
library(cluster)
s <- silhouette( fairCluster$cluster, dist(diamondsFair) )

# Silhouette Results
summary(s)

# Graphic representation of the silhouette
plot(s)


## Observations on the two groups
# The silhouette index is positive for the 2 groups, which shows the homogeneity within the groups.
# Also the 2 groups have the same coefficient, which shows a good separation of the data. We can see on the abscissa
# axis the silhouette index and on the ordinate axis the grouping of the data of the two groups.


### Determining the correlation between the 'price' and 'carat' variables for VS1-like clarity


## Data Preparation

# Table with price, carat and clarity
tableauVS1 <- data.frame(diamonds$price,diamonds$carat,diamonds$clarity)
names(tableauVS1) <- c("Price","Carat","Clarity")

# Price and Carat for 'Fair' cuts
VS1Price <- tableauVS1$Price[tableauVS1$Clarity == 'VS1']
VS1Carat <- tableauVS1$Carat[tableauVS1$Clarity == 'VS1']

# The 'diamondsVS1' dataset
diamondsVS1 <- data.frame( matrix( c( VS1Carat, VS1Price ), ncol = 2 ) )
colnames(diamondsVS1) = c('Carat','Price')

# The first data from 'diamondsVS1'
head(diamondsVS1)

# Variables to correlate:
# Y: Dependent variable: Price
# X: Independent variable: Cara

# Regression relationship between the two variables
require(stats)
regression <- lm(diamondsVS1$Price ~ diamondsVS1$Carat)
coeff = coefficients(regression)

# Regression Information
regression

# Regression Line Equation
equation = paste0( 'y = ', round(coeff[2],0),
' * x ', round(coeff[1],0) )

# Equation
equation


## Interpretation of the regression result:

# Detail of the regression
summary(regression)

# Interpretation of regression results:
# We see the price = α + β carat pattern with α = -2705.50 and β = 9000.73.
# This equation tells us that the price depends linearly on the carats: the price increases by $9000.73 per
# additional carat.
# Since the p value = 2.2e-16 is less than 0.05, we can conclude that there is a significant relationship between the
# variables of the linear regression model and since R-squered is very high with 0.9029, it allows us to conclude that
# the regression model is of very good quality.


## Graphic representation of the regression line:

# Graphical representation of the regression line
plot(diamondsVS1, main = equation, col = 'blue')
abline(regression, col = 'red', lwd = 2)

