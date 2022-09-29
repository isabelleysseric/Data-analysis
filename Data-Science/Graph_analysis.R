# DATA PROCESSING: Graph analysis


### Database 'diamonds'

# 'igraph' library
install.packages("igraph")
library(igraph)

# 'igraphdata' library
install.packages('igraphdata')
library(igraphdata)
data(package="igraphdata")

# UKfaculty dataset
data(UKfaculty)

# Dataset summary
summary(UKfaculty)


### Visualization of the graph

# Graphic representation in 2 boxes
layout(matrix(1:2,1,2))

# Representation by group
plot.igraph( UKfaculty, main = 'UKfaculty ~ Groupes',
  layout = layout_nicely(UKfaculty, dim = 2),
  vertex.color = V(UKfaculty)$Group,
  vertex.size = 10 ,
  edge.arrow.size=0.1,
  edge.width = E(UKfaculty)$weight ^ 0.3
)

# Representation by influencers
plot.igraph( UKfaculty, main = 'UKfaculty ~ Influenceurs',
  layout = layout_nicely(UKfaculty, dim = 2),
  vertex.color = V(UKfaculty)$Group,
  vertex.size = E(UKfaculty)$weight ^ 1 ,
  edge.arrow.size=0.1,
  edge.width = E(UKfaculty)$weight ^ 0.3
)


### Number of nodes in the graph
vcount(UKfaculty)


### Type of graph
is.directed(UKfaculty)


### Number of edges in the graph
ecount(UKfaculty)


### Calculating the centrality of the graph

# Intermediate centrality of each node
centraliteGraphe <- betweenness(UKfaculty)


# Normalization between 0 and 1
centraliteGrapheNorm <- (centraliteGraphe - min(centraliteGraphe)) / (max(centraliteGraphe)- min(centraliteGraphe))

# Visualization of centrality
plot(
  centraliteGrapheNorm,
  main = 'Centralité du graphe',
  xlab='Noeuds',
  ylab='Centralité moyenne normalisée',
  col='blue'
)

abline(
  h = mean( centraliteGrapheNorm, na.rm = TRUE ),
  lty = 3,
  lwd = 2,
  col = 'red'
)

# Average value
centraliteGrapheNormMoy <- mean(centraliteGrapheNorm)
centraliteGrapheNormMoy


### Clustering with the 'cluster_louvain' algorithm

# Transformation of the directed graph into an undirected graph
grapheND <- as.undirected(UKfaculty)
grapheND

# Algorithm 'cluster_louvain'
regroupement <- cluster_louvain(graphe_NonDirige)
regroupement


### Visualization of the grouping result

# Grouping result
str(regroupement)


# Visualization of the result
plot(
  regroupement,
  UKfaculty,
  main = 'Regroupement',
  vertex.size=10,
  edge.arrow.size=0.3
)


### Number of nodes in each group
sizes(regroupement)


### Removal of the group with the largest number of nodes in the initial graph

# Structure of the graph
str(UKfaculty)

# New graph without group 5 with 28 nodes
newGraphe <- delete_vertices(UKfaculty, unlist(UKfaculty[[5]]))

# Graph summary: 53 nodes and 358 arcs now
summary(newGraphe)


### Calculating the number of nodes in the new graph

# Number of nodes in the new graph
vcount(newGraphe)


# Visualization of the new graph
plot.igraph(
  newGraphe,
  main = 'Nouveau Graphe',
  vertex.color = 'pink',
  edge.color = 'grey',
  vertex.size = 10,
  edge.arrow.size = 0.3
)


### Calculation of the centrality of the new graph

# Centrality of the new graph
centraliteGrapheNG <- betweenness(newGraphe)

# Normalized centrality of the new graph
centraliteGrapheNormNG <-
  ( centraliteGrapheNG - min(centraliteGrapheNG) ) / ( max(centraliteGrapheNG) - min(centraliteGrapheNG) )

# Average centrality of the new graph
centraliteGrapheNormMoyNG <- mean(centraliteGrapheNormNG)
centraliteGrapheNormMoyNG


### Comparison of centrality values

# Visualization of the centrality of each graph
layout(matrix(1:2,1,2))

# Old graph
plot(
  centraliteGrapheNorm,
  main = 'Centralité ancien graphe',
  xlab = 'Noeuds',
  ylab = 'Centralité moyenne normalisée',
  col = 'blue'
)
abline(
  h = mean( centraliteGrapheNorm, na.rm = TRUE ),
  lty = 3,
  lwd = 2,
  col = 'red'
)

# New graph
plot(
  centraliteGrapheNormNG,
  main = 'Centralité nouveau graphe',
  xlab = 'Noeuds',
  ylab = 'Centralité moyenne normalisée',
  col = 'blue'
)
abline(
  h = mean( centraliteGrapheNormNG, na.rm = TRUE ),
  lty = 3,
  lwd = 2,
  col = 'red'
)

# Observation:
# We can see that in the new graph there are fewer points located above the average centrality than in the old graph.

# Number of nodes in the old graph and in the new
str(centraliteGraphe)

str(centraliteNewGraphe)

# Centrality of the old graphs
centraliteGrapheNormMoy

# Number of points before and after mean centrality
length( centraliteGrapheNorm[ centraliteGrapheNorm > 0.5 ])

length( centraliteGrapheNorm [
centraliteGrapheNorm > centraliteGrapheNormMoy ] )

length( centraliteGrapheNorm [
centraliteGrapheNorm < centraliteGrapheNormMoy ] )

# Centrality of the new graphs
centraliteGrapheNormMoyNG

# Number of points before and after mean centrality
length( centraliteGrapheNormNG[ centraliteGrapheNormNG > 0.5 ])

length( centraliteGrapheNormNG [
centraliteGrapheNormNG > centraliteGrapheNormMoy ] )

length( centraliteGrapheNormNG [
centraliteGrapheNormNG > centraliteGrapheNormMoyNG ] )

length( centraliteGrapheNormNG [
centraliteGrapheNormNG < centraliteGrapheNormMoyNG ] )

# Observation:
# The centrality of the new graph has slightly decreased from 0.14 to 0.13. It is noted that most of the very important
# nodes (greater than 0.5) are always present. In the old graph we had 29 points (36%) above and 52 (64%) below the
# average centrality of the graph. Compared to the new graph which now has only 13 above (25%) against 40 (75%) below.
# The deleted group contained a large number of not very important nodes with some moderately important ones, which
# lowered the centrality a little.
