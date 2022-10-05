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
print("Dataset summary")
summary(UKfaculty)

# Output:
# IGRAPH 6f42903 D-W- 81 817 --
# + attr: Type (g/c), Date (g/c), Citation (g/c), Author (g/c), Group (v/n), weight (e/n)


### Visualization of the graph

# Graphic representation in 2 boxes
print("Graphic representation in 2 boxes")
layout(matrix(1:2,1,2))

# Representation by group
plot.igraph( UKfaculty, main = 'UKfaculty ~ Groups',
  layout = layout_nicely(UKfaculty, dim = 2),
  vertex.color = V(UKfaculty)$Group,
  vertex.size = 10 ,
  edge.arrow.size=0.1,
  edge.width = E(UKfaculty)$weight ^ 0.3
)

# Representation by influencers
plot.igraph( UKfaculty, main = 'UKfaculty ~ Influencers',
  layout = layout_nicely(UKfaculty, dim = 2),
  vertex.color = V(UKfaculty)$Group,
  vertex.size = E(UKfaculty)$weight ^ 1 ,
  edge.arrow.size=0.1,
  edge.width = E(UKfaculty)$weight ^ 0.3
)


### Number of nodes in the graph
print("Number of nodes in the graph")
vcount(UKfaculty)

# Output:
# 81

### Type of graph
print("Type of graph")
is.directed(UKfaculty)

# Output:
# TRUE


### Number of edges in the graph
print("Number of edges in the graph")
ecount(UKfaculty)

# Output:
# 817


### Calculating the centrality of the graph

# Intermediate centrality of each node
print("Intermediate centrality of each node")
centraliteGraphe <- betweenness(UKfaculty)
centraliteGraphe

# Output:
#   347.1190476  711.2849456    5.5000000  185.9786797  727.4570360  296.0070679  245.2856061    0.0000000  272.7139250  389.9184094    0.0000000
#    29.8955128   96.7674714    0.0000000  104.0853896   16.5000000  522.2601732    0.0000000   16.4607143   71.6873016  143.3159452  231.1086081
#     0.8333333  241.3940629   43.7797619    5.4191198  260.0696484   79.3115551  684.1950577   37.2781136  109.7913781    0.0000000   20.6452742
#    39.4174603  263.6281746    0.0000000 1223.0844364  628.5462482  139.3486416  148.1517857    0.2000000  143.4625763   77.7957792    4.2738095
#     5.1666667  106.8142857    5.5666667  325.6341492  248.0175075    5.2289683  234.3818043  381.8448163  201.8959957  389.1091187   93.1060606
#    10.9978175  142.4654040  431.8554695   38.2029942    0.0000000  104.0959957  965.5960470    0.0000000    9.9393939    7.3333333   20.8666667
#    88.3321581   62.9221001  368.5825411  190.0153666    0.1250000  445.4177683    0.0000000    0.0000000  322.7988456   11.5250000  349.7698732
#     0.0000000   70.3940476   90.2478175   68.0222222


# Normalization between 0 and 1
print("Normalization between 0 and 1")
centraliteGrapheNorm <- (centraliteGraphe - min(centraliteGraphe)) / (max(centraliteGraphe) - min(centraliteGraphe))
centraliteGrapheNorm

# Output:
#  0.2838062829 0.5815501567 0.0044968277 0.1520571059 0.5947725393 0.2420168707 0.2005467478 0.0000000000 0.2229722796 0.3187992568 0.0000000000
#  0.0244427220 0.0791175724 0.0000000000 0.0851007392 0.0134904832 0.4270025500 0.0000000000 0.0134583630 0.0586118991 0.1171758391 0.1889555628
#  0.0006813375 0.1973650025 0.0357945540 0.0044306996 0.2126342554 0.0648455272 0.5594013278 0.0304787735 0.0897659841 0.0000000000 0.0168796802
#  0.0322279142 0.2155437243 0.0000000000 1.0000000000 0.5139025806 0.1139321518 0.1211296467 0.0001635210 0.1172957255 0.0636062212 0.0034942882
#  0.0042242927 0.0873318984 0.0045513347 0.2662401217 0.2027803642 0.0042752308 0.1916317446 0.3121982464 0.1650711837 0.3181375767 0.0761239844
#  0.0089918710 0.1164804324 0.3530872086 0.0312349606 0.0000000000 0.0851094107 0.7894761950 0.0000000000 0.0081264986 0.0059957703 0.0170606918
#  0.0722208177 0.0514454262 0.3013549434 0.1553575215 0.0001022006 0.3641758125 0.0000000000 0.0000000000 0.2639219632 0.0094228981 0.2859736113
#  0.0000000000 0.0575545281 0.0737870704 0.0556153117

# Visualization of centrality
plot(
  centraliteGrapheNorm,
  main = 'Centrality of the graph',
  xlab = 'Nodes',
  ylab = 'Normalized mean centrality',
  col  = 'blue'
)

abline(
  h = mean( centraliteGrapheNorm, na.rm = TRUE ),
  lty = 3,
  lwd = 2,
  col = 'red'
)

# Average value
print("Average value")
centraliteGrapheNormMoy <- mean(centraliteGrapheNorm)
centraliteGrapheNormMoy

# Output:
# 0.1449507


### Clustering with the 'cluster_louvain' algorithm

# Transformation of the directed graph into an undirected graph
print("Transformation of the directed graph into an undirected graph")
grapheND <- as.undirected(UKfaculty)
grapheND

# Output:
# IGRAPH 3fc0935 U-W- 81 577 --
# + attr: Type (g/c), Date (g/c), Citation (g/c), Author (g/c), Group (v/n), weight (e/n)
# + edges from 3fc0935:
#   1-- 4  3-- 4  5-- 6  5-- 7  6-- 7  3-- 9  4-- 9  5-- 9  5--10  7--10  5--12  7--12  5--13  7--13 10--13 12--13  2--15 14--15  5--16  7--16 12--16
#  13--16  3--17  4--17  9--17  2--18  7--18 15--18 15--19 18--19  2--20  6--20  8--20 14--20 15--20 18--20 19--20  2--21  8--21 10--21 13--21 15--21
#  19--21  5--22  7--22 10--22 12--22 21--22  5--23  7--23 10--23 12--23 13--23 22--23  2--25 15--25 20--25 21--25  2--26 14--26 20--26 21--26  5--27
#   7--27 10--27 13--27 16--27 19--27 22--27 23--27  5--28 10--28 13--28  2--29  4--29  6--29  7--29  8--29 14--29 15--29 18--29 19--29 20--29 21--29
#  22--29 23--29 24--29 26--29 27--29  6--30  7--30 10--30 22--30  2--31  8--31 15--31 19--31 20--31 21--31 25--31 26--31 29--31  2--32 24--32 29--32
#  31--32 10--33 12--33 13--33 16--33 23--33 27--33 15--34 19--34 21--34 25--34 29--34 31--34  2--35 15--35 18--35 21--35 26--35 27--35 29--35 31--35
#  34--35  1--36  3--36  4--36  2--37  5--37  7--37  9--37 10--37 13--37 15--37 16--37 18--37 19--37 21--37 26--37 27--37 29--37 31--37 32--37 33--37
#  34--37 35--37  1--38  2--38  5--38 10--38 17--38 33--38 37--38  2--39 15--39 18--39 19--39 21--39 25--39 29--39 31--39  5--40  7--40 10--40 12--40
# + ... omitted several edges

# Algorithm 'cluster_louvain'
print("Algorithm 'cluster_louvain'")
regroupement <- cluster_louvain(grapheND)
regroupement

# Output:
# IGRAPH clustering multi level, groups: 5, mod: 0.56
# + groups:
#   $`1`
#    [1]  1  3  4  9 17 36 44 45 53 59 60 61 62 73 74 75 78 81
#
#   $`2`
#    [1]  2  8 11 15 18 19 21 25 29 31 34 35 39 41 43 46 57 58 79
#
#   $`3`
#    [1]  5 24 32 37 38 48 50 52 54 55 64 67 70
#
#   $`4`
#   + ... omitted several groups/vertices


### Visualization of the grouping result

# Grouping result
print("Grouping result")
str(regroupement)

# Output:
# List of 5
#  $ membership : int [1:18] 1 3 4 9 17 36 44 45 53 59 ...
#  $ memberships: int [1:19] 2 8 11 15 18 19 21 25 29 31 ...
#  $ modularity : int [1:13] 5 24 32 37 38 48 50 52 54 55 ...
#  $ vcount     : int [1:25] 6 7 10 12 13 16 22 23 27 28 ...
#  $ algorithm  : int [1:6] 14 20 26 51 56 80
#  - attr(*, "class")= chr "communities"

# Visualization of the result
plot(
  regroupement,
  UKfaculty,
  main = 'Grouping result',
  vertex.size=10,
  edge.arrow.size=0.3
)


### Number of nodes in each group
print("Number of nodes in each group")
sizes(regroupement)

# Output:
# Community sizes
#  1  2  3  4  5
# 18 19 13 25  6


### Removal of the group with the largest number of nodes in the initial graph

# Structure of the graph
print("Structure of the graph")
str(UKfaculty)

# Output:
# Class 'igraph'  hidden list of 10
#  $ : num 81
#  $ : logi TRUE
#  $ : num [1:817] 56 75 11 42 27 57 6 39 4 47 ...
#  $ : num [1:817] 51 41 68 33 46 50 28 70 36 54 ...
#  $ : num [1:817] 580 411 719 376 569 215 533 620 527 592 ...
#  $ : num [1:817] 241 433 238 352 258 274 115 24 263 25 ...
#  $ : num [1:82] 0 6 23 27 37 65 74 91 93 101 ...
#  $ : num [1:82] 0 9 28 32 40 50 58 76 82 87 ...
#  $ :List of 4
#   ..$ : num [1:3] 1 0 1
#   ..$ :List of 4
#   .. ..$ Type    : chr "TSPE"
#   .. ..$ Date    : chr "Mon Mar 19 21:56:02 2007"
#   .. ..$ Citation: chr "Nepusz T., Petroczi A., Negyessy L., Bazso F.: Fuzzy communities and the concept of bridgeness in complex netwo"| __truncated__
#   .. ..$ Author  : chr "Nepusz T., Petroczi A., Negyessy L., Bazso F."
#   ..$ :List of 1
#   .. ..$ Group: num [1:81] 3 1 3 3 2 2 2 1 3 2 ...
#   ..$ :List of 1
#   .. ..$ weight: num [1:817] 4 14 4 4 10 2 6 2 4 4 ...
#  $ :<environment: 0x0000017331b270c0>

# New graph without group 5 with 28 nodes
newGraphe <- delete_vertices(UKfaculty, unlist(UKfaculty[[5]]))

# Graph summary: 53 nodes and 358 arcs now
print("Graph summary: 53 nodes and 358 arcs now")
summary(newGraphe)

# Output:
# IGRAPH 9cb832f D-W- 53 358 --
# + attr: Type (g/c), Date (g/c), Citation (g/c), Author (g/c), Group
# | (v/n), weight (e/n)


### Calculating the number of nodes in the new graph

# Number of nodes in the new graph
print("Number of nodes in the new graph")
vcount(newGraphe)

# Output:
# 53

# Visualization of the new graph
plot.igraph(
  newGraphe,
  main = 'New Graph',
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
print("Average centrality of the new graph")
centraliteGrapheNormMoyNG <- mean(centraliteGrapheNormNG)
centraliteGrapheNormMoyNG

# Output:
# 0.1307393


### Comparison of centrality values

# Visualization of the centrality of each graph
print("Visualization of the centrality of each graph")
layout(matrix(1:2,1,2))

# Old graph
plot(
  centraliteGrapheNorm,
  main = 'Old graph centrality',
  xlab = 'Nodes',
  ylab = 'Normalized mean centrality',
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
  main = 'New graph centrality',
  xlab = 'Nodes',
  ylab = 'Normalized mean centrality',
  col = 'blue'
)
abline(
  h = mean( centraliteGrapheNormNG, na.rm = TRUE ),
  lty = 3,
  lwd = 2,
  col = 'red'
)

cat("
  Observation:
  We can see that in the new graph there are fewer points located above the average centrality than in the old graph."
)

# Number of nodes in the old graph and in the new
print("Number of nodes in the old graph")
str(centraliteGraphe)

# Output:
# num [1:81] 347.1 711.3 5.5 186 727.5 ...

print("Number of nodes in the new graph")
str(centraliteGrapheNG) #str(centraliteNewGraphe)

# Output:
#  num [1:53] 290.5 820.1 98.5 426.2 0 ...

# Centrality of the old graphs
print("Centrality of the old graphs")
centraliteGrapheNormMoy

# Output:
#  0.1449507

# Number of points before and after mean centrality
print("Number of points before and after mean centrality : centraliteGrapheNorm > 0.5 ")
length( centraliteGrapheNorm[ centraliteGrapheNorm > 0.5 ])

# Output:
# 6

print("Number of points before and after mean centrality : centraliteGrapheNorm > centraliteGrapheNormMoy")
length( centraliteGrapheNorm [ centraliteGrapheNorm > centraliteGrapheNormMoy ] )

# Output:
# 29

print("Number of points before and after mean centrality : centraliteGrapheNorm < centraliteGrapheNormMoy ")
length( centraliteGrapheNorm [ centraliteGrapheNorm < centraliteGrapheNormMoy ] )

# Output:
# 52

# Centrality of the new graphs
print("Centrality of the new graphs")
centraliteGrapheNormMoyNG

# Output:
# 0.1307393

# Number of points before and after mean centrality
print("Number of points before and after mean centrality > 0.5")
length( centraliteGrapheNormNG[ centraliteGrapheNormNG > 0.5 ])

# Output:
# 4

print("Number of points before and after mean centrality > centraliteGrapheNormMoy")
length( centraliteGrapheNormNG [ centraliteGrapheNormNG > centraliteGrapheNormMoy ] )

# Output:
# 11

print("Number of points before and after mean centrality > centraliteGrapheNormMoyNG")
length( centraliteGrapheNormNG [ centraliteGrapheNormNG > centraliteGrapheNormMoyNG ] )

# Output:
# 13

print("Number of points before and after mean centrality < centraliteGrapheNormMoyNG")
length( centraliteGrapheNormNG [ centraliteGrapheNormNG < centraliteGrapheNormMoyNG ] )

# Output:
# 40

cat("
  Observation:
  The centrality of the new graph has slightly decreased from 0.14 to 0.13.
  It is noted that most of the very important nodes (greater than 0.5) are always present.
  In the old graph we had 29 points (36%) above and 52 (64%) below the average centrality of the graph.
  Compared to the new graph which now has only 13 above (25%) against 40 (75%) below.
  The deleted group contained a large number of not very important nodes with some moderately important ones, which lowered the centrality a little.\n\n"
)
