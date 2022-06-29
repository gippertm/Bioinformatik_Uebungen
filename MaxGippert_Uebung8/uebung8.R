#Übung 8
#Max Gippert
#Aufgabe 7.4

#a) und b)
#install.packages("palmerpenguins")
library(palmerpenguins)
data(package = "palmerpenguins")
head(penguins)
str(penguins)

#1. Dataset
#Spezies:
levels(penguins$species)
#Inseln
levels(penguins$island)
 
#2. Dataset
head(penguins_raw)
penguins_raw$Region
penguins_raw$Island
penguins_raw$Species
penguins_raw$Stage


#c) 
peng <- penguins[,c(3:6)]
peng <- na.omit(peng)

#PCA:
penguins_pca <- prcomp(peng, scale. = T, center = T)
penguins_pca
#[1] 1.6594442 0.8789293 0.6043475 0.3293816
#
#Rotation (n x k) = (4 x 4):
#  PC1          PC2        PC3        PC4
#bill_length_mm     0.4552503 -0.597031143 -0.6443012  0.1455231
#bill_depth_mm     -0.4003347 -0.797766572  0.4184272 -0.1679860
#flipper_length_mm  0.5760133 -0.002282201  0.2320840 -0.7837987
#body_mass_g        0.5483502 -0.084362920  0.5966001  0.5798821

#Visualisation
biplot(penguins_pca, scale = 0)



#d) 
#install.packages("factoextra", "cluster")
library(factoextra)
library(cluster)

peng_scale <- scale(peng)

fviz_nbclust(peng_scale, kmeans, method = "wss")
kmeans(peng_scale, 3, nstart=50)
#K-means clustering with 3 clusters of sizes 123, 132, 87
#
#Cluster means:
#bill_length_mm bill_depth_mm flipper_length_mm body_mass_g
#1      0.6562677    -1.0983711         1.1571696   1.0901639
#2     -1.0465260     0.4858415        -0.8899121  -0.7694891
#3      0.6600059     0.8157307        -0.2857869  -0.3737654
#
#Clustering vector:
#  [1] 2 2 2 2 2 2 2 2 3 2 2 2 2 2 2 2 3 2 3 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 2 2 2 2 2 3 2 2 2 3 2 2 2 2 2
#[59] 2 2 3 2 2 2 2 2 2 2 3 2 2 2 3 2 3 2 2 2 3 2 3 2 2 2 2 2 2 2 2 2 3 2 2 2 3 2 2 2 3 2 3 2 2 2 2 2 2 2 3 2 3 2 3 2 3 2
#[117] 2 2 2 2 2 2 3 2 2 2 2 2 3 2 3 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
#[175] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
#[233] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
#[291] 3 3 3 3 2 3 2 3 3 3 3 3 3 3 2 3 2 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 2 3 3 3 3 3 3 3 3 3 3 3 3 3

#Within cluster sum of squares by cluster:
#  [1] 143.1502 122.1477 112.9852#
#(between_SS / total_SS =  72.3 %)#

