#Übung 10
#Max Gippert

#install.packages("igraph")
library(igraph)
help("igraph")


setwd("C:/Users/mxgi0/Desktop/Studium/Biophysik/Semester 4/Bioinformatik/Übungen/UE10")
input <- as.matrix(read.csv("proteinInteractionNetworkHomoSapiensSmall.csv"))

#Graphen aus Edgelist erstellen
graph <- graph_from_edgelist(input)

#Visualisierung aus Graphen erstellen
plot.igraph(graph)

degreeGraph <- degree(graph)
degreeGraph
#AP4M1      APP    APBA1     LRP8    PSEN1    APBA2    APBB1    APLP1     LRP1    APLP2   NOTCH1     KAT5    APBB2 
#1       50        3       10       11        2        8        7       11        7       10        7        4 
#APH1A    NCSTN   PSENEN     DAB1     PRNP    CNTN4 MAPK8IP1    APOA1     GPC1    APOA4      CLU     APOE   TOMM40 
#4        7        4        8        7        2        5        5        5        5        6       10        3 
#RELN    SORL1    RTN4R     NGFR     NTN1     GSAP    EP300      IDE    ITGB1    CASP3    TGFB2    NTRK1    GULP1 
#4        4        2        6        1        3        7        1        6        1        1        6        2 
#F11    CNTN2     KLC1    ITM2B     KLK6     GRB2    SPON1     NAE1     SHC1   PITRM1    BACE1 TNFRSF21     RBPJ 
#1        2        2        3        1        7        2        1        7        1        6        2        4 
#MAML1     TP53    ITGA5   TOMM22 
#4        5        1        1 

closenessGraph <- closeness(graph, normalized=T)
closenessGraph
#AP4M1        APP      APBA1       LRP8      PSEN1      APBA2      APBB1      APLP1       LRP1      APLP2 
#0.01136364 0.02222222 0.01162791 0.07142857 1.00000000 0.01149425 0.01176471 0.01162791 0.16666667 0.01190476 
#NOTCH1       KAT5      APBB2      APH1A      NCSTN     PSENEN       DAB1       PRNP      CNTN4   MAPK8IP1 
#0.16666667 0.05555556 0.01111111 0.01176471 0.12500000        NaN 0.04000000        NaN        NaN        NaN 
#APOA1       GPC1      APOA4        CLU       APOE     TOMM40       RELN      SORL1      RTN4R       NGFR 
#0.01136364 1.00000000 0.01149425 0.04166667 0.01234568        NaN        NaN        NaN        NaN 0.09090909 
#NTN1       GSAP      EP300        IDE      ITGB1      CASP3      TGFB2      NTRK1      GULP1        F11 
#NaN 0.07692308 0.04166667        NaN 0.10000000        NaN        NaN 0.16666667 0.08333333        NaN 
#CNTN2       KLC1      ITM2B       KLK6       GRB2      SPON1       NAE1       SHC1     PITRM1      BACE1 
#0.02702703 1.00000000 0.09090909        NaN 0.04545455        NaN        NaN 1.00000000        NaN 0.04761905 
#TNFRSF21       RBPJ      MAML1       TP53      ITGA5     TOMM22 
#NaN 1.00000000 0.12500000        NaN 0.05882353 1.00000000 
?betweenness
betweennessGraph <- betweenness(graph)
betweennessGraph
#AP4M1         APP       APBA1        LRP8       PSEN1       APBA2       APBB1       APLP1        LRP1 
#0.0000000 402.2500000   0.0000000  14.7500000  14.0000000   0.0000000   0.0000000   2.5000000  31.2500000 
#APLP2      NOTCH1        KAT5       APBB2       APH1A       NCSTN      PSENEN        DAB1        PRNP 
#3.4166667  33.7500000  16.5000000   0.0000000   0.0000000   7.3333333   0.0000000   5.2500000   0.0000000 
#CNTN4    MAPK8IP1       APOA1        GPC1       APOA4         CLU        APOE      TOMM40        RELN 
#0.0000000   0.0000000   0.0000000   0.9166667   0.0000000   1.5833333   4.6666667   0.0000000   0.0000000 
#SORL1       RTN4R        NGFR        NTN1        GSAP       EP300         IDE       ITGB1       CASP3 
#0.0000000   0.0000000   2.0000000   0.0000000   0.0000000  31.6666667   0.0000000   9.8333333   0.0000000 
#TGFB2       NTRK1       GULP1         F11       CNTN2        KLC1       ITM2B        KLK6        GRB2 
#0.0000000   7.5000000   0.0000000   0.0000000   0.0000000   0.0000000   2.5000000   0.0000000   0.0000000 
#SPON1        NAE1        SHC1      PITRM1       BACE1    TNFRSF21        RBPJ       MAML1        TP53 
#0.0000000   0.0000000  12.5000000   0.0000000   0.0000000   0.0000000   5.8333333   0.0000000   0.0000000 
#ITGA5      TOMM22 
#0.0000000   0.0000000 

closenessGraph <- na.omit(closenessGraph)
betweennessGraph <- na.omit(betweennessGraph)

close <- as.vector(closenessGraph)
namesClose <- names(closenessGraph)
#closeSort <- sort(closeDF[,1], decreasing = T)
cDF <- data.frame(name=namesClose, closeness = close)
cSort <- cDF[order(cDF[,2], decreasing = T),]

between <- as.vector(betweennessGraph)
namesBetween <- names(betweennessGraph)
bDF <- data.frame(name=namesBetween, betweenness = between)
bSort <- bDF[order(bDF[,2], decreasing = T),]

#Erstellung einer Rangsumme zur Einschätzung der Wichtigkeit
bSortnew <- subset(bSort, bSort[,1] %in% cSort[,1])

index <- c()
for(i in 1:length(bSortnew[,1])){
index <- c(index, which(bSortnew[i,1]==cSort[,1]))
}
cSort <- cSort[index,]
rank(-cSort[,2], ties.method = "min")+rank(-bSortnew[,2], ties.method = "min")
?rank
importanceRank <- data.frame(nameC = cSort[,1], rankSum = rank(-cSort[,2], ties.method = "min")+rank(-bSortnew[,2], ties.method = "min"))
importanceRank <- importanceRank[order(importanceRank$rankSum, decreasing = F),]


#5 wichtigsten Proteine nach jeder Kategorie
head(cDF[order(cDF[,2], decreasing = T),])
head(bSort)
head(importanceRank)
head(subset(cDF[order(cDF[,2], decreasing = T)[-c(1:6)],], ))


#> head(cDF[order(cDF[,2], decreasing = T),])
#name closeness
#5   PSEN1         1
#18   GPC1         1
#29   KLC1         1
#32   SHC1         1
#34   RBPJ         1
#37 TOMM22         1

#> head(bSort)
#name betweenness
#2     APP   402.25000
#11 NOTCH1    33.75000
#33  EP300    31.66667
#9    LRP1    31.25000
#12   KAT5    16.50000
#4    LRP8    14.75000

#> head(importanceRank)
#nameC rankSum
#1     APP       8
#7   PSEN1       8
#8    SHC1       9
#2  NOTCH1      11
#4    LRP1      12
#12   RBPJ      13

#> head(subset(cDF[order(cDF[,2], decreasing = T)[-c(1:6)],], ))
#name closeness
#2     APP 0.9333333
#9    LRP1 0.8333333
#11 NOTCH1 0.6666667
#26  NTRK1 0.6666667
#22   NGFR 0.6363636
#15  NCSTN 0.6250000