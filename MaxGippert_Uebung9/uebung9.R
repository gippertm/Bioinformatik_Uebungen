#Uebung 9
#Max Gippert
#Aufgabe 9.3

setwd("E:/Desktop/Studium/Biophysik/Semester 4/Bioinformatik/Übungen/UE09")
input <- as.matrix(read.csv("genedata-for-knn.csv"))

#install.packages("class")
library(class)
train <- input[,1:2]
test1 <- c(0,0)
test2 <- c(0,5)
test3 <- c(0,-8)
test4 <- c(-5,10)

#9.3.1

#?knn
knn(train, test1, input[,3] , 100)
#[1] NO gesund

knn(train, test2, input[,3] , 100)
#[1] NO gesund

knn(train, test3, input[,3] , 100)
#[1] YES krank

knn(train, test4, input[,3] , 100)
#[1] NO gesund


#9.3.2

?knn.cv
valid <- knn.cv(train, input[,3], 100)
sum(input[,3]!=valid)
#[1] 110

#Diese Funktion überprüft die Güte des Trainingsdatensatzes, indem sie für jeden klassifizierten Datenpunkt eine knn-Klassifizierung durchführt.
#Dadurch lässt sich feststellen, ob der Datensatz für diesen Datenpunkt eine korrekte Zuordnung vornehmen würde. 
#Im Vergleich mit der ursprünglichen Klassifizierung kann so ermittelt werden, wie viele Datenpunkte korrekt zugeordnet werden.
#Für einen k-Wert von 10 ergeben sich in diesem Datensatz eine falsche Zuordnung von 110 Datenpunkten, also jeder 9. Datenpunkt wird falsch zugeordnet.
#Die ergibt einen groben Richtwert für die Wahrscheinlichkeit einer richtigen Vorhersage mit 8/9.