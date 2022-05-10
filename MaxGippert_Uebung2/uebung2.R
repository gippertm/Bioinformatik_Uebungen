#Uebung 2 
#Max Gippert
library(dplyr)

#Aufgabe 2.2
setwd("C:/Users/mxgi0/Desktop/Studium/Biophysik/Semester 4/Bioinformatik/Übungen/Übung2")

sequenzen <- read.table("alignedSequencesCalmodulin.txt", header=F, sep="\t")

sequenz1 <- strsplit(toString(sequenzen$V1[1]), split="")
sequenz1 <- as.character(sequenz1[[1]]) #Mir ist nicht ganz klar, was hier passiert ist
#es sieht so aus, dass jeder Eintrag der urspünglichen sequenz1 durch den Aufruf in Zeile 10 erneut in einen gesamten Vektor verpackt wurde

sequenz2 <- strsplit(toString(sequenzen$V1[2]), split="")
sequenz2 <- as.character(sequenz2[[1]])

vergleich <- sequenz1==sequenz2

#Prozentuale Übereinstimmung 
result <- sum(vergleich)/length(sequenz1)
paste0("Die prozentuale Übereinstimmung beträgt ", result*100, "%")

#Antwort: Die prozentuale Übereinstimmung beträgt 96.6666666666667%


#Aufgabe 2.3
blosum62 <- read.table("BLOSUM62.txt", header=T)
blosum62 <- rename(blosum62, "*"="X.")
sequenz1[length(sequenz1)]

blosum_score <- function(sequence1, sequence2){
  score=0
  for(i in 1:length(sequence1)){
    score <- score + blosum62[sequence1[i], sequence2[i]]
  }
  score
}

paste0("Der Score nach  BLOSUM62 ist für dieses Alignment ", blosum_score(sequenz1, sequenz2))

#Antwort: Der Score nach  BLOSUM62 ist für dieses Alignment 730