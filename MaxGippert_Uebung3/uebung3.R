#Übung 3
#Max Gippert
library(dplyr)
library(tidyverse)
setwd("E:/Desktop/Studium/Biophysik/Semester 4/Bioinformatik/Übungen/Übung3")

#Aufgabe 3.1
seqs <- read.table("seqs.txt", sep=" ")
seqs <- t(seqs)
seqs[,1]

distP <- function(seqMat){ #Funktion zur Ermittlung der p-Distanz
  #Berechnung der p-scores
  pScores <- matrix(0,nrow=length(seqMat[1,]), ncol=length(seqMat[1,]))
  for(i in 1:(length(seqMat[1,]))){     #Spalten werden durchgegangen, Startsequenz wird hierdurch ausgewählt
    for(j in 1:i){                      #Vergleichsequenz wird ausgewählt
      for(a in 1:(length(seqMat[,1]))){ #Zeilen werden durchgegangen und verglichen
        if(seqMat[a,i]!=seqMat[a,j]){   #Wenn ungleiche AA, wird Wert in Matrix um 1 erhöht
          pScores[i,j] <- pScores[i,j]+1
          pScores[j,i] <- pScores[j,i]+1
          }   
      }
    }
  }
  #Nun ist pScore mit Unterschieden zwischen den Sequenzen gefüllt
  distP <- pScores/(length(seqMat[,1]))
  #Ausgabe 
  return(distP)
}

distPoisson <- function(distP){ #Funktion zur Ermittlung der Poisson-Distanz
   distPoisson <- -log(1-distP)
   return(distPoisson)
}
 
distJK <- function(distP){ #Funktion zur Ermittlung der Jukes-Cantor-Distanz
   distJK <- ifelse(distP[,c(1:length(distP[1,]))]<0.75, -(3/4)*log(1-(4/3)*distP), NA)
   return(distJK)
}

distP(seqs)
distPoisson(distP(seqs))
distJK(distP(seqs))


#Aufgabe 3.3
library(phangorn)
fdir <- system.file("extdata/trees", package = "phangorn")
primates <- read.phyDat(file.path(fdir, "primates.dna"),
                        format = "interleaved")
distMprimates <- dist.ml(primates)

plot(upgma(distMprimates), main="UPGMA")
plot(NJ(distMprimates), "unrooted", main="NJ")

#3.3.1
#Unter diesen Arten befindet sich ein Nicht-Primat - die Maus. 
#Im Ultrametrischen Baum zeigt sich dies dadurch, dass der LUA der Maus und allen anderen Spezies evolutionär weiter zurück liegt, 
#als der LUA aller Spezies ohne Maus.
#Im Diagramm, welches durch die Neighbor-joining Methode erstellt wurde, zeigt sich diese Sonderrolle der Maus nicht so klar, 
#doch die zur Maus führende Kante ist die längste aller Spezies.
#
#3.3.2
#Nach der UPGMA-Methode formt der Lemur mit den Spezies "Bovine" einen kleinen Cluster, eine evolutionäre Stufe weiter zurück
#bildet sich auch ein Cluster mit "Tarsier", der schlussendlich im LUA aller betrachteten Primaten eingeht
#
#3.3.3
#Der Mensch bildet den kleinsten cluster mit den Schimpansen, in einem größeren Maßstab gehören in dieser Reihenfolge noch die Gorillas, 
#Orang-Utans und Gibbons zu dem Cluster, welchem der Mensch zugeordnet werden kann.
