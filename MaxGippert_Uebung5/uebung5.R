#Uebung 5
#Max Gippert
#Aufgabe 5.3 siehe unten
library(dplyr)

setwd("C:/Users/mxgi0/Desktop/Studium/Biophysik/Semester 4/Bioinformatik/Übungen/Übung5")
#Allgemeine Funktionen

###### Liste mit Sequenzen als Vektoren erstellen
inputToList <- function(inSeq){    
  seqs <- list() #Liste, um einen Vektor von Vektoren zu erstellen
  
  for(i in 1:length(inSeq[,1])){
    sequenz <- strsplit(toString(inSeq[i,1]), split="")
    sequenz <- as.character(sequenz[[1]])
    seqs[[i]] <- sequenz
  } 
  return(seqs)
}

###### Data Frame mit Sequenzen als Spalten erstellen
inputToDataFrame <- function(inSeq){    
  #erste Sequenz aufteilen, um Anzahl der benötigten Zeilen zu erhalten
  sequenz <- strsplit(toString(inSeq[1,1]), split="")
  sequenz <- as.character(sequenz[[1]])
  
  seqs <- data.frame(matrix(NA, #DataFrame der richtigen Dimension erstellen
                            nrow=length(sequenz), 
                            ncol=length(inSeq[,1])))
  seqs[,1] <- sequenz 
  
  for(i in 2:length(inSeq[,1])){  #erste Spalte wurde bereits ermittelt
    sequenz <- strsplit(toString(inSeq[i,1]), split="")
    sequenz <- as.character(sequenz[[1]])
    seqs[,i] <- sequenz
  } 
  
  colnames(seqs) <- c(1:length(inSeq[,1])) #Spalten sind nummeriert
  return(seqs)
}



#Aufgabe 5.3
input <- read.table("sequenz.txt", header=F, sep="\t")
sequenzList <- inputToList(input)


k_mer <- function(seq, k){ 
  #DataFrame, in welchen alle k-mere gespeichert werden
  kMers = data.frame(matrix(NA,           
                            nrow=(length(seq)-k), 
                            ncol=2))
  kMers[,1] <- ""
  kMers[1,] <- 0
  
  for(i in 1:(length(seq)-(k-1))){
    
    if(paste(c(seq[i:(i+k-1)]), collapse = "")%in%kMers[,1]==F){ #Wenn noch kein Eintrag dieses kmers vorhanden ist
      kMers[i,1] <- paste(c(seq[i:(i+k-1)]), collapse = "") #wird die Sequenz in die erste Spalte eingetragen
      kMers[i,2] <- 1 #und der counter (zweite Spalte) auf eins gesetzt
    }else{ #sonst wird der counter des Eintrages, der dem kmer entspricht um eins erhöht
      index <- match(paste(c(seq[i:(i+k-1)]), collapse = ""), kMers[,1])
      index
      kMers[index,2] <- kMers[index,2]+1
    }
  }
  kMers <- subset(kMers, kMers[,1]!="") #lösche alle leeren Zeilen
  kMers <- kMers[order(kMers[,2], decreasing=T),] #sortiere den Dataframe nach dem counter
  kMers
}

#Lösungen

k_mer(sequenz_k_mer, 1)
# G  9
# T  8
# A  7
# C  6

k_mer(sequenz_k_mer, 2)
# TG  5
# GC  4
# AT  4
# CA  3
# GA  3

k_mer(sequenz_k_mer, 3)
# ATG  4
# GCA  3
# CAT  3
# TGC  2
# TGA  2

k_mer(sequenz_k_mer, 4)
# GCAT  3
# CATG  3
# TGCA  2
# ATGA  2
# ACGT  1

k_mer(sequenz_k_mer, 5)
# GCATG  3
# TGCAT  2
# CATGA  2
# ACGTT  1
# CGTTG  1