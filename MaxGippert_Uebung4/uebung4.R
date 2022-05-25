#Uebung 4
#Max Gippert

#Aufgabe 4.2
setwd("C:/Users/mxgi0/Desktop/Studium/Biophysik/Semester 4/Bioinformatik/Übungen/Übung4")
input <- read.table("human_ACTG_Isoforms.txt", header=F, sep="\t")

#Allgemeine Funktion
inputToList <- function(inSeq){    #Liste mit Sequenzen als Vektoren erstellen
  seqs <- list() #Liste, um einen Vektor von Vektoren zu erstellen

  for(i in 1:length(inSeq[,1])){
    sequenz <- strsplit(toString(inSeq[i,1]), split="")
    sequenz <- as.character(sequenz[[1]])
    seqs[[i]] <- sequenz
  } 
  return(seqs)
}
human_ACTG_Isoforms <- inputToList(input)


konsensAlignmentList <- function(seqList){
  vergleich <- vector()
  konsens <- vector()
  
  #Betrachte jeweils das i-te Element und speichere alle diese Elemente in einem Vektor
  for(i in 1:length(seqList[[1]])){
      for(j in 1:length(seqList)){
        vergleich <- c(vergleich, seqList[[j]][i])
      }
    konsens <- c(konsens, sort(vergleich, decreasing=T)[1])
    vergleich <- vector()
  }
  return(konsens)
}
konsensAlignmentList(human_ACTG_Isoforms)

#Ergebnis:
#[1] "M" "E" "E" "E" "I" "A" "A" "L" "V" "I" "D" "N" "G" "S" "G" "M" "C" "K" "A" "G" "F" "A" "G" "D" "D" "A" "P" "R" "A"
#[30] "V" "F" "P" "S" "I" "V" "G" "R" "S" "W" "W" "Q" "W" "V" "R" "V" "T" "M" "G" "Q" "K" "P" "T" "Y" "V" "T" "R" "P" "R"
#[59] "Q" "S" "V" "R" "S" "I" "L" "T" "L" "K" "Y" "P" "I" "E" "H" "G" "I" "V" "T" "N" "W" "D" "D" "M" "E" "K" "I" "W" "H"
#[88] "H" "T" "F" "Y" "N" "E" "L" "R" "V" "A" "P" "E" "E" "H" "P" "V" "L" "L" "T" "E" "A" "P" "L" "N" "P" "K" "A" "N" "R"
#[117] "E" "K" "M" "T" "Q" "I" "M" "F" "L" "T" "R" "N" "T" "P" "P" "M" "Y" "V" "A" "I" "Q" "S" "V" "L" "S" "L" "Y" "P" "S"
#[146] "T" "R" "R" "P" "C" "T" "W" "P" "S" "R" "P" "C" "C" "P" "S" "T" "P" "L" "G" "A" "T" "T" "G" "L" "V" "W" "T" "S" "G"
#[175] "T" "G" "V" "T" "T" "T"




#Hier anderer Ansatz, mit Data Frame gelöst
###

inputToDataFrame <- function(inSeq){    #Data Frame mit Sequenzen als Spalten erstellen
  
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
####
human_ACTG_Isoforms_DF <- inputToDataFrame(input)


#Funktion, welche einem Data.Frame von Sequenzene eine Spalte mit den Konsens-Alignment anfügt und dieses ausgibt

konsensAlignmentDF <- function(seqDF){
  
  seqDF$konsens <- vector(length=length(seqDF[,1]), mode="character") #Neue Spalte in der Tabelle mit häufigsten Element der jeweiligen Zeile 
  for(i in 1:length(seqDF[,1])){
    seqDF$konsens[i] <- sort(as.character(seqDF[i,]), decreasing=T)[1]
  }
  return(seqDF$konsens)
}

konsensAlignmentDF(human_ACTG_Isoforms_DF)
#Ergebnis:
#[1] "M" "E" "E" "E" "I" "A" "A" "L" "V" "I" "D" "N" "G" "S" "G" "M" "C" "K" "A" "G" "F" "A" "G" "D" "D" "A" "P" "R" "A"
#[30] "V" "F" "P" "S" "I" "V" "G" "R" "S" "W" "W" "Q" "W" "V" "R" "V" "T" "M" "G" "Q" "K" "P" "T" "Y" "V" "T" "R" "P" "R"
#[59] "Q" "S" "V" "R" "S" "I" "L" "T" "L" "K" "Y" "P" "I" "E" "H" "G" "I" "V" "T" "N" "W" "D" "D" "M" "E" "K" "I" "W" "H"
#[88] "H" "T" "F" "Y" "N" "E" "L" "R" "V" "A" "P" "E" "E" "H" "P" "V" "L" "L" "T" "E" "A" "P" "L" "N" "P" "K" "A" "N" "R"
#[117] "E" "K" "M" "T" "Q" "I" "M" "F" "L" "T" "R" "N" "T" "P" "P" "M" "Y" "V" "A" "I" "Q" "S" "V" "L" "S" "L" "Y" "P" "S"
#[146] "T" "R" "R" "P" "C" "T" "W" "P" "S" "R" "P" "C" "C" "P" "S" "T" "P" "L" "G" "A" "T" "T" "G" "L" "V" "W" "T" "S" "G"
#[175] "T" "G" "V" "T" "T" "T"

konsensAlignmentList(human_ACTG_Isoforms) == konsensAlignmentDF(human_ACTG_Isoforms_DF) 
#TRUE
