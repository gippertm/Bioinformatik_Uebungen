#Übung 6
#Max Gippert

BinomialExperiment <- function(n, p=0.5){
  hits <- 0
  #Simulation
  for(i in 1:n){
    if(runif(1)<=p){
      hits <- hits+1
    }
  }
  
  #Auswertung
  p_laplace <- (hits+1)/(n+2)
  
  print(paste0("Die Wahrscheinlichkeit für einen Treffer liegt bei ", p_laplace))
}

for(i in 1:5) BinomialExperiment(10000, p=0.75)
 
