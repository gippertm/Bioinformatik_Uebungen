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

#Wie nach dem Gesetz der großen Zahlen zu erwarten, wird die Schätzung und die Varianz der Ergebnisse mit einer hohen 
#Stichprobengröße (n) immer besser und nähert sich dem tatsächlichen Wert für p an. Dabei gibt es bei niedrigen 
#zweistelligen n noch sehr hohe Schwankungen der Ergebnisse und diese liegen nur selten nahe am tatsächlichen Wert - 
#weichen teilweise um mehr als 30% ab
#Bei großen Zahlen ab ca. 1000 produziert die Simulation stets Ergebnisse, die immer sehr nahe (+-1) an der tatsächlichen
#Wahrscheinlichkeit liegen.
