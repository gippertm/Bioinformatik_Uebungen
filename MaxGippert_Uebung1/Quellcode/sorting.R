library(dslabs)
data(murders)

#einfache Sortierung
sort(murders$total)

x <- c(31,4,15,92,65)
x
sort(x)
index <- order(x) #gibt indizes der sortierten Liste
x[index]

#damit kombination mehrerer Vektoren
murders$state[1:10]
murders$abb[1:10]

murders$state[order(murders$total)]

#Besondere Werte
max(murders$total)
i_max <- which.max(murders$total)
i_max
murders$state[i_max]

#rank der nativen reihenfolge
rank(x)

#Durchschnitt mit
mean(murders$population)
