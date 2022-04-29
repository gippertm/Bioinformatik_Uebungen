library(dslabs)
data(murders)

murder_rate <- murders$total/murders$population*100000

#logical to index vectors
index <- murder_rate <= 0.71
index

murders$state[index]
sum(index)

west <- murders$region == "West"
safe <- murder_rate <= 1
index <- safe & west
murders$state[index]

#FUNKTIONEN
i1 <- which(murders$state == "Massachusetts") #welche Indizes sind TRUE
i1

i2 <- match(c("New York","Florida","Texas"),murders$state) #Indizes, welche dem ersten Objekt im zweiten entsprechen
i2

x <- c("a", "b", "c", "d", "e")
y <- c("a", "d", "f")
y %in% x #sind die Elemente in y auch in x, return logical vector

