library(dslabs)
data("murders")
##Typ der Variable
class(murders)
##Struktur der Variable
str(murders)
##Beginn des data.frames
head(murders)
##Bezeichungen der Spalten
names(murders)

##Daten
murders$population
##ergibt ein Vector:
pop <- murders$population
length(pop)
class(pop)

##characters
class(murders$state)

##factors
class(murders$region)
##für Kategorien, werden als integers gespeichert, eindeutige Zuweisung, weniger Speicher
