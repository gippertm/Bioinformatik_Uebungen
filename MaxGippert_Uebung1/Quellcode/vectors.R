codes <- c(380,124,818)
country <- c("italy","canada","eqypt")
codes <- c(italy=380,canada=124,eqypt=818)
#codes <- c("italy"=380,"canada"=124,"eqypt"=818)
#names(codes) <- country macht auch das gleiche

codes
class(codes)

seq(1,10)
seq(1,10,2)
1:10
seq(2,100,length.out=10) #in gleich große Abschnitte unterteilt

#verwende [] um einzelne Elemente aufzurufen
codes[2]
codes[c(1,3)]
codes[1:2]
codes["canada"]
codes[c("italy","canada")]

#Coercion -> flexible Datentypanalyse durch R
x <- c(1,"canada",3)
x
class(x) #alle Eingaben als char interpretiert

y <- 1:5
z <- as.character(y)
z

#missing data --> NA value
x <- c("1","b","3")
as.numeric(x)
#logical vector
na_vec <- is.na(x)

#Integer definieren mit Zahl+L
class(4L)

#Data-Frames definieren
tab <- data.frame(name = murders$state, 
                  rank = rank(murders$population))
tab
#R definiert char automatisch als factors (laut tutorial, is aber nicht so)
tabelle <- data.frame(names=c("Andy", "Michael", "Jim", "Dwight"),
                     popularity = c(20, 60, 100, 10), 
                     stringsAsFactors = FALSE)
class(tabelle$names)


#Analyse
height <- c(1:10)
height*2.5
height-mean(height)
# Vektoren werden komponentenweise berechnet, alle mathematischen Operationen

murder_rate <- murders$total/murders$population*100000
murder_rate

order(murders_rate)
murders$state[order(murder_rate,decreasing = TRUE)]

#aufgaben
name <- c("Mandi", "Amy", "Nicole", "Olivia")
distance <- c(0.8, 3.1, 2.8, 4.0)
time <- c(10, 30, 40, 50)

time_h <- time/60
speed <- distance/time_h
tab <- data.frame(names=name, time=time_h, speed=speed)
tab