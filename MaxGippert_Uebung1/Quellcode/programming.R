#Programming in R
library(dslabs)
data(murders)
murder_rate <- murders$total/murders$population*100000


#1. Conditionals
a <- 3
if(a!=0){
  print(1/a)
} else{
  print("No reciprocal for 0.")
}

ifelse(a > 0, 1/a, NA) 
b <- c(1,2,3,-4,5)
ifelse(b>0, 1/b, NA) #funktioniert wie for-Schleife auf Vektoren!

ind <- which.min(murder_rate)
if(murder_rate[ind]<0.25){
  print(murders$state[ind])
} else{
  print("No state has a murder rate that low")
}

data(na_example)
sum(is.na(na_example))
no_nas <- ifelse(is.na(na_example), 0, na_example)
sum(is.na(no_nas))


#any-function
#TRUE, wenn min ein Eintrag TRUE
z <- c(TRUE, FALSE, FALSE, FALSE)
any(z)
#all-function
#TRUE, wenn alle Einträge TRUE
all(z)



#2. Funktionen
avg <- function(x){ #Parameter in den Klammern
  a <- sum(x)/length(x) #variables only exist during the call for the function and in the function
  return(a)
}

test <- seq(2,100,2)
avg(test)
mean(test)

avgplus <- function(x, arithmetic=TRUE){
  n <- length(x)
  ifelse(arithmetic, sum(x)/n, prod(x)^(1/n)) #letzte line der Funktion wird ausgegeben
}
avgplus(test, arithmetic=FALSE)


#3. Loops
compute_sum <- function(n){
  x <- 1:n
  sum(x)
}
compute_sum(3)
#Vectorausgabe mit for-loops

compute_sums <- function(n){
  ausgabe <- vector(length=n)
  for(i in 1:n){
    ausgabe[i] <- compute_sum(i)
  }
  ausgabe
}
compute_sums(6)
plot(compute_sums(50))
lines(n, n*(n+1)/2)



#4. Weiteres 
?apply()
?sapply()
?tapply()
?mapply()
?split()
?cut()
?quantile()
?reduce()
?identical()
?unique()

