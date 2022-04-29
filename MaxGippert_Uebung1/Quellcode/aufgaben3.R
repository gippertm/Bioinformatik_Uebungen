library(dslabs)
data(heights)
options(digits = 3)    # report 3 significant digits for all answers
library(dplyr)

str(heights)

mean <- mean(heights$height)
ind1 <- which(heights$height > mean)
length(ind1)

ind2 <- which(heights$height>mean & heights$sex=="Female")
length(ind2)

length(which(heights$sex=="Female"))/length(heights$sex)

min <- as.vector(min(heights$height))
which(heights$height == min)

heights$sex[1032]

max(heights$height)

x <- 50:82

sum(!x%in%heights$height)

heights_2 <- mutate(heights,ht_cm = height*2.54)
heights_2$ht_cm[18]
mean(heights_2$ht_cm)

females <- heights_2 %>% filter(sex=="Female")
nrow(females)
mean(females$ht_cm)


data(olive)
head(olive)

plot(olive$palmitic,olive$palmitoleic)

hist(olive$eicosenoic)

boxplot(palmitic~region, data=olive)