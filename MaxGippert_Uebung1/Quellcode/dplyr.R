install.packages("dplyr")
library(dplyr)

#mutate() added Zeilen und Spalten
#filter() um Daten zu filtern
#select() um spezifisch Daten auszuwählen
#%>% pipe-operator

murders <- mutate(murders,rate=total/population*100000)
head(murders)

filter(murders,rate <= 0.71)

new_table <- select(murders,state,region,rate)
filter(new_table,rate <= 0.71)

murders %>% select(state, region, rate) %>% filter(rate <= 0.71)



#summarizing
library(tidyverse)
library(dslabs)
s <- murders %>% filter(region=="West") %>% summarize(minimum=min(rate),
                                                 median=median(rate),
                                                 maximum=max(rate))
s
s$minimum

us_murder_rate <- murders %>% summarize(rate=sum(total)/sum(population)*10^5) %>% #.$rate fungiert als placeholder für die hier verarbeiteten Daten
class(us_murder_rate)
us_murder_rate%>%pull(rate) #so wird eine Zahl aus einem Dataframe

murders %>% 
  filter(region == "West") %>%
  summarize(range = quantile(rate, c(0, 0.5, 1)))


# returning minimum, median, and maximum as a data frame
my_quantile <- function(x){
  r <-  quantile(x, c(0, 0.5, 1))
  data.frame(minimum = r[1], median = r[2], maximum = r[3]) 
}
murders %>% 
  filter(region == "West") %>%
  summarize(my_quantile(rate))


#group then summarize

murders %>% group_by(region)
#es ändert sich die class() des data.frames
murders %>% group_by(region) %>% 
  summarize(median = median(rate))

#Sortieren von Tabellen
murders %>%
  arrange(population) %>%
  head(.)

murders %>%
  arrange(desc(population)) %>%
  head()

murders %>%
  arrange(region, rate) %>%
  head()

murders %>% top_n(10,rate)
murders %>% arrange(desc(rate)) %>% top_n(10)