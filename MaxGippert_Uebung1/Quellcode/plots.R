#scatterplots
population_mill <- murders$population/10^6
total_gun_murders <- murders$total

plot(population_mill,total_gun_murders)

#histograms
hist(murders$rate)
murders$state[which.max(murders$rate)]

#boxplots
boxplot(rate~region, data=murders)