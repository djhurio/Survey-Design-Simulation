######### Testing simFrame on real data


### Reset

rm(list=ls())


### Libs

library("simFrame")


### Pop

# Workdir
dir = "C:/DATA/LU/Results"
dir_sim = "C:/DATA/LU/sim"


### DATA

setwd(dir)

load("pop2.Rdata")
ls()


### Pop

class(pop)
dim(pop)
head(pop)



### simFrame

showMethods("setup")

set <- setup(pop, size = 10, k = 2)
summary(set)

set

draw(pop, set, i = 1)
draw(pop, set, i = 2)


sc <- SampleControl(size = 500, k = 50)

### Mean

sim_mean <- function(x) {c(mean_y0 = mean(x$y0),
 mean_y1 = mean(x$y1),
 mean_y2 = mean(x$y2),
 mean_y3 = mean(x$y3),
 mean_y4 = mean(x$y4))}

results <- runSimulation(pop, sc, fun = sim_mean)

head(results)

aggregate(results)

tv <- colMeans(pop[c("y0","y1","y2","y3","y4")])
tv

simBwplot(results, true = tv)
simDensityplot(results, true = tv)


### Total

sim_tot <- function(x) {c(tot_y0 = sum(x$y0 * x$.weight),
 tot_y1 = sum(x$y1 * x$.weight),
 tot_y2 = sum(x$y2 * x$.weight),
 tot_y3 = sum(x$y3 * x$.weight),
 tot_y4 = sum(x$y4 * x$.weight))}

results <- runSimulation(pop, sc, fun = sim_tot)

head(results)

aggregate(results)

tv <- colSums(pop[c("y0","y1","y2","y3","y4")])
tv

simBwplot(results, true = tv)
simDensityplot(results, true = tv)



### Mean 2

sim_mean <- function(x) {c(mean_y0 = sum(x$y0 * x$.weight) / sum(x$.weight),
 mean_y1 = sum(x$y1 * x$.weight) / sum(x$.weight),
 mean_y2 = sum(x$y2 * x$.weight) / sum(x$.weight),
 mean_y3 = sum(x$y3 * x$.weight) / sum(x$.weight),
 mean_y4 = sum(x$y4 * x$.weight) / sum(x$.weight))}

results <- runSimulation(pop, sc, fun = sim_mean)

head(results)

aggregate(results)

tv <- colMeans(pop[c("y0","y1","y2","y3","y4")])
tv

simBwplot(results, true = tv)
simDensityplot(results, true = tv)


### Ratio

sim <- function(x) {c(r_y0_y0 = sum(x$y0 * x$.weight) / sum(x$y0 * x$.weight),
 r_y1_y0 = sum(x$y1 * x$.weight) / sum(x$y0 * x$.weight),
 r_y2_y0 = sum(x$y2 * x$.weight) / sum(x$y0 * x$.weight),
 r_y3_y0 = sum(x$y3 * x$.weight) / sum(x$y0 * x$.weight),
 r_y4_y0 = sum(x$y4 * x$.weight) / sum(x$y0 * x$.weight),
 r_y1_y4 = sum(x$y1 * x$.weight) / sum(x$y4 * x$.weight),
 r_y2_y4 = sum(x$y2 * x$.weight) / sum(x$y4 * x$.weight),
 r_y4_y4 = sum(x$y4 * x$.weight) / sum(x$y4 * x$.weight))}

results <- runSimulation(pop, sc, fun = sim)

head(results)

aggregate(results)

tv <- colSums(pop[c("y0","y1","y2","y3","y4","y1","y2","y4")]) / colSums(pop[c("y0","y0","y0","y0","y0","y4","y4","y4")])
tv

simBwplot(results, true = tv)
simDensityplot(results, true = tv)




#### Stratified cluster sampling

head(pop)

set <- setup(pop, design = "strata", grouping = "H_ID", size = c(50, 50, 50, 50),  k = 100)

summary(set)

s1 <- draw(pop, set, i = 1)

head(s1)

s1$n <- 1

s1_H <- aggregate(s1$n, s1[c("H_ID","strata",".weight")], sum)

head(s1_H)

s1_H$n <- 1

aggregate(s1_H$n, s1_H["strata"], sum)
aggregate(s1_H$.weight, s1_H["strata"], mean)



sim <- function(x) {c(r_y0_y0 = sum(x$y0 * x$.weight) / sum(x$y0 * x$.weight),
 r_y1_y0 = sum(x$y1 * x$.weight) / sum(x$y0 * x$.weight),
 r_y2_y0 = sum(x$y2 * x$.weight) / sum(x$y0 * x$.weight),
 r_y3_y0 = sum(x$y3 * x$.weight) / sum(x$y0 * x$.weight),
 r_y4_y0 = sum(x$y4 * x$.weight) / sum(x$y0 * x$.weight),
 r_y1_y4 = sum(x$y1 * x$.weight) / sum(x$y4 * x$.weight),
 r_y2_y4 = sum(x$y2 * x$.weight) / sum(x$y4 * x$.weight),
 r_y4_y4 = sum(x$y4 * x$.weight) / sum(x$y4 * x$.weight))}


results <- runSimulation(pop, set, fun = sim)

head(results)

aggregate(results)

tv <- colSums(pop[c("y0","y1","y2","y3","y4","y1","y2","y4")]) / colSums(pop[c("y0","y0","y0","y0","y0","y4","y4","y4")])
tv

plot(results, true = tv)
simDensityplot(results, true = tv)

