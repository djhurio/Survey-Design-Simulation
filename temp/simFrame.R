####### Testing simFrame

### Install

# install.packages("simFrame", repos="http://cran.r-project.org/")
# install.packages("laeken", repos="http://cran.r-project.org/")
# install.packages("robCompositions", repos="http://cran.r-project.org/")
# install.packages("mvtnorm", repos="http://cran.r-project.org/")
# install.packages("rlecuyer", repos="http://cran.r-project.org/")


### Load

library("simFrame")
library("laeken")
library("robCompositions")
library("mvtnorm")


###

data("eusilcP")

ls()

head(eusilcP)

nrow(eusilcP)

colnames(eusilcP)

class(eusilcP)

set <- setup(eusilcP, size = 10, k = 2)
summary(set)

set

draw(eusilcP[, c("id", "eqIncome")], set, i = 1)
draw(eusilcP[, c("id", "eqIncome")], set, i = 2)


sc <- SampleControl(size = 500, k = 50)
cc <- DARContControl(target = "eqIncome", epsilon = 0.02, fun = function(x) x * 25)
sim <- function(x) {c(mean = mean(x$eqIncome), trimmed = mean(x$eqIncome, trim = 0.02))}

set.seed(12345)
results <- runSimulation(eusilcP, sc, contControl = cc, fun = sim)

head(results)

aggregate(results)

tv <- mean(eusilcP$eqIncome)
tv

plot(results, true = tv)
simBwplot(results, true = tv)
simDensityplot(results, true = tv)



data("eusilcP")
set.seed(12345)

set <- setup(eusilcP, design = "region", grouping = "hid", size = c(75, 250, 250, 125, 200, 225, 125, 150, 100),  k = 100)
sum(c(75, 250, 250, 125, 200, 225, 125, 150, 100))

cc <- DCARContControl(target = "eqIncome", epsilon = 0.005, grouping = "hid", dots = list(mean = 5e+05, sd = 10000))

sim <- function(x, k) {
     g <- gini(x$eqIncome, x$.weight)$value
     eqIncHill <- fitPareto(x$eqIncome, k = k, method = "thetaHill", groups = x$hid)
     gHill <- gini(eqIncHill, x$.weight)$value
     eqIncPDC <- fitPareto(x$eqIncome, k = k, method = "thetaPDC", groups = x$hid)
     gPDC <- gini(eqIncPDC, x$.weight)$value
     c(standard = g, Hill = gHill, PDC = gPDC)
}

results <- runSimulation(eusilcP, set, contControl = cc, design = "gender", fun = sim, k = 125)

head(results)

aggregate(results)

tv <- simSapply(eusilcP, "gender", function(x) gini(x$eqIncome)$value)

plot(results, true = tv)
simDensityplot(results, true = tv)



### Model based

set.seed(12345)

crnorm <- function(n, mean, sigma) invilr(rmvnorm(n, mean, sigma))
sigma <- matrix(c(1, -0.5, 1.4, -0.5, 1, -0.6, 1.4, -0.6, 2), 3, 3)
dc <- DataControl(size = 150, distribution = crnorm, dots = list(mean = c(0, 2, 3), sigma = sigma))

nc <- NAControl(NArate = 0.05)

sim <- function(x, orig) {
	i <- apply(x, 1, function(x) any(is.na(x)))
	ni <- length(which(i))
	xKNNa <- impKNNa(x)$xImp
	xLS <- impCoda(x, method = "lm")$xImp
	c(knn = aDist(xKNNa, orig)/ni, LS = aDist(xLS, orig)/ni)
}

results <- runSimulation(dc, nrep = 50, NAControl = nc, fun = sim)

head(results)
aggregate(results)

plot(results, xlab = "Relative Aitchison distance")
simDensityplot(results, alpha = 0.6, xlab = "Relative Aitchison distance")



### Parallel computing

### One CPU

cl <- makeCluster(1, type = "SOCK")

clusterEvalQ(cl, {
	library("simFrame")
	library("robCompositions")
	library("mvtnorm")
})

clusterSetupRNG(cl, seed = 12345)

crnorm <- function(n, mean, sigma) invilr(rmvnorm(n, mean, sigma))
sigma <- matrix(c(1, -0.5, 1.4, -0.5, 1, -0.6, 1.4, -0.6, 2), 3, 3)
dc <- DataControl(size = 150, distribution = crnorm, dots = list(mean = c(0, 2, 3), sigma = sigma))
nc <- NAControl(NArate = c(0.01, 0.03, 0.05, 0.07, 0.09))
sim <- function(x, orig) {
	i <- apply(x, 1, function(x) any(is.na(x)))
	ni <- length(which(i))
	xKNNa <- impKNNa(x)$xImp
	xLS <- impCoda(x, method = "lm")$xImp
	c(knn = aDist(xKNNa, orig)/ni, LS = aDist(xLS, orig)/ni)
}

clusterExport(cl, c("crnorm", "sigma", "dc", "nc", "sim"))

# results <- clusterRunSimulation(cl, dc, nrep = 50, NAControl = nc, fun = sim)
stime <- system.time(clusterRunSimulation(cl, dc, nrep = 50, NAControl = nc, fun = sim))[3]

stopCluster(cl)


### Two CPUs

cl <- makeCluster(2, type = "SOCK")

clusterEvalQ(cl, {
	library("simFrame")
	library("robCompositions")
	library("mvtnorm")
})

clusterSetupRNG(cl, seed = 12345)

crnorm <- function(n, mean, sigma) invilr(rmvnorm(n, mean, sigma))
sigma <- matrix(c(1, -0.5, 1.4, -0.5, 1, -0.6, 1.4, -0.6, 2), 3, 3)
dc <- DataControl(size = 150, distribution = crnorm, dots = list(mean = c(0, 2, 3), sigma = sigma))
nc <- NAControl(NArate = c(0.01, 0.03, 0.05, 0.07, 0.09))
sim <- function(x, orig) {
	i <- apply(x, 1, function(x) any(is.na(x)))
	ni <- length(which(i))
	xKNNa <- impKNNa(x)$xImp
	xLS <- impCoda(x, method = "lm")$xImp
	c(knn = aDist(xKNNa, orig)/ni, LS = aDist(xLS, orig)/ni)
}

clusterExport(cl, c("crnorm", "sigma", "dc", "nc", "sim"))

# results <- clusterRunSimulation(cl, dc, nrep = 50, NAControl = nc, fun = sim)
ptime <- system.time(clusterRunSimulation(cl, dc, nrep = 50, NAControl = nc, fun = sim))[3]

stopCluster(cl)


### Results

stime
ptime
stime/ptime





####### Testing simFrame 2
### http://cran.r-project.org/web/packages/simFrame/vignettes/simFrame-intro.pdf

rm(list = ls())

library(simFrame)

showMethods("setNA")


nc <- NAControl(NArate = 0.05)
getNArate(nc)

setNArate(nc, c(0.01, 0.03, 0.05, 0.07, 0.09))
getNArate(nc)

library("mvtnorm")
dc <- DataControl(size = 10, distribution = rmvnorm, dots = list(mean = rep(0, 2), sigma = matrix(c(1, 0.5, 0.5, 1), 2, 2)))

set.seed(12345)
foo <- generate(dc)
foo


data("eusilcP")
set.seed(12345)
set <- setup(eusilcP, size = 10, k = 2)
summary(set)
set

draw(eusilcP[, c("id", "eqIncome")], set, i = 1)

cc <- DARContControl(target = "V2", epsilon = 0.2, fun = function(x) x * 100)

set.seed(12345)
bar <- contaminate(foo, cc)
bar


nc <- NAControl(NArate = 0.3)

setNA(bar, nc)

data("eusilcP")
sc <- SampleControl(size = 500, k = 50)
cc <- DARContControl(target = "eqIncome", epsilon = 0.02, fun = function(x) x * 25)
sim <- function(x) {c(mean = mean(x$eqIncome), trimmed = mean(x$eqIncome, trim = 0.02))}
set.seed(12345)
results <- runSimulation(eusilcP, sc, contControl = cc, fun = sim)

head(results)

aggregate(results)

tv <- mean(eusilcP$eqIncome)
tv

plot(results, true = tv)
simDensityplot(results, true = tv)



library("laeken")
data("eusilcP")
set.seed(12345)

set <- setup(eusilcP, design = "region", grouping = "hid", size = c(75, 250, 250, 125, 200, 225, 125, 150, 100), k = 100)


