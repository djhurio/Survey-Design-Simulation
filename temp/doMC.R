### Testing of doMC
### MLiberts
### 25/12/2010 14:37:01

# install.packages("doMC", repos="http://cran.r-project.org/")

library(doMC)
registerDoMC()


###

foreach(i = 1:3) %dopar% sqrt(i)



###

x <- iris[which(iris[, 5] != "setosa"), c(1, 5)]
head(x)
dim(x)

trials <- 3e3

ptime <- system.time({
	r <- foreach(icount(trials), .combine = cbind) %dopar% {
		ind <- sample(100, 100, replace = TRUE)
		result1 <- glm(x[ind, 2] ~ x[ind, 1], family = binomial(logit))
		coefficients(result1)
	}
})[3]

ptime


stime <- system.time({
	r <- foreach(icount(trials), .combine = cbind) %do% {
		ind <- sample(100, 100, replace = TRUE)
		result1 <- glm(x[ind, 2] ~ x[ind, 1], family = binomial(logit))
		coefficients(result1)
	}
})[3]

stime

stime/ptime


getDoParWorkers()
getDoParName()
getDoParVersion()

