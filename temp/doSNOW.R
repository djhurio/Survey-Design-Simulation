### Testing of doSNOW
### MLiberts
### 29/12/2010

# install.packages("doSNOW", repos="http://cran.r-project.org/")

library(doSNOW)

cl <- makeCluster(2) # I have two cores
registerDoSNOW(cl)

###

foreach(i = 1:3) %dopar% sqrt(i)



###

x <- iris[which(iris[, 5] != "setosa"), c(1, 5)]
head(x)
dim(x)

trials <- 400

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

stopCluster(cl)
