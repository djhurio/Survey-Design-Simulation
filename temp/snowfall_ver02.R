############# Testing paralel computing by package snowfall
### Martins Liberts
### 29.12.2010

# Install package
# install.packages("snowfall", repos="http://cran.r-project.org/")

# Load package
library(snowfall)


###Data

x <- iris[which(iris[, 5] != "setosa"), c(1, 5)]


###Function

calcPar <- function(i) {
	ind <- sample(100, 100, replace = TRUE)
	result1 <- glm(x[ind, 2] ~ x[ind, 1], family = binomial(logit))
	coefficients(result1)
}


### Sim

trials <- 300

# sequential execution
sfInit(parallel=FALSE)
stime <- system.time(r <- sfLapply(1:trials, calcPar))[3]
sfStop()

# paralel execution
sfInit(parallel=TRUE, cpus=2)
sfExport("x")
ptime <- system.time(r <- sfLapply(1:trials, calcPar))[3]
sfStop()

stime
ptime

stime/ptime
