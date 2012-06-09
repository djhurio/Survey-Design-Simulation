############# Testing snowfall

library(snowfall)

proc <- function(x) {
	ind <- sample(100, 100, replace = TRUE)
	mean(ind)
#	cat(paste("Iter: ", x, "\n", sep=""))
}

proc(1:10)

N <- 1e3

### CPU = 1

sfInit(parallel=FALSE)
t1 <- Sys.time()
result <- sfClusterApplyLB(1:N, proc)
t2 <- Sys.time()
sfStop()

time_CPU1 <- t2-t1


### CPU = 2

sfInit(parallel=TRUE)
t1 <- Sys.time()
result <- sfClusterApplyLB(1:N, proc)
t2 <- Sys.time()
sfStop()

time_CPU2 <- t2-t1

time_CPU1
time_CPU2

as.numeric(time_CPU1)/as.numeric(time_CPU2)





