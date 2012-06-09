########################################
### Population data changing in time ###
########################################


### Libs

require(reshape2)
require(ggplot2)
require(foreach)
require(bigmemory)
require(bigtabulate)


### Reset ###
setwd(projwd)
rm(list = ls())
gc()
source(".Rprofile")



### Dir
# data.tmp <- "~/temp/pop"



### Data

setwd(projwd)
load("Data/TM.Rdata")
TM


###

setwd(dir.data.out)

# load("frame.p.Rdata")
frame.p <- attach.big.matrix("frame.p.desc")

head(frame.p)
# test.df(frame.p)

nrow(frame.p)

bigtable(frame.p, "eka")
prop.table(bigtable(frame.p, "eka"))

eka.distr <- as.numeric(prop.table(bigtable(frame.p, "eka")))
eka.distr


eka <- frame.p[, "eka"]
class(eka)
length(eka)

# rm(frame.p)


###

TM

TM.cum <- cbind(TM[, c(1:3)], rowSums(TM[, c(3:4)]), rowSums(TM[, c(3:5)]))
colnames(TM.cum) <- colnames(TM)
TM.cum

TM.cum.list <- split(TM.cum[, c(3:5)], TM.cum$per)
TM.cum.list[[1]]
TM.cum.list[[2]]

# TM.list <- split(TM[, c(3:5)], TM$per)
# TM.list[[1]]
# class(TM.list[[1]])
# TM.list[[2]]
# length(TM.list)


# state <- c(1)
# tm.cum <- TM1_cum

state.change <- function(state, tm.cum) {
  n <- length(state)
  #if (n == 1) return(ncol(tm.cum) - sum(runif(1) < tm.cum[state, ]) + 1)
  #else return(ncol(tm.cum) - rowSums(runif(n) < tm.cum[state, ]) + 1)
  if (n == 1) return(as.integer(ncol(tm.cum) - sum(runif(1) < tm.cum[state, ]) + 1))
  else return(as.integer(ncol(tm.cum) - rowSums(runif(n) < tm.cum[state, ]) + 1))
}

# eka1 <- state.change(eka, TM.cum.list[[1]])
# class(eka1)
# 
# prop.table(table(eka))
# prop.table(table(eka1))



# !3 %in% 1:5

# MC1 <- function(state, tm) {
#   state <- as.numeric(state)[1]
#   tm <- as.matrix(tm)
#   
#   if (nrow(tm) != ncol(tm)) stop("tm must be square matrix")
# 
#   n <- nrow(tm)
#   if (!state %in% 1:n) stop("Wrong state")
#   
#   return(sample(1:n,1,prob=tm[state,]))
# }
# 
# MC1(1, TM.list[[1]])
# MC1(2, TM.list[[1]])
# MC1(3, TM.list[[1]])
# 
# MC1(1, matrix(1/3, 3, 3))
# MC1(1, rbind(c(0, 1, 0), c(0, 1, 0), c(0, 1, 0)))
# MC1(3, rbind(c(0, 1, 0), c(0, 1, 0), c(0, 1, 0)))
# 
# # !all(c(rep(1, 5), rep(2, 5)) %in% 1:3)
# 
# 
# MC <- function(state, tm.list, time) {
#   state <- as.numeric(state)[1]
#   tm.list <- as.list(tm.list)
#   time <- as.vector(time)
# 
#   m <- length(tm.list)
#   if (!all(time %in% 1:m)) stop("Wrong time")
#   
#   n <- length(time)
#   
#   res <- vector("numeric", n + 1)
#   res[1] <- state
#   
#   st <- state
#   for (i in 1:n) {
#     st <- MC1(st, tm.list[[time[i]]])
#     res[i+1] <- st
#   }
#   
#   return(res)
# }
# 
# 
# MC(1, list(matrix(1/3, 3, 3)), rep(1, each=15))
# MC(1, list(matrix(1/3, 3, 3), diag(3)), rep(1:2, each=15))
# 
# 
# 
# MC(1, TM.list, rep(1:4, each = 13))
# MC(2, TM.list, rep(1:4, each = 13))
# MC(3, TM.list, rep(1:4, each = 13))



# MC.v <- function(state, tm.list, time) {
# 
#   require(foreach)
#   require(doMC)
#   registerDoMC()
# 
#   t1 <- Sys.time()
#   res <- foreach(st = state, .combine = rbind) %dopar% MC(st, tm.list, time)
#   t2 <- Sys.time()
#   time.run <- as.numeric(t2 - t1, units="secs")
#   cat(paste("Total time:", time.run, "\n", sep=" "))
#   
#   colnames(res) <- paste("t", 0:length(time), sep = "")
#   rownames(res) <- NULL
#   return(res)
# }
# 
# MC.v(1:3, TM.list, rep(1:4, each = 13))
# 
# time <- rep(1:4, times = 3, each = 13)
# length(time)
# length(time) / 13
# 
# k <- 1e3
# 
# eka.sample <- eka[1:k]
# pop <- MC.v(eka.sample, TM.list, time)
# class(pop)
# dim(pop)
# 
# 50 / k * length(eka) / 60 / 60
# 
# 
# 
# state0 <- eka
# tm.list <- TM.cum.list
# time <- rep(1, 5)

###

# gen.pop.1 <- function(state0, tm.list, time) {
#   n <- length(state0)
#   m <- length(time)
#   
#   pop <- data.frame(matrix(0, n, m+1))
# 
#   pop[, 1] <- state0
#   for (i in 1:m) pop[, i+1] <- state.change(pop[, i], tm.list[[time[i]]])
#   names(pop) <- paste("t", 0:m, sep = "")
#   return(pop)
# }

###

# gen.pop.1 <- function(state0, tm.list, time, file.name = "file") {
#   
#   n <- length(state0)
#   m <- length(time)
#   
#   pop <- filebacked.big.matrix(nrow = n,
#      ncol = m+1,
#      type = "integer",
#      dimnames = list(NULL, paste("t", 0:m, sep = "")),
#      backingfile = paste(file.name, ".bin", sep = ""),
#      descriptorfile = paste(file.name, ".desc", sep = ""))
#   
#   pop[, 1] <- as.integer(state0)
# 
#   for (i in 1:m) {
#     cat(paste("i:", i, "time:", time[i], "\n"))
#     pop[, i+1] <- state.change(pop[, i], tm.list[[time[i]]])
#   }
# 
#   return(NULL)
# }


gen.pop <- function(state0, tm.list, time,
                    file.name = "file", extra.col = NULL) {
  
  n <- length(state0)
  
  m1 <- length(time)
  m2 <- length(extra.col)
  
  pop <- filebacked.big.matrix(nrow = n,
    ncol = m2 + m1 + 1,
    type = "integer",
    dimnames = list(NULL, c(extra.col, paste("eka", 0:m1, sep = ""))),
    backingfile = paste(file.name, ".bin", sep = ""),
    descriptorfile = paste(file.name, ".desc", sep = ""))
  
  pop[, m2+1] <- as.integer(state0)
  
  for (i in 1:m1) {
    cat(paste("i:", i, "time:", time[i], "\n"))
    pop[, m2+1+i] <- state.change(pop[, m2+i], tm.list[[time[i]]])
  }
  
  return(NULL)
}



################
### Temp dir ###
################

# setwd(data.tmp)

###

# popg <- gen.pop.1(eka, TM.cum.list, rep(1, 3))
# class(popg)
# head(popg)



time <- rep(1:4, times = 1, each = 13)
time
length(time)

# T <- 6*52
# T

set.seed(20120502)
gen.pop(eka, TM.cum.list, time,
        file.name = "pop.eka", extra.col = c("casenum", "H_ID", "P_ID"))

# gen.pop(eka, TM.cum.list, rep(1, T), file.name = "file1")

popg <- attach.big.matrix("pop.eka.desc")
class(popg)
head(popg)


head(frame.p)

popg[, c("casenum", "H_ID", "P_ID")] <- as.integer(frame.p[, c("casenum","H_ID", "P_ID")])
head(popg)
