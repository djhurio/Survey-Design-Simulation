###### Function: SamplingClusterStr

# ### Ver01
# # Initial version
# 
# SamplingClusterStr1 <- function(frame.1, frame.2, n=30,
#   name.weight=".dw", name.cluster, name.strata) {
# 
#   # Libs
#   require(bigmemory)
#  
#   # Argument type convertion
#   
#   if (!is.big.matrix(frame.1)) frame.1 <- as.data.frame(frame.1)
#   if (!is.big.matrix(frame.2)) frame.2 <- as.data.frame(frame.2)
#   n <- as.vector(as.integer(n))
#   name.weight <- as.character(name.weight)
#   name.cluster <- as.character(name.cluster)
#   name.strata <- as.character(name.strata)
# 
#   # Testing
# 
#   if (name.weight %in% colnames(frame.1))
#     print("WARNING: Weight variable exists, it will be overwrited")
#   if (!name.cluster %in% colnames(frame.1))
#     stop("ERROR: Can not find cluster variable in frame.1")
#   if (!name.cluster %in% colnames(frame.2))
#     stop("ERROR: Can not find cluster variable in frame.2")
#   if (!name.strata %in% colnames(frame.2))
#     stop("ERROR: Can not find strata variable in frame.2")
# 
#   N <- as.vector(table(frame.2[, name.strata]))
#   if (any(n <= 0) | any(n > N)) stop("ERROR: n has to be in 0-N")
# 
#   # Sampling
# 
#   # tmp.a <- c(0, cumsum(N)[-length(N)])
#   tmp.a <- cumsum(N) - N
#   
#   s.2 <- as.data.frame(frame.2[, c(name.strata, name.cluster)])
#   s.2$rand <- runif(nrow(frame.2))
#   s.2 <- s.2[order(s.2$strata, s.2$rand), ]
#   rownames(s.2) <- NULL
#   s.2$nr <- 1:nrow(s.2) - tmp.a[s.2$strata]
#   s.2 <- as.vector(s.2[s.2$nr <= n[s.2$strata], name.cluster])
#   head(s.2)
#   
#   s.1 <- as.data.frame(frame.1[frame.1[, name.cluster] %in% s.2, ])
#   s.1[name.weight] <- N[s.1$strata]/n[s.1$strata]
# 
#   return(s.1)
# }
# 
# ### Ver02
# # Sampling using package "sampling"
# 
# SamplingClusterStr2 <- function(frame.1, frame.2, n=30,
#  name.weight=".dw", name.cluster, name.strata) {
# 
#   # Libs
#   require(bigmemory)
#   require(sampling)
#  
#   # Argument type convertion
#   
#   if (!is.big.matrix(frame.1)) frame.1 <- as.data.frame(frame.1)
#   if (!is.big.matrix(frame.2)) frame.2 <- as.data.frame(frame.2)
#   n <- as.vector(as.integer(n))
#   name.weight <- as.character(name.weight)
#   name.cluster <- as.character(name.cluster)
#   name.strata <- as.character(name.strata)
# 
#   # Testing
# 
#   if (name.weight %in% colnames(frame.1))
#     print("WARNING: Weight variable exists, it will be overwrited")
#   if (!name.cluster %in% colnames(frame.1))
#     stop("ERROR: Can not find cluster variable in frame.1")
#   if (!name.cluster %in% colnames(frame.2))
#     stop("ERROR: Can not find cluster variable in frame.2")
#   if (!name.strata %in% colnames(frame.2))
#     stop("ERROR: Can not find strata variable in frame.2")
# 
#   N <- as.vector(table(frame.2[, name.strata]))
#   if (any(n <= 0) | any(n > N)) stop("ERROR: n has to be in 0-N")
# 
#   # Sampling
# 
#   f.2 <- as.matrix(frame.2[, c(name.cluster, name.strata)])
#   s.2 <- strata(data = f.2, stratanames = name.strata, size = n, method = "srswor")
#   s.2 <- s.2$ID_unit
#   
#   s.1 <- as.data.frame(frame.1[frame.1[, name.cluster] %in% s.2, ])
#   s.1[name.weight] <- N[s.1$strata]/n[s.1$strata]
# 
#   return(s.1)
# }
# 
# ### Ver03
# # Sampling using for-loop
# 
# SamplingClusterStr3 <- function(frame.1, frame.2, n=30,
#  name.weight=".dw", name.cluster, name.strata) {
# 
#   # Libs
#   require(bigmemory)
#  
#   # Argument type convertion
#   
#   if (!is.big.matrix(frame.1)) frame.1 <- as.data.frame(frame.1)
#   if (!is.big.matrix(frame.2)) frame.2 <- as.data.frame(frame.2)
#   n <- as.vector(as.integer(n))
#   name.weight <- as.character(name.weight)
#   name.cluster <- as.character(name.cluster)
#   name.strata <- as.character(name.strata)
# 
#   # Testing
# 
#   if (name.weight %in% colnames(frame.1))
#     print("WARNING: Weight variable exists, it will be overwrited")
#   if (!name.cluster %in% colnames(frame.1))
#     stop("ERROR: Can not find cluster variable in frame.1")
#   if (!name.cluster %in% colnames(frame.2))
#     stop("ERROR: Can not find cluster variable in frame.2")
#   if (!name.strata %in% colnames(frame.2))
#     stop("ERROR: Can not find strata variable in frame.2")
# 
#   N <- as.vector(table(frame.2[, name.strata]))
#   if (any(n <= 0) | any(n > N)) stop("ERROR: n has to be in 0-N")
# 
#   # Sampling
# 
#   strata <- unique(frame.2[, name.strata])
#   s.2 <- NULL
#   for (s in strata)
#     s.2 <- c(s.2, sample(frame.2[frame.2[, name.strata] == s, name.cluster], n[strata == s]))
#   
#   s.1 <- as.data.frame(frame.1[frame.1[, name.cluster] %in% s.2, ])
#   s.1[name.weight] <- N[s.1$strata]/n[s.1$strata]
# 
#   return(s.1)
# }
# 
# ### Ver04
# # Using mapply and modified sample function
# 
# SamplingClusterStr4 <- function(frame.1, frame.2, n=30,
#  name.weight=".dw", name.cluster, name.strata) {
# 
#   # Libs
#   require(bigmemory)
#   require(bigtabulate)
#  
#   # Argument type convertion
#   if (!is.big.matrix(frame.1)) frame.1 <- data.frame(frame.1)
#   if (!is.big.matrix(frame.2)) frame.2 <- data.frame(frame.2)
#   n <- as.vector(as.integer(n))
#   name.weight <- as.character(name.weight)
#   name.cluster <- as.character(name.cluster)
#   name.strata <- as.character(name.strata)
# 
#   # Testing
# 
#   if (name.weight %in% colnames(frame.1))
#     print("WARNING: Weight variable exists in frame.1, it will be overwrited")
#   if (!name.cluster %in% colnames(frame.1))
#     stop("ERROR: Can not find cluster variable in frame.1")
#   if (!name.cluster %in% colnames(frame.2))
#     stop("ERROR: Can not find cluster variable in frame.2")
#   if (!name.strata %in% colnames(frame.2))
#     stop("ERROR: Can not find strata variable in frame.2")
#   if (any(frame.2[, name.strata] != sort(frame.2[, name.strata])))
#     stop("ERROR: frame.2 is not sorted by strata")
# 
#   # N <- as.vector(table(frame.2[, name.strata]))
#   # N <- as.vector(tapply(rep(1, nrow(frame.2)), frame.2[, name.strata], length))
#   # N <- as.vector(aggregate(rep(1, nrow(frame.2)), list(frame.2[, name.strata]), length))
#   N <- as.vector(bigtable(frame.2, name.strata))
# 
#   # system.time(as.vector(table(frame.2[, name.strata])))
#   # system.time(as.vector(tapply(rep(1, nrow(frame.2)), frame.2[, name.strata], length)))
#   # system.time(as.vector(aggregate(rep(1, nrow(frame.2)), list(frame.2[, name.strata]), length)))
#   # system.time(as.vector(bigtable(frame.2, name.strata)))
# 
#   if (any(n <= 0) | any(n > N)) stop("ERROR: n has to be in 0-N")
# 
#   # Sampling
# 
#   # Modified sample function
#   sample.mod <- function(x, size, add = 0) sample(x, size, FALSE, NULL) + add
# 
#   a <- cumsum(N) - N
#   s.2.index <- unlist(mapply(sample.mod, N, n, a, SIMPLIFY = F))
#   s.2 <- frame.2[s.2.index, name.cluster]
#   s.1 <- data.frame(frame.1[frame.1[, name.cluster] %in% s.2, ])
# 
#   # Weight
#   w <- N / n
#   # strata <- unique(frame.2[, name.strata])
#   s.1[name.weight] <- w[s.1$strata]
# 
#   return(s.1)
# }
# 
# 
# ### Ver05
# # Strata can be defined with any values
# # Sample size can be 0 in some strata
# 
# SamplingClusterStr5 <- function(frame.1, frame.2, n,
#  name.weight=".dw", name.cluster, name.strata) {
# 
#   ### Libs
#   require(bigmemory)
#   require(bigtabulate)
#  
#   ### Argument type convertion
#   if (!is.big.matrix(frame.1)) frame.1 <- data.frame(frame.1)
#   if (!is.big.matrix(frame.2)) frame.2 <- data.frame(frame.2)
#   n <- as.vector(as.integer(n))
#   name.weight <- as.character(name.weight)
#   name.cluster <- as.character(name.cluster)
#   name.strata <- as.character(name.strata)
# 
#   ### Testing
#   if (name.weight %in% colnames(frame.1))
#     print("WARNING: Weight variable exists in frame.1, it will be overwrited")
#   if (!name.cluster %in% colnames(frame.1))
#     stop("ERROR: Can not find cluster variable in frame.1")
#   if (!name.cluster %in% colnames(frame.2))
#     stop("ERROR: Can not find cluster variable in frame.2")
#   if (!name.strata %in% colnames(frame.2))
#     stop("ERROR: Can not find strata variable in frame.2")
#   if (any(frame.2[, name.strata] != sort(frame.2[, name.strata])))
#     stop("ERROR: frame.2 is not sorted by strata")
# 
#   N <- as.vector(bigtable(frame.2, name.strata))
#   a <- cumsum(N) - N
# 
#   if (any(n < 0) | any(n > N)) stop("ERROR: n has to be in 0-N")
#   if (sum(n) == 0) stop("Total sample size is 0")
# 
#   N <- N[n>0]
#   a <- a[n>0]
#   n <- n[n>0]
#   
#   ### Sampling
#   # Modified sample function
#   sample.mod <- function(x, size, add = 0) sample(x, size, FALSE, NULL) + add
# 
#   s.2.index <- unlist(mapply(sample.mod, N, n, a, SIMPLIFY = F))
#   s.2 <- frame.2[s.2.index, name.cluster]
#   s.1 <- data.frame(frame.1[frame.1[, name.cluster] %in% s.2, ])
# 
#   ### Weighting
#   w <- N / n
#   s.1[name.weight] <- w[match(s.1[, name.strata], unique(s.1[, name.strata]))]
# 
#   return(s.1)
# }


### Ver06
# Strata can be defined with any values
# Sample size can be 0 in some strata
# Sample size is reduced to population size if n>N

SamplingClusterStr <- function(frame.1,
                               frame.2,
                               n,
                               name.weight=".dw",
                               name.cluster,
                               name.strata) {

  ### Libs
  require(bigmemory)
  require(bigtabulate)
 
  ### Argument type convertion
  if (!is.big.matrix(frame.1)) frame.1 <- data.frame(frame.1)
  if (!is.big.matrix(frame.2)) frame.2 <- data.frame(frame.2)
  n <- as.vector(as.integer(n))
  name.weight <- as.character(name.weight)[1]
  name.cluster <- as.character(name.cluster)[1]
  name.strata <- as.character(name.strata)[1]

  ### Testing
  if (name.weight %in% colnames(frame.1))
    print("WARNING: Weight variable exists in frame.1, it will be overwrited")
  if (!name.cluster %in% colnames(frame.1))
    stop("ERROR: Can not find cluster variable in frame.1")
  if (!name.cluster %in% colnames(frame.2))
    stop("ERROR: Can not find cluster variable in frame.2")
  if (!name.strata %in% colnames(frame.2))
    stop("ERROR: Can not find strata variable in frame.2")
  if (any(frame.2[, name.strata] != sort(frame.2[, name.strata])))
    stop("ERROR: frame.2 is not sorted by strata")

  N <- as.vector(bigtable(frame.2, name.strata))
  a <- cumsum(N) - N

  #if (any(n < 0) | any(n > N)) stop("ERROR: n has to be in 0-N")
  if (any(n < 0)) stop("ERROR: n has to be greater then 0")
  
  # Reduction of n, if n>N
  n <- ifelse(n>N, N, n)

  if (sum(n) == 0) stop("Total sample size is 0")

  N <- N[n>0]
  a <- a[n>0]
  n <- n[n>0]
  
  ### Sampling
  # Modified sample function
  # Defined by the script "/Procedures/sample.mod.R"
  # sample.mod <- function(x, size, add = 0) sample(x, size) + add

  s.2.index <- unlist(mapply(sample.mod, N, n, a, SIMPLIFY = F))
  s.2 <- frame.2[s.2.index, name.cluster]
  s.1 <- data.frame(frame.1[frame.1[, name.cluster] %in% s.2, ])

  ### Weighting
  w <- N / n
  s.1[name.weight] <- w[match(s.1[, name.strata],
                              sort(unique(s.1[, name.strata])))]

  return(s.1)
}




################
### Dev area ###
################

# # libs
# library(bigmemory)
# library(bigtabulate)
# 
# library(sampling)
# library(rbenchmark)
# 
# 
# # Workdir
# # dir <- "/home/djhurio/temp"
# # dir <- "C:/DATA/LU/Results"
# # dir.data <- "C:/Users/Martins Liberts/Documents/DATA/LU/Work"
# dir.data <- "~/DATA/LU/Work"
# 
# 
# # DATA
# setwd(dir.data)
# frame.p <- attach.big.matrix("frame.p.desc")
# frame.h <- attach.big.matrix("frame.h.desc")
# 
# head(frame.p)
# head(frame.h)
# 
# 
# 
# 
# # Temp parameters
# 
# frame.1 <- frame.p
# frame.2 <- frame.h
# n <- c(10, 10, 0, 10)
# # n <- c(300000, 10, 0, 10)
# name.weight <- ".dw"
# name.cluster <- "H_ID"
# name.strata <- "strata"
# 
# 
# ###############
# ### Testing ###
# ###############
# 
# # Sample size
# 
# m.h <- rep(200, 4)
# 
# s <- SamplingClusterStr1(frame.p, frame.h, m.h, ".dw", "H_ID", "strata")
# head(s)
# sum(s$.dw)
# 
# s <- SamplingClusterStr2(frame.p, frame.h, m.h, ".dw", "H_ID", "strata")
# head(s)
# sum(s$.dw)
# 
# s <- SamplingClusterStr3(frame.p, frame.h, m.h, ".dw", "H_ID", "strata")
# head(s)
# sum(s$.dw)
# 
# s <- SamplingClusterStr4(frame.p, frame.h, m.h, ".dw", "H_ID", "strata")
# head(s)
# sum(s$.dw)
# 
# s <- SamplingClusterStr5(frame.p, frame.h, m.h, ".dw", "H_ID", "strata")
# head(s)
# sum(s$.dw)
# 
# s <- SamplingClusterStr6(frame.p, frame.h, m.h, ".dw", "H_ID", "strata")
# head(s)
# sum(s$.dw)
# 
# benchmark(
#   SamplingClusterStr1(frame.p, frame.h, m.h, ".dw", "H_ID", "strata"),
#   SamplingClusterStr2(frame.p, frame.h, m.h, ".dw", "H_ID", "strata"),
#   SamplingClusterStr3(frame.p, frame.h, m.h, ".dw", "H_ID", "strata"),
#   SamplingClusterStr4(frame.p, frame.h, m.h, ".dw", "H_ID", "strata"),
#   SamplingClusterStr5(frame.p, frame.h, m.h, ".dw", "H_ID", "strata"),
#   SamplingClusterStr6(frame.p, frame.h, m.h, ".dw", "H_ID", "strata"),
#   columns=c("test", "replications", "elapsed", "relative"),
#   order="relative",
#   replications=5
# )
# 
# 
# s <- SamplingClusterStr6(frame.p, frame.h, c(100, 0, 0, 0), ".dw", "H_ID", "strata")
# head(s)
# sum(s$.dw)
# 
# s <- SamplingClusterStr6(frame.p, frame.h, c(0, 100, 0, 0), ".dw", "H_ID", "strata")
# head(s)
# sum(s$.dw)
# 
# s <- SamplingClusterStr6(frame.p, frame.h, c(0, 0, 100, 0), ".dw", "H_ID", "strata")
# head(s)
# sum(s$.dw)
# 
# s <- SamplingClusterStr6(frame.p, frame.h, c(0, 0, 0, 100), ".dw", "H_ID", "strata")
# head(s)
# sum(s$.dw)
# 
# s <- SamplingClusterStr6(frame.p, frame.h, c(1e6, 0, 0, 0), ".dw", "H_ID", "strata")
# head(s)
# sum(s$.dw)
