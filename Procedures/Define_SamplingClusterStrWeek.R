###### Function: SamplingClusterStrWeekWeek
### Stratified SRS cluster sampling with allocation by weeks

### Ver01
# Initial version
 
SamplingClusterStrWeek <- function(frame.1,
                                   frame.2,
                                   n,
                                   name.weight=".dw",
                                   name.week = ".week",
                                   name.cluster,
                                   name.strata,
                                   weeks = 1) {

  ### Libs
  require(bigmemory)
  require(bigtabulate)
 
  ### Argument type convertion
  if (!is.big.matrix(frame.1)) frame.1 <- data.frame(frame.1)
  if (!is.big.matrix(frame.2)) frame.2 <- data.frame(frame.2)
  n <- as.vector(as.integer(n))
  name.weight <- as.character(name.weight)[1]
  name.week <- as.character(name.week)[1]
  name.cluster <- as.character(name.cluster)[1]
  name.strata <- as.character(name.strata)[1]
  weeks <- as.integer(weeks)[1]
  
  ### Testing
  if (name.weight %in% colnames(frame.1))
    print("WARNING: Weight variable exists in frame.1, it will be overwrited")
  if (name.week %in% colnames(frame.1))
    print("WARNING: Week variable exists, it will be overwritten")
  if (!name.cluster %in% colnames(frame.1))
    stop("ERROR: Can not find cluster variable in frame.1")
  if (!name.cluster %in% colnames(frame.2))
    stop("ERROR: Can not find cluster variable in frame.2")
  if (!name.strata %in% colnames(frame.2))
    stop("ERROR: Can not find strata variable in frame.2")
  if (any(frame.2[, name.strata] != sort(frame.2[, name.strata])))
    stop("ERROR: frame.2 is not sorted by strata")

  list.str <- sort(unique(frame.2[, name.strata]))
  if (length(list.str) != length(n))
    stop("ERROR: wrong definition of n or strata variable")
  
  if (weeks < 1L) weeks <- 1L
  N <- as.vector(bigtable(frame.2, name.strata))
  a <- cumsum(N) - N
  n <- round(n / weeks) * weeks

  n[n < 0L] <- 0L
  n[n > N] <- N[n > N] %/% weeks * weeks
  
  if (any(n %% weeks > 0L)) stop("ERROR: n is not a multiple of weeks")
  if (sum(n) == 0L) return(NULL)

  N <- N[n>0]
  a <- a[n>0]
  n <- n[n>0]
  w <- N / n
  
  # Sampling
  s.2.index <- unlist(mapply(sample.mod, N, n, a, SIMPLIFY = F))
  s.2 <- data.frame(frame.2[s.2.index, c(name.strata, name.cluster)])

  # Weeks
  s.2[name.week] <- rep(1:weeks, times = sum(n) / weeks)
  
  # Weights
  s.2[name.weight] <- w[match(s.2[, name.strata], sort(unique(s.2[, name.strata])))]
  
  s.1 <- data.frame(frame.1[frame.1[, name.cluster] %in% s.2[, name.cluster], ])
  s.1[name.week] <- NULL
  s.1[name.weight] <- NULL
  s.1 <- merge(s.1, s.2)

  return(s.1)
}
