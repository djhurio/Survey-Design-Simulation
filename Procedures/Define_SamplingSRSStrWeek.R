###### Function: SamplingSRSStrWeek
### Stratified SRS sampling with allocation by weeks

### Ver01
# Initial version

SamplingSRSStrWeek <- function(frame.1,
                               n,
                               name.weight = ".dw",
                               name.week = ".week",
                               name.strata,
                               weeks = 1) {
  
  # Libs
  require(bigmemory)
  require(bigtabulate)
  
  # Argument type convertion
  if (!is.big.matrix(frame.1)) frame.1 <- as.data.frame(frame.1)
  n <- as.vector(as.integer(n))
  name.weight <- as.character(name.weight)[1]
  name.week <- as.character(name.week)[1]
  name.strata <- as.character(name.strata)[1]
  weeks <- as.integer(weeks)[1]
  
  # Testing
  if (name.weight %in% colnames(frame.1))
    print("WARNING: Weight variable exists, it will be overwritten")
  if (name.week %in% colnames(frame.1))
    print("WARNING: Week variable exists, it will be overwritten")
  if (!name.strata %in% colnames(frame.1))
    stop("ERROR: Can not find strata variable in frame.1")
  if (any(frame.1[, name.strata] != sort(frame.1[, name.strata])))
    stop("ERROR: frame.1 is not sorted by strata")
  
  if (weeks < 1L) weeks <- 1L
  
  N <- as.vector(bigtable(frame.1, name.strata))
  a <- cumsum(N) - N
  n <- round(n / weeks) * weeks

  n[n < 0L] <- 0L
  n[n > N] <- N[n > N] %/% weeks * weeks
  
  if (any(n %% weeks > 0L)) stop("ERROR: n is not a multiple of weeks")
  if (sum(n) == 0L) return(NULL)
  
  N <- N[n>0]
  a <- a[n>0]
  n <- n[n>0]
  
  # Sampling
  s.1.index <- unlist(mapply(sample.mod, N, n, a, SIMPLIFY = F))
  s.1 <- data.frame(frame.1[s.1.index, ])

  # Weighting
  w <- N / n
  s.1[name.weight] <- w[match(s.1[, name.strata],
                              sort(unique(s.1[, name.strata])))]

  # Weeks
  s.1[name.week] <- rep(1:weeks, times = sum(n) / weeks)

  return(s.1)
}
