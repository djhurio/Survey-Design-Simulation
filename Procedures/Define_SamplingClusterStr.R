###### Function: SamplingClusterStr

### Ver01
# Initial version

### Ver02
# Sampling using package "sampling"

### Ver03
# Sampling using for-loop

### Ver04
# Using mapply and modified sample function

### Ver05
# Strata can be defined with any values
# Sample size can be 0 in some strata

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

  if (any(n < 0)) stop("ERROR: n has to be greater then 0")
  
  # Reduction of n, if n>N
  n <- ifelse(n>N, N, n)

  if (sum(n) == 0) stop("Total sample size is 0")

  N <- N[n>0]
  a <- a[n>0]
  n <- n[n>0]
  
  s.2.index <- unlist(mapply(sample.mod, N, n, a, SIMPLIFY = F))
  s.2 <- frame.2[s.2.index, name.cluster]
  s.1 <- data.frame(frame.1[frame.1[, name.cluster] %in% s.2, ])

  ### Weighting
  w <- N / n
  s.1[name.weight] <- w[match(s.1[, name.strata],
                              sort(unique(s.1[, name.strata])))]

  return(s.1)
}
