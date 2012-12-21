###### Function: SamplingSRS

### Ver01
# Initial version

SamplingSRS <- function(frame.1, n = 30, name.weight = ".dw") {
  
  # Libs
  require(bigmemory)

  # Argument class conversation
  if (!is.big.matrix(frame.1)) frame.1 <- as.data.frame(frame.1)
  n <- as.integer(n[1])
  name.weight <- as.character(name.weight[1])

  # Testing
  N <- nrow(frame.1)
  if (n<=0 | n>N) stop("n has to be in 0-N")
  if (name.weight %in% colnames(frame.1))
    print("WARNING: Weight variable exists, it will be overwritten")

  # Sampling
  s.1 <- as.data.frame(frame.1[sample(N, n), ])
  s.1[name.weight] <- N/n

  return(s.1)
}
