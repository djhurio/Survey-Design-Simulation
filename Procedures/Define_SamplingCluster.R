# ###### Function: SamplingCluster

### Ver01
# Initial version

### Ver02
# Two frames

SamplingCluster <- function(frame.1, frame.2, n=30, name.weight=".dw",
                            name.cluster) {

  # Libs
  require(bigmemory)

  # Argument type convertion
  if (!is.big.matrix(frame.1)) frame.1 <- as.data.frame(frame.1)
  if (!is.big.matrix(frame.2)) frame.2 <- as.data.frame(frame.2)
  n <- as.integer(n)
  name.weight <- as.character(name.weight)
  name.cluster <- as.character(name.cluster)

  # Testing
  N <- nrow(frame.2)

  if (n <= 0 | n > N) stop("ERROR: n has to be in 0-N")
  if (name.weight %in% colnames(frame.1))
    print("WARNING: Weight variable exists, it will be overwrited")
  if (!name.cluster %in% colnames(frame.1))
    stop("ERROR: Can not find cluster variable in frame.1")
  if (!name.cluster %in% colnames(frame.2))
    stop("ERROR: Can not find cluster variable in frame.2")

  # Sampling
  s.2 <- as.vector(frame.2[, name.cluster][sample(N, n)])

  s.1 <- as.data.frame(frame.1[frame.1[,name.cluster] %in% s.2, ])
  s.1[name.weight] <- N/n

  return(s.1)
}
