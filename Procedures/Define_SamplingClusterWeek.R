###### Function: SamplingClusterWeek

### Ver01
# Initial version

### Ver02

### Ver03
# Fix: no-sorting
# The sample size is converted to the closest multiple of the number of weeks

SamplingClusterWeek <- function(frame.1,
                                frame.2,
                                n=30,
                                name.weight=".dw",
                                name.cluster,
                                name.week = ".week",
                                weeks = 1) {
  
  # Libs
  require(bigmemory)
  
  # Argument type convertion
  if (!is.big.matrix(frame.1)) frame.1 <- as.data.frame(frame.1)
  if (!is.big.matrix(frame.2)) frame.2 <- as.data.frame(frame.2)
  n <- as.integer(n)[1]
  name.weight <- as.character(name.weight)[1]
  name.cluster <- as.character(name.cluster)[1]
  name.week <- as.character(name.week[1])
  weeks <- as.integer(weeks[1])
  
  # Testing
  if (weeks < 1) weeks <- 1
  
  N <- nrow(frame.2)
  n <- round(n / weeks) * weeks
  
  if (n < weeks) n <- weeks
  if (n > N) n <- N %/% weeks * weeks
  
  if (n %% weeks > 0) stop("n is not a multiple of weeks")
  
  if (name.weight %in% colnames(frame.1))
    print("WARNING: Weight variable exists, it will be overwritten")
  if (name.week %in% colnames(frame.1))
    print("WARNING: Week variable exists, it will be overwritten")
  
  if (!name.cluster %in% colnames(frame.1))
    stop("ERROR: Can not find cluster variable in frame.1")
  if (!name.cluster %in% colnames(frame.2))
    stop("ERROR: Can not find cluster variable in frame.2")
  
  # Sampling
  s.2 <- as.vector(frame.2[, name.cluster][sample(N, n)])
  
  tmp <- data.frame(s.2, rep(1:weeks, each = n / weeks))
  names(tmp) <- c(name.cluster, name.week)
  
  s.1 <- data.frame(frame.1[frame.1[, name.cluster] %in% s.2, ])
  
  s.1[name.weight] <- N/n
  
  s.1[name.week] <- NULL
  s.1 <- merge(s.1, tmp, by = name.cluster, sort = F)
  
  s.1 <- s.1[c(colnames(frame.1), name.weight, name.week)]
  
  return(s.1)
}
