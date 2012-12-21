###### Function: SamplingSRSWeek

### Ver01
# Initial version

### Ver 02

### Ver03
# Fix: no-sorting
# The sample size is converted to the closest multiple of the number of weeks

SamplingSRSWeek <- function(frame.1,
                            n = 30,
                            name.weight = ".dw",
                            name.week = ".week",
                            weeks = 1) {
  
  # Libs
  require(bigmemory)
  
  # Argument type convertion
  if (!is.big.matrix(frame.1)) frame.1 <- as.data.frame(frame.1)
  n <- as.integer(n)[1]
  name.weight <- as.character(name.weight)[1]
  name.week <- as.character(name.week)[1]
  weeks <- as.integer(weeks)[1]
  
  # Testing
  if (weeks < 1) weeks <- 1
  
  N <- nrow(frame.1)
  n <- round(n / weeks) * weeks

  if (n < weeks) n <- weeks
  if (n > N) n <- N %/% weeks * weeks
  
  if (n %% weeks > 0) stop("n is not a multiple of weeks")
  
  if (name.weight %in% colnames(frame.1))
    print("WARNING: Weight variable exists, it will be overwritten")
  if (name.week %in% colnames(frame.1))
    print("WARNING: Week variable exists, it will be overwritten")
  
  # Sampling
  s.1 <- data.frame(frame.1[sample(N, n), ])
  s.1[name.weight] <- N/n
  s.1[name.week] <- rep(1:weeks, each = n / weeks)
  return(s.1)
}
