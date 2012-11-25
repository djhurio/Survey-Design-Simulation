###### Function: SamplingSRSWeek

### Ver01
# Initial version

# SamplingSRSWeek <- function(frame.1, n = 30, name.weight = ".dw", name.week = ".week", weeks = 1) {
#   
#   # Libs
#   require(bigmemory)
# 
#   # Argument type convertion
#   
#   if (!is.big.matrix(frame.1)) frame.1 <- as.data.frame(frame.1)
#   n <- as.integer(n)[1]
#   name.weight <- as.character(name.weight)[1]
#   name.week <- as.character(name.week)[1]
#   weeks <- as.integer(weeks)[1]
# 
#   # Testing
# 
#   N <- nrow(frame.1)
#   if (n < 1) n <- 1
#   if (n > N) n <- N
# 
#   if (weeks < 1) weeks <- 1
# 
#   if (name.weight %in% colnames(frame.1))
#     print("WARNING: Weight variable exists, it will be overwrited")
#   if (name.week %in% colnames(frame.1))
#     print("WARNING: Week variable exists, it will be overwrited")
# 
#   # Sampling
#   
#   s <- sort(sample(1:N, n))
# 
#   s.1 <- data.frame(frame.1[s, ])
#   s.1[name.weight] <- N/n
#   s.1[name.week] <- (1:n-1) %% weeks + 1
#   
#   return(s.1)
# }


### Ver 02

# SamplingSRSWeek <- function(frame.1, n = 30, name.weight = ".dw", name.week = ".week", weeks = 1) {
#   
#   # Libs
#   require(bigmemory)
#   
#   # Argument type convertion
#   
#   if (!is.big.matrix(frame.1)) frame.1 <- as.data.frame(frame.1)
#   n <- as.integer(n)[1]
#   name.weight <- as.character(name.weight)[1]
#   name.week <- as.character(name.week)[1]
#   weeks <- as.integer(weeks)[1]
#   
#   # Testing
#   
#   N <- nrow(frame.1)
#   if (n < 1) n <- 1
#   if (n > N) n <- N
#   
#   if (weeks < 1) weeks <- 1
#   
#   if (name.weight %in% colnames(frame.1))
#     print("WARNING: Weight variable exists, it will be overwrited")
#   if (name.week %in% colnames(frame.1))
#     print("WARNING: Week variable exists, it will be overwrited")
#   
#   # Sampling
#   
#   s <- sort(sample(1:N, n))
#   
#   s.1 <- data.frame(.pop.id = s, frame.1[s, ])
#   s.1[name.weight] <- N/n
#   s.1[name.week] <- (1:n-1) %% weeks + 1
#   
#   return(s.1)
# }


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
  
  #s <- sort(sample(1:N, n))
  #s <- sample(N, n)
  
  s.1 <- data.frame(frame.1[sample(N, n), ])
  s.1[name.weight] <- N/n
  # s.1[name.week] <- (1:n-1) %% weeks + 1
  # Randomise the order of week numbers
  # s.1[name.week] <- sample(rep(1:weeks, n / weeks))
  s.1[name.week] <- rep(1:weeks, each = n / weeks)
  return(s.1)
}

# rep(1:5, 3)
# rep(1:5, each = 3)
# rep(1:5, times = 3)

################
### Dev area ###
################

# # libs
# library(bigmemory)
# 
# 
# # DATA
# setwd(dir.data)
# pop <- attach.big.matrix("frame.p.desc")
# head(pop)
# 
# 
# # Test
# 
# nrow(pop)
# 
# s <- SamplingSRSWeek(frame = pop, n = 100, name.weight = ".dw")
# head(s)
# nrow(s)
# sum(s$.dw)
# 
# s <- SamplingSRSWeek(frame = pop, n = 100, name.weight = ".dw", weeks = 13)
# head(s)
# tail(s)
# nrow(s)
# sum(s$.dw)
# table(s$.week)
# 
# s <- SamplingSRSWeek(frame = pop, n = 1, name.weight = ".dw", weeks = 13)
# head(s)
# tail(s)
# nrow(s)
# sum(s$.dw)
# table(s$.week)
# 
# s <- SamplingSRSWeek(frame = pop, n = 14, name.weight = ".dw", weeks = 13)
# head(s)
# tail(s)
# nrow(s)
# sum(s$.dw)
# table(s$.week)
