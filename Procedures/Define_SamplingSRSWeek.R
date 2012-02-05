###### Function: SamplingSRSWeek

### Ver01
# Initial version


SamplingSRSWeek <- function(frame.1, n = 30, name.weight = ".dw", name.week = ".week", weeks = 1) {
  
  # Libs
  require(bigmemory)

  # Argument type convertion
  
  if (!is.big.matrix(frame.1)) frame.1 <- as.data.frame(frame.1)
  n <- as.integer(n)[1]
  name.weight <- as.character(name.weight)[1]
  name.week <- as.character(name.week)[1]
  weeks <- as.integer(weeks)[1]

  # Testing

  N <- nrow(frame.1)
  if (n < 1) n <- 1
  if (n > N) n <- N

  if (weeks < 1) weeks <- 1

  if (name.weight %in% colnames(frame.1))
    print("WARNING: Weight variable exists, it will be overwrited")
  if (name.week %in% colnames(frame.1))
    print("WARNING: Week variable exists, it will be overwrited")

  # Sampling

  s.1 <- as.data.frame(frame.1[sample(1:N, n), ])
  s.1[name.weight] <- N/n
  s.1[name.week] <- (1:n-1) %% weeks + 1
  
  return(s.1)
}


################
### Dev area ###
################

# # libs
# library(bigmemory)
# 
# 
# # Workdir
# # dir <- "/home/djhurio/temp"
# # dir <- "C:/DATA/LU/Results"
# # dir <- "C:/Users/Martins Liberts/Documents/DATA/LU/Work"
# dir <- "~/DATA/LU/Work"
# 
# 
# # DATA
# setwd(dir)
# pop <- attach.big.matrix("frame.p.desc")
# head(pop)
# 
# 
# # Temp parameters
# 
# # frame.1 <- pop
# # n <- 10
# # name.weight <- ".dw"
# # name.week <- "week"
# # weeks <- 3
# # 
# 
# 
# nrow(pop)
# 
# s <- SamplingSRSWeek(frame = pop, n = 100, name.weight = ".dw")
# head(s)
# sum(s$.dw)
# 
# s <- SamplingSRSWeek(frame = pop, n = 100, name.weight = ".dw", weeks = 13)
# head(s)
# sum(s$.dw)
# table(s$.week)
