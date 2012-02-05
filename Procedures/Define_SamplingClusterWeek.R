###### Function: SamplingClusterWeek

### Ver01
# Initial version

SamplingClusterWeek <- function(frame.1, frame.2, n=30, name.weight=".dw", name.cluster, name.week = ".week", weeks = 1) {

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

  N <- nrow(frame.2)
  if (n < 1) n <- 1
  if (n > N) n <- N
  
  if (weeks < 1) weeks <- 1

  if (name.weight %in% colnames(frame.1))
    print("WARNING: Weight variable exists, it will be overwrited")
  if (name.week %in% colnames(frame.1))
    print("WARNING: Week variable exists, it will be overwrited")

  if (!name.cluster %in% colnames(frame.1))
    stop("ERROR: Can not find cluster variable in frame.1")
  if (!name.cluster %in% colnames(frame.2))
    stop("ERROR: Can not find cluster variable in frame.2")

  # Sampling

  s.2 <- as.vector(frame.2[, name.cluster][sample(1:N, n)])
  
  tmp <- data.frame(s.2, (1:n-1) %% weeks + 1)
  names(tmp) <- c(name.cluster, name.week)

  s.1 <- as.data.frame(frame.1[frame.1[,name.cluster] %in% s.2, ])
  s.1[name.weight] <- N/n
  
  s.1[name.week] <- NULL
  s.1 <- merge(s.1, tmp)

  return(s.1)
}

################
### Dev Area ###
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
# # DATA
# setwd(dir)
# frame.p <- attach.big.matrix("frame.p.desc")
# frame.h <- attach.big.matrix("frame.h.desc")
# 
# head(frame.p)
# head(frame.h)
# 
# 
# 
# # Temp parameters
# 
# frame.1 <- frame.p
# frame.2 <- frame.h
# n <- 10
# name.weight <- ".dw"
# name.cluster <- "H_ID"
# name.week <- "week"
# weeks <- 6
# 
# 
# ### Testing
# 
# 
# nrow(frame.p)
# 
# s <- SamplingClusterWeek(frame.1 = frame.p, frame.2 = frame.h, n = 500,
#   name.weight = ".dw", name.cluster = "H_ID")
# head(s)
# sum(s$.dw)
# 
# s <- SamplingClusterWeek(frame.1 = frame.p, frame.2 = frame.h, n = 500,
#   name.weight = ".dw", name.cluster = "H_ID", weeks = 13)
# head(s)
# sum(s$.dw)
# table(s$.week)
