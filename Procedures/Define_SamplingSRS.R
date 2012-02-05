# ###### Function: SamplingSRS

# ### Ver01
# # Initial version


SamplingSRS <- function(frame.1, n = 30, name.weight = ".dw") {
  
  # Libs
  require(bigmemory)

  # Argument type convertion
  
  if (!is.big.matrix(frame.1)) frame.1 <- as.data.frame(frame.1)
  n <- as.integer(n[1])
  name.weight <- as.character(name.weight[1])

  # Testing

  N <- nrow(frame.1)
  if (n<=0 | n>N) stop("n has to be in 0-N")
  if (name.weight %in% colnames(frame.1))
    print("WARNING: Weight variable exists, it will be overwrited")

  # Sampling

  s.1 <- as.data.frame(frame.1[sample(1:N, n), ])
  s.1[name.weight] <- N/n

  return(s.1)
}


################
### Dev area ###
################

# # libs
# library(bigmemory)


# # Workdir
# # dir <- "/home/djhurio/temp"
# # dir <- "C:/DATA/LU/Results"
# dir <- "C:/Users/Martins Liberts/Documents/DATA/LU/Work"



# # DATA
# setwd(dir)
# pop <- attach.big.matrix("frame.p.desc")
# head(pop)


# # # Temp parameters

# frame <- pop
# n <- 10
# name.weight <- ".dw"



# nrow(pop)

# s <- SamplingSRS(frame = pop, n = 100, name.weight = ".dw")
# head(s)
# sum(s$.dw)
