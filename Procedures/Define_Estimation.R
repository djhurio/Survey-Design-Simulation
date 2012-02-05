###### Function: estimation

### Ver01
# Initial version

### Ver02
# Weights are defined as vector or scalar


Estimation <- function(x, w, param) {

  # Libs
  require(bigmemory)

  # Argument type convertion
  if (!is.big.matrix(x)) x <- as.data.frame(x)
  w <- as.vector(as.numeric(w))
  param <- as.matrix(param)
  
  # Testing
  if (length(w) != 1 & length(w) != nrow(x)) stop("Error in w")
  if (!all(param[,1] %in% c("sum", "mean", "ratio"))) stop("Error in param 1")
  if (ncol(param) != 3) stop("Error in param 2")
  
  # Estim
  N <- ifelse(length(w) == 1, w * nrow(x), sum(w))
  n <- nrow(x)

  E <- cbind(N, n)
  for (i in 1:nrow(param)) {
    a <- sum(w * x[,param[i,2]])
    if (param[i,1] == "sum") estim <- as.matrix(a)
    if (param[i,1] == "mean") estim <- as.matrix(a / N)
    if (param[i,1] %in% c("sum","mean")) colnames(estim) <- paste(param[i,1], ".", param[i,2], sep="")
    if (param[i,1] == "ratio") {
      b <- sum(w * x[,param[i,3]])
      estim <- as.matrix(a / b)
      colnames(estim) <- paste("r", param[i,2], param[i,3], sep=".")
      }
    E <- cbind(E, estim)
    }

  rownames(E) <- NULL
  return(E)
}




################
### Dev area ###
################

# # libs
# library(bigmemory)


# # Workdir
# dir.data <- "C:/Users/Martins Liberts/Documents/DATA/LU/Work"


# # DATA

# setwd(dir.data)

# pop <- attach.big.matrix("frame.p.desc")
# head(pop)
# N <- nrow(pop)

# n <- 1000
# s <- as.data.frame(pop[sample(1:N, n), ])
# s$dw <- N/n
# head(s)

# dw <- rep(N/n, n)

# param <- rbind(c("sum","y1",NA), c("sum","y2",NA), c("mean","y1",NA), c("ratio","y2","y4"))


# ### Temp param

# x <- s
# w <- dw
# param <- param

# head(x)
# head(pop)

# head(s)

# Estimation(x=s, w=dw, param=param)
# Estimation(x=s, w=N/n, param=param)
# Estimation(x=s, w=s[,"dw"], param=param)
# Estimation(x=s, w=s$dw, param=param)
