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
  n <- nrow(x)
  N <- ifelse(length(w) == 1, w * n, sum(w))

  E <- as.data.frame(matrix(NA, 1, 2+nrow(param)))
  
  E[1,1] <- N
  E[1,2] <- n
  colnames(E)[1:2] <- c("N", "n")
  
  for (i in 1:nrow(param)) {
    a <- sum(w * x[,param[i,2]])
    if (param[i,1] == "sum") estim <- as.matrix(a)
    if (param[i,1] == "mean") estim <- as.matrix(a / N)
    if (param[i,1] %in% c("sum","mean")) nam <- paste(param[i,1], ".", param[i,2], sep="")
    if (param[i,1] == "ratio") {
      b <- sum(w * x[,param[i,3]])
      estim <- as.matrix(a / b)
      nam <- paste("r", param[i,2], param[i,3], sep=".")
      }
    E[1,2+i] <- estim
    colnames(E)[2+i] <- nam
    }

  return(E)
}
