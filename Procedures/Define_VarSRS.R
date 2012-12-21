###### Function VarSRS

### Ver01
# Initial version

VarSRS <- function(data, par, n) {

  # Argument type convertion
  if (!is.big.matrix(data)) data <- as.data.frame(data)
  par <- as.matrix(par)
  n <- as.integer(n)

  # Testing
  N <- nrow(data)
  if (n<=0 | n>N) stop("ERROR: n has to be in 0-N")
  if (!all(par[,1] %in% c("sum", "mean", "ratio"))) stop("ERROR: in par 1")
  if (ncol(par) != 3) stop("ERROR: in par 2")

  # Var estimation
  f <- n/N
  E <- NULL

  for (i in 1:nrow(par)) {

    S_y <- var(data[,par[i,2]])
    
    if (par[i,1] == "sum") estim <- as.matrix(N^2 * (1-f)/n * S_y)
    if (par[i,1] == "mean") estim <- as.matrix((1-f)/n * S_y)
    if (par[i,1] %in% c("sum","mean")) colnames(estim) <- paste("Var_", par[i,1], "_", par[i,2], sep="")
    if (par[i,1] == "ratio") {
      R <- sum(data[,par[i,2]])/sum(data[,par[i,3]])
      S_z <- var(data[,par[i,3]])
      S_yz <- var(data[,par[i,2]], data[,par[i,3]])
      estim <- as.matrix(1/mean(data[,par[i,3]])^2 * (1-f)/n * (S_y + R^2*S_z - 2*R*S_yz))
      colnames(estim) <- paste("Var_", par[i,2], "/", par[i,3], sep="")
      }
    E <- cbind(E, estim)
    }
  rownames(E) <- NULL
  return(E)
}
