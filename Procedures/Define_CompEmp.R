### Function to compute secondary estimates

CompEmp <- function(x, var.names) {
  
  x <- as.data.frame(x)
  if (is.null(dim(x))) stop("x has to be two dimensional")
  
  var.names <- as.character(var.names)[1:3]
  
  x$sum.pop <- rowSums(x[var.names])
  x$sum.act <- rowSums(x[var.names[1:2]])
  
  x$r.act <- x$sum.act / x$sum.pop
  x$r.empl <- x[, var.names[1]] / x$sum.pop
  x$r.unempl <- x[, var.names[2]] / x$sum.act
  
  return(x)
}
