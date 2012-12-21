### Function extr.data()

### Ver01
### Initial version

### Ver02
# With mapply

extr.data <- function(x, rows, cols, col.skip = 0, var.name = "var") {
  
  rows <- as.integer(rows)
  cols <- as.integer(cols)
  col.skip <- as.integer(col.skip)[1]
  var.name <- as.character(var.name)[1]
  
  N <- nrow(x)
  M <- ncol(x) - col.skip
  
  if (is.null(dim(x))) stop("x is not a two dimensional object")

  if (min(rows) < 1) stop("wrong id for rows")
  if (max(rows) > N) stop("wrong id for rows")
  if (min(cols) < 1) stop("wrong id for cols")
  if (max(cols) > M) stop("wrong id for cols")
  
  if (length(rows) != length(cols)) stop("rows and cols has to be the same length")

  extr <- function(r, c) x[r, c]
  d2 <- mapply(extr, rows, cols + col.skip)
  
  if (col.skip > 0) {
    d <- data.frame(data.frame(x[rows, 1:col.skip]), d2)
  } else {
    d <- data.frame(d2)
  }
  
  colnames(d)[col.skip + 1] <- var.name
  
  return(d)
}
