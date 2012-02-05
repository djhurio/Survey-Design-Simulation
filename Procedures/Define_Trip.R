###### Function: estimation

### Ver01
# Initial version

Trip.1 <- function(data, x, y, int) {

  # Libs
  require(bigmemory)
  require(TSP)
  
  # Argument type convertion
  if (!is.big.matrix(data)) data <- as.data.frame(data)
  x <- as.character(x)[1]
  y <- as.character(y)[1]
  int <- as.character(int)[1]
  
  # Testing
  
  # Tour
  
  comp.dist <- function(value) {
    dist.matrix <- dist(data[data[, int] == value, c(x, y)])
    tsp <- TSP(dist.matrix)
    t <- solve_TSP(tsp)
    attr(t, "tour_length") / 1e3
  }

  return(sum(sapply(unique(data[, int]), comp.dist)))
}




### Ver02
# Plot of trip available

Trip.2 <- function(data, x, y, int, draw.plot = F) {

  # Libs
  require(bigmemory)
  require(TSP)
  require(sp)
  require(maptools)
  
  # Argument type convertion
  if (!is.big.matrix(data)) data <- as.data.frame(data)
  x <- as.character(x)[1]
  y <- as.character(y)[1]
  int <- as.character(int)[1]
  draw.plot <- as.logical(draw.plot)
  
  # Testing
  
  # Tour
  
  comp.dist <- function(value) {
    dist.matrix <- dist(data[data[, int] == value, c(x, y)])
    tsp <- TSP(dist.matrix)
    t <- solve_TSP(tsp)
    attr(t, "tour_length") / 1e3
  }
  
  if (draw.plot) {
    plot(data[, c(x, y)], pch = 3, cex = 0.4, col = "red")
    for (i in unique(data[, int])) {
      dist.matrix <- dist(data[data[, int] == i, c(x, y)])
      tsp <- TSP(dist.matrix)
      t <- solve_TSP(tsp)
      lines(data[data[, int] == i, c(x, y)][t, ], col = i)
    }
  }

  return(sum(sapply(unique(data[, int]), comp.dist)))
}



### Ver03
# Add map

Trip <- function(data, x, y, int, draw.plot = F, map) {

  # Libs
  require(bigmemory)
  require(TSP)
  require(sp)
  require(maptools)
  
  # Argument type convertion
  if (!is.big.matrix(data)) data <- as.data.frame(data)
  x <- as.character(x)[1]
  y <- as.character(y)[1]
  int <- as.character(int)[1]
  draw.plot <- as.logical(draw.plot)
  
  # Testing
  
  # Tour
  
  comp.dist <- function(value) {
    dist.matrix <- dist(data[data[, int] == value, c(x, y)])
    tsp <- TSP(dist.matrix)
    t <- solve_TSP(tsp)
    attr(t, "tour_length") / 1e3
  }
  
  if (draw.plot) {
    plot(map)
    points(data[, c(x, y)], pch = 1, cex = 0.4, col = "red")
    for (i in unique(data[, int])) {
      dist.matrix <- dist(data[data[, int] == i, c(x, y)])
      tsp <- TSP(dist.matrix)
      t <- solve_TSP(tsp)
      lines(data[data[, int] == i, c(x, y)][t, ], col = "blue")
    }
  }
  
  tr <- try(sum(sapply(unique(data[, int]), comp.dist)), T)
  if (class(tr) != "try-error") return(tr) else return(-99999)
  
}

################
### Dev area ###
################

# # libs
# library(bigmemory)
# 
# 
# # Workdir
# dir.data <- "~/DATA/LU/Work"
# 
# 
# # DATA
# 
# setwd(dir.data)
# 
# load("LV_maps.Rdata")
# 
# pop <- attach.big.matrix("frame.p.desc")
# head(pop)
# N <- nrow(pop)
# 
# n <- 1000
# s <- as.data.frame(pop[sample(1:N, n), ])
# head(s)
# 
# 
# ### Temp param
# 
# data <- s
# x <- "coord_x_p"
# y <- "coord_y_p"
# int <- "int.ID"
# draw.plot <- T
# map <- LV2_LKS92
# 
# 
# ### Test run
# 
# set.seed(1)
# Trip(data=s, x="coord_x_p", y="coord_y_p", int="int.ID", draw.plot=T, map=LV2_LKS92)
# 
# Trip(data=s, x="coord_x_p", y="coord_y_p", int="int.ID", draw.plot=F)
