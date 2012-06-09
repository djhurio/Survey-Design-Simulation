###### Function: Distance for one interviewer in unlimited time

### Ver01
# Initial version

# Trip.1 <- function(data, x, y, int) {
# 
#   # Libs
#   require(bigmemory)
#   require(TSP)
#   
#   # Argument type convertion
#   if (!is.big.matrix(data)) data <- as.data.frame(data)
#   x <- as.character(x)[1]
#   y <- as.character(y)[1]
#   int <- as.character(int)[1]
#   
#   # Testing
#   
#   # Tour
#   
#   comp.dist <- function(value) {
#     dist.matrix <- dist(data[data[, int] == value, c(x, y)])
#     tsp <- TSP(dist.matrix)
#     t <- solve_TSP(tsp)
#     attr(t, "tour_length") / 1e3
#   }
# 
#   return(sum(sapply(unique(data[, int]), comp.dist)))
# }




### Ver02
# Plot of trip available

# Trip.2 <- function(data, x, y, int, draw.plot = F) {
# 
#   # Libs
#   require(bigmemory)
#   require(TSP)
#   require(sp)
#   require(maptools)
#   
#   # Argument type convertion
#   if (!is.big.matrix(data)) data <- as.data.frame(data)
#   x <- as.character(x)[1]
#   y <- as.character(y)[1]
#   int <- as.character(int)[1]
#   draw.plot <- as.logical(draw.plot)
#   
#   # Testing
#   
#   # Tour
#   
#   comp.dist <- function(value) {
#     dist.matrix <- dist(data[data[, int] == value, c(x, y)])
#     tsp <- TSP(dist.matrix)
#     t <- solve_TSP(tsp)
#     attr(t, "tour_length") / 1e3
#   }
#   
#   if (draw.plot) {
#     plot(data[, c(x, y)], pch = 3, cex = 0.4, col = "red")
#     for (i in unique(data[, int])) {
#       dist.matrix <- dist(data[data[, int] == i, c(x, y)])
#       tsp <- TSP(dist.matrix)
#       t <- solve_TSP(tsp)
#       lines(data[data[, int] == i, c(x, y)][t, ], col = i)
#     }
#   }
# 
#   return(sum(sapply(unique(data[, int]), comp.dist)))
# }



# ### Ver03
# # Add map
# 
# Trip.3 <- function(data, x, y, int, draw.plot = F, map) {
# 
#   # Libs
#   require(bigmemory)
#   require(TSP)
#   require(sp)
#   require(maptools)
#   
#   # Argument type convertion
#   if (!is.big.matrix(data)) data <- as.data.frame(data)
#   x <- as.character(x)[1]
#   y <- as.character(y)[1]
#   int <- as.character(int)[1]
#   draw.plot <- as.logical(draw.plot)
#   
#   # Testing
#   # Tour
#   
#   comp.dist <- function(value) {
#     dist.matrix <- dist(data[data[, int] == value, c(x, y)])
#     tsp <- TSP(dist.matrix)
#     t <- solve_TSP(tsp)
#     attr(t, "tour_length") / 1e3
#   }
#   
#   if (draw.plot) {
#     plot(map)
#     points(data[, c(x, y)], pch = 1, cex = 0.4, col = "red")
#     for (i in unique(data[, int])) {
#       dist.matrix <- dist(data[data[, int] == i, c(x, y)])
#       tsp <- TSP(dist.matrix)
#       t <- solve_TSP(tsp)
#       lines(data[data[, int] == i, c(x, y)][t, ], col = "blue")
#     }
#   }
#   
#   tr <- try(sum(sapply(unique(data[, int]), comp.dist)), T)
#   if (class(tr) != "try-error") return(tr) else return(-99999)
#   
# }


### Ver04
# Simplify

# Trip <- function(data, coord.int, draw.plot = F) {
#   
#   # Libs
#   # require(bigmemory)
#   require(TSP)
#   
#   # Argument type convertion
#   data <- as.data.frame(data)
#   coord.int <- as.numeric(coord.int)[1:2]
#   draw.plot <- as.logical(draw.plot)[1]
#   
#   data <- rbind(coord.int, data)
#   
#   dist.matrix <- dist(data)
#   tsp <- TSP(dist.matrix)
#   t <- solve_TSP(tsp, control = list(start = 1L))
#   
#   if (draw.plot) {
#     plot(data, pch = 3, cex = 0.4, col = "red")
#     points(coord.int[1], coord.int[2], pch = 1, cex = 1, col = "black")
#     lines(rbind(data[t, ], coord.int), col = "blue")
#   }
#   
#   return(attr(t, "tour_length"))
#   
# }



### Ver05
# Return list - trip distance and plot

Trip <- function(data, coord.int) {
  
  # Libs
  require(TSP)
  
  # Argument type convertion
  data <- as.data.frame(data[1:2])
  coord.int <- as.numeric(coord.int)[1:2]
  
  data <- rbind(coord.int, data)
  
  dist.matrix <- dist(data)
  tsp <- TSP(dist.matrix)
  t <- solve_TSP(tsp, control = list(start = 1L))
  
  data <- as.data.frame(rbind(data[t, ], coord.int))
  
  rownames(data) <- NULL
  
  return(list(attr(t, "tour_length"), data))
  
}


### Ver05b
### Fast version

Trip.fast <- function(data, coord.int) {
  
  # Libs
  require(TSP)
  
  # Argument type convertion
  data <- as.data.frame(data[1:2])
  coord.int <- as.numeric(coord.int)[1:2]
  
  data <- rbind(coord.int, data)
  
  dist.matrix <- dist(data)
  tsp <- TSP(dist.matrix)
  t <- solve_TSP(tsp, control = list(start = 1L))
  
  # data <- as.data.frame(rbind(data[t, ], coord.int))
  # rownames(data) <- NULL
  
  # return(list(attr(t, "tour_length"), data))
  return(attr(t, "tour_length"))
  
}



### Vectorised Trip

# vTrip.1 <- function(data, coord.int) {
#   
#   # Argument type convertion
#   data <- as.data.frame(data[1:3])
#   coord.int <- as.data.frame(coord.int)[1:3]
# 
#   # Test
#   if (nrow(unique(data[1])) != nrow(unique(coord.int[1]))) stop("Error1 in int.ID")
#   if (!all(sort(unique(data[,1])) == sort(unique(coord.int[,1])))) stop("Error2 in int.ID")
#   
#   # Run
#   
#   res <- mapply(Trip, split(data[2:3], data[1]), split(coord.int[2:3], coord.int[1]))
#   
#   res1 <- as.data.frame(do.call(rbind, res[1, ]))
#   res1 <- data.frame(rownames(res1), res1)
#   names(res1) <- c(names(coord.int[1]), "dist")
#   rownames(res1) <- NULL
#   res1
#   
#   res2 <- as.data.frame(do.call(rbind, res[2, ]))
#   res2 <- data.frame(floor(as.numeric(rownames(res2))), res2)
#   names(res2)[1] <- names(coord.int[1])
#   rownames(res2) <- NULL
#   head(res2)
# 
#   return(list(res1, res2))
#   
# }


### Vectorised Trip
### V02
### Added period

vTrip <- function(data, coord.int) {
  
  # Argument type convertion
  data <- as.data.frame(data[1:4])
  coord.int <- as.data.frame(coord.int)[1:3]
  
  int.in.data <- unique(data[,1])
  coord.int <- coord.int[coord.int[,1] %in% int.in.data, ]
  
  length(int.in.data)
  
  # Test
  if (length(int.in.data) != length(unique(coord.int[,1]))) stop("Error1 in int.ID")
  if (!all(sort(int.in.data) == sort(unique(coord.int[,1])))) stop("Error2 in int.ID")
  
  # Redefine Trip
  
  Trip.alt <- function(int, week) {
    r <- Trip(data[data[,1] == int & data[,2] == week, 3:4], coord.int[coord.int[,1] == int, 2:3])
    res1 <- data.frame(int = int, week = week, dist = r[[1]])
    res2 <- data.frame(int = int, week = week, r[[2]])
    return(list(res1, res2))
  }

  # Run
  
  tab <- unique(data[1:2])
  tab <- tab[order(tab[,1], tab[,2]), ]
  rownames(tab) <- NULL
  
  res <- mapply(Trip.alt, tab[,1], tab[,2])
  
  res1 <- as.data.frame(do.call(rbind, res[1, ]))
  res2 <- as.data.frame(do.call(rbind, res[2, ]))
  
  return(list(res1, res2))
  
}

### Vectorised Trip
### V02b
### Fast version

vTrip.fast.1 <- function(data, coord.int) {
  
  # Argument type convertion
  data <- as.data.frame(data[1:4])
  coord.int <- as.data.frame(coord.int)[1:3]
  
  #int.in.data <- unique(data[,1])
  #coord.int <- coord.int[coord.int[,1] %in% int.in.data, ]
  
  #length(int.in.data)
  
  # Test
  #if (length(int.in.data) != length(unique(coord.int[,1]))) stop("Error1 in int.ID")
  #if (!all(sort(int.in.data) == sort(unique(coord.int[,1])))) stop("Error2 in int.ID")
  
  # Redefine Trip
  
  Trip.alt <- function(int, week) {
    r <- Trip.fast(data[data[,1] == int & data[,2] == week, 3:4], coord.int[coord.int[,1] == int, 2:3])
    #res1 <- data.frame(int = int, week = week, dist = r[[1]])
    #res2 <- data.frame(int = int, week = week, r[[2]])
    #return(list(res1, res2))
    return(r)
  }
  
  # Run
  
  tab <- unique(data[1:2])
  #tab <- tab[order(tab[,1], tab[,2]), ]
  #rownames(tab) <- NULL
  
  res <- mapply(Trip.alt, tab[,1], tab[,2])
  
  #res1 <- as.data.frame(do.call(rbind, res[1, ]))
  #res2 <- as.data.frame(do.call(rbind, res[2, ]))
  
  #return(list(res1, res2))
  return(sum(res))
  
}


### Vectorised Trip
### V02c
### Fast version
### Replace of mapply by lapply

vTrip.fast.2 <- function(data, coord.int) {
  
  # Argument type convertion
  data <- as.data.frame(data[1:4])
  coord.int <- as.data.frame(coord.int)[1:3]
  
  #int.in.data <- unique(data[,1])
  #coord.int <- coord.int[coord.int[,1] %in% int.in.data, ]
  
  #length(int.in.data)
  
  # Test
  #if (length(int.in.data) != length(unique(coord.int[,1]))) stop("Error1 in int.ID")
  #if (!all(sort(int.in.data) == sort(unique(coord.int[,1])))) stop("Error2 in int.ID")
  
  # Redefine Trip
  
  Trip.alt <- function(x) {
    x <- unlist(x)
    r <- Trip.fast(data[data[,1] == x[1] & data[,2] == x[2], 3:4], coord.int[coord.int[,1] == x[1], 2:3])
    return(r)
  }
  
  # Run
  
  tab <- unique(data[1:2])
  rownames(tab) <- NULL
  tab <- split(tab, rownames(tab))
  
  res <- sapply(tab, Trip.alt)
  
  #res1 <- as.data.frame(do.call(rbind, res[1, ]))
  #res2 <- as.data.frame(do.call(rbind, res[2, ]))
  
  #return(list(res1, res2))
  return(sum(res))
  
}


### Vectorised Trip
### V02d
### Fast version
### paralel

vTrip.fast.3 <- function(data, coord.int) {
  
  require(parallel)
  #getOption("mc.cores", 2L)
    
  # Argument type convertion
  data <- as.data.frame(data[1:4])
  coord.int <- as.data.frame(coord.int)[1:3]
  
  #int.in.data <- unique(data[,1])
  #coord.int <- coord.int[coord.int[,1] %in% int.in.data, ]
  
  #length(int.in.data)
  
  # Test
  #if (length(int.in.data) != length(unique(coord.int[,1]))) stop("Error1 in int.ID")
  #if (!all(sort(int.in.data) == sort(unique(coord.int[,1])))) stop("Error2 in int.ID")
  
  # Redefine Trip
  
  Trip.alt <- function(x) {
    x <- unlist(x)
    r <- Trip.fast(data[data[,1] == x[1] & data[,2] == x[2], 3:4], coord.int[coord.int[,1] == x[1], 2:3])
    return(r)
  }
  
  # Run
  
  tab <- unique(data[1:2])
  rownames(tab) <- NULL
  tab <- split(tab, rownames(tab))
  
  res <- simplify2array(mclapply(tab, Trip.alt))
  #sum(res)
  
  #res1 <- as.data.frame(do.call(rbind, res[1, ]))
  #res2 <- as.data.frame(do.call(rbind, res[2, ]))
  
  #return(list(res1, res2))
  return(sum(res))
  
}






################
### Dev area ###
################

# ### Reset ###
# setwd(projwd)
# rm(list = ls())
# gc()
# source(".Rprofile")
# 
# 
# # libs
# library(bigmemory)
# require(rbenchmark)
# 
# 
# # DATA
# setwd(dir.data.out)
# pop <- attach.big.matrix("frame.p.desc")
# head(pop)
# 
# load("frame.int.Rdata")
# head(frame.int)
# 
# N <- nrow(pop)
# N1 <- nrow(pop[pop[, "int.ID"] == 1, ])
# N2 <- nrow(pop[pop[, "int.ID"] == 2, ])
# N3 <- nrow(pop[pop[, "int.ID"] == 3, ])
# 
# n <- 100
# 
# s1 <- as.data.frame(pop[pop[, "int.ID"] == 1, ][sample(1:N1, n), ])
# s2 <- as.data.frame(pop[pop[, "int.ID"] == 2, ][sample(1:N2, n), ])
# s3 <- as.data.frame(pop[pop[, "int.ID"] == 3, ][sample(1:N3, n), ])
# head(s1)
# head(s2)
# head(s3)
# 
# 
# ### Temp param
# 
# # data <- s[c("coord_x_p", "coord_y_p")]
# # coord.int <- c(frame.int[1, 3], frame.int[1, 4])
# 
# 
# ### Test run Trip
# 
# r1 <- Trip(data=s1[c("coord_x_p", "coord_y_p")], coord.int = frame.int[1, 3:4])
# r2 <- Trip(data=s2[c("coord_x_p", "coord_y_p")], coord.int = frame.int[2, 3:4])
# r3 <- Trip(data=s3[c("coord_x_p", "coord_y_p")], coord.int = frame.int[3, 3:4])
# 
# class(r1)
# class(r1[[1]])
# class(r1[[2]])
# 
# r1[[1]]
# r2[[1]]
# r3[[1]]
# 
# Trip.fast(data=s1[c("coord_x_p", "coord_y_p")], coord.int = frame.int[1, 3:4])
# Trip.fast(data=s2[c("coord_x_p", "coord_y_p")], coord.int = frame.int[2, 3:4])
# Trip.fast(data=s3[c("coord_x_p", "coord_y_p")], coord.int = frame.int[3, 3:4])
# 
# 
# head(r1[[2]])
# head(r2[[2]])
# head(r3[[2]])
# 
# 
# 
# 
# ### Test run vTrip
# 
# data <- rbind(s1, s2, s3)[c("int.ID", "coord_x_p", "coord_y_p")]
# data$week <- ceiling(runif(nrow(data)) * 3) + 5
# data <- data[c("int.ID", "week", "coord_x_p", "coord_y_p")]
# class(data)
# head(data)
# table(data$week)
# 
# coord.int <- frame.int[, -1]
# class(coord.int)
# head(coord.int)
# 
# r <- vTrip(data, coord.int)
# 
# class(r)
# class(r[[1]])
# class(r[[2]])
# 
# r[[1]]
# sum(r[[1]][3])
# vTrip.fast.1(data, coord.int)
# 
# r[[2]]
# 
# s <- data.frame(pop[sample(1:N, 10000), c("int.ID", "coord_x_p", "coord_y_p")])
# s$week <- ceiling(runif(nrow(s)) * 13)
# s <- s[c("int.ID", "week", "coord_x_p", "coord_y_p")]
# head(s)
# table(s$week)
# 
# r <- vTrip(s, coord.int)
# sum(r[[1]][3])
# 
# vTrip.fast.1(s, coord.int)
# vTrip.fast.2(s, coord.int)
# vTrip.fast.3(s, coord.int)
# 
# 
# benchmark(vTrip.fast.1(s, coord.int),
#           vTrip.fast.2(s, coord.int),
#           vTrip.fast.3(s, coord.int),
#           columns=c("test", "replications", "elapsed", "relative"),
#           order="relative", replications=5)
# 
# debug(vTrip)
# debug(vTrip.fast)
# 
# isdebugged(vTrip)
# isdebugged(vTrip.fast)
# 
# undebug(vTrip)
# undebug(vTrip.fast)
# 
# 
