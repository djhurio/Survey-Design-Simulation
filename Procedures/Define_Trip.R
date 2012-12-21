###### Function: Distance for one interviewer in unlimited time

### Ver01
# Initial version

### Ver02
# Plot of trip available

### Ver03
# Add map

### Ver04
# Simplify

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
# Fast version

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
  
  return(attr(t, "tour_length"))
}


###### Vectorised Trip

### VerV02
### Added period

### VerV02b
### Fast version

vTrip.fast.1 <- function(data, coord.int) {
  
  # Argument type convertion
  data <- as.data.frame(data[1:4])
  coord.int <- as.data.frame(coord.int)[1:3]
  
  # Redefine Trip
  Trip.alt <- function(int, week) {
    r <- Trip.fast(data[data[,1] == int & data[,2] == week, 3:4],
                   coord.int[coord.int[,1] == int, 2:3])
    return(r)
  }
  
  # Run
  tab <- unique(data[1:2])
  
  res <- mapply(Trip.alt, tab[,1], tab[,2])
  
  return(sum(res))
}
