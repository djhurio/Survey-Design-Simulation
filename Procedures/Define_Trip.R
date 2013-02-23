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

Trip.fast.geo <- function(data, coord.int) {
  # Libs
  require(TSP)
  
  # Argument type convertion
  data <- as.data.frame(data[1:2])
  coord.int <- as.numeric(coord.int)[1:2]
  
  data <- rbind(coord.int, data)
  
  dist.matrix <- dist(data)
  tsp <- TSP(dist.matrix)
  t <- solve_TSP(tsp, control = list(start = 1L))
  
  return(rbind(data[t, ], coord.int))
}

# Trip.fast.geo(data.frame(1:2, 1:2), c(3, 4))

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


### Plot Trip

plot.Trip <- function(data, x, y, int, coord.int, map) {
  # Libs
  require(TSP)
  require(ggplot2)
  
  # Check
  if (names(coord.int)[1] != int) stop("Wrong name of interviewer ID")
  
  # Argument type convertion
  data <- as.data.frame(data)
  x <- as.character(x)[1]
  y <- as.character(y)[1]
  int <- as.character(int)[1]
  coord.int <- as.data.frame(coord.int)[1:3]
  
  # Map as data.frame
  map.df <- fortify(map)
  
  # Tour
  comp.dist <- function(value) Trip.fast(data[data[, int] == value, c(x, y)],
    coord.int[coord.int[, int] == value, 2:3])
  tl <- sum(sapply(unique(data[, int]), comp.dist))

  # Path
  path.int <- function(value) {
    p <- Trip.fast.geo(data[data[, int] == value, c(x, y)],
                  coord.int[coord.int[,1] == value, 2:3])
    data.frame(int.ID = value, p)
  }
  
  path <- lapply(unique(data[, int]), path.int)
  path <- do.call(rbind, path)
  
  # Plot
  ggplot(map.df, aes_string(x = "long", y = "lat")) +
    geom_point(size = .5, colour = "gray") +
    geom_point(aes_string(x = x, y = y), data, size = 1.5, colour = "red") +
    geom_point(aes_string(x = names(coord.int)[2], y = names(coord.int)[3]),
               coord.int, shape = 2) +
    geom_path(aes_string(x = x, y = y, group = "int.ID"), path,
              colour = "blue", size = .2) +
    labs(x = "LKS92 E", y = "LKS92 N") +
    scale_y_continuous(labels = comma) +
    scale_x_continuous(labels = comma) +
    ggtitle(paste("Kopīgais ceļa garums:", comma(round(tl / 1e3)), "km.",
                  "Izlases apjoms:", nrow(data), "personas.")) +
    theme_bw()
}

# head(frame.int)

# plot.Trip(s, "coord_x_p", "coord_y_p", "int.ID",
#   coord.int = frame.int[c("int.ID", "x_int", "y_int")], map = LV2_LKS92)
