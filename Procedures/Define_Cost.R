######## Function: Cost calculation for one interviewer
######## Result: time

### Ver01

Cost <- function(trip = 0, speed, n.h = 0, n.p = 0, time.h = 0, time.p = 0) {
  
  # Testing
  trip <- as.numeric(trip)
  speed <- as.numeric(speed)
  n.h <- as.numeric(n.h)
  n.p <- as.numeric(n.p)
  time.h <- as.numeric(time.h)
  time.p <- as.numeric(time.p)
  
  return(trip / 1e3 / speed + n.h * time.h + n.p * time.p)
  
}



### Testing

# Cost(100e3, 100, 5, 10, 1, .5)
# Cost(rep(100e3, 3), c(20, 30, 30), 5, 10, 1, .5)
