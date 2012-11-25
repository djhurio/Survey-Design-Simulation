######## Function: Cost calculation for one interviewer

### Ver01

# Cost1 <- function(trip = 0, speed, n.h = 0, n.p = 0, time.h = 0, time.p = 0) {
#   
#   trip <- as.numeric(trip)
#   speed <- as.numeric(speed)
#   n.h <- as.numeric(n.h)
#   n.p <- as.numeric(n.p)
#   time.h <- as.numeric(time.h)
#   time.p <- as.numeric(time.p)
#   
#   return(trip / 1e3 / speed + n.h * time.h + n.p * time.p)
# }



### Testing

# Cost(100e3, 100, 5, 10, 1, .5)
# Cost(rep(100e3, 3), c(20, 30, 30), 5, 10, 1, .5)


### Ver02

# Cost2 <- function(trip = 0,
#                   cons = 0,
#                   price.f = 0,
#                   n.h = 0,
#                   n.p = 0,
#                   price.h = 0,
#                   price.p = 0) {
#   
#   trip <- as.numeric(trip)
#   cons <- as.numeric(cons)
#   price.f <- as.numeric(price.f)
#   n.h <- as.numeric(n.h)
#   n.p <- as.numeric(n.p)
#   price.h <- as.numeric(price.h)
#   price.p <- as.numeric(price.p)
#   
#   return(trip * cons * price.f + n.h * price.h + n.p * price.p)
# }


### Ver03

Cost <- function(trip = 0,
                  cons = 0,
                  price.f = 0,
                  k.d = 1,
                  n.h = 0,
                  n.p = 0,
                  price.h = 0,
                  price.p = 0) {
  
  trip <- as.numeric(trip)
  cons <- as.numeric(cons)
  price.f <- as.numeric(price.f)
  k.d <- as.numeric(k.d)
  n.h <- as.numeric(n.h)
  n.p <- as.numeric(n.p)
  price.h <- as.numeric(price.h)
  price.p <- as.numeric(price.p)
  
  return(trip * cons * price.f * k.d + n.h * price.h + n.p * price.p)
}



### Testing

# Cost(trip = 100, cons = 8, price.f = 1,
#       n.h = 10, n.p = 20, price.h = 3, price.p = 1)
# Cost(trip = 100, cons = c(5, 8, 10), price.f = 1,
#       n.h = 10, n.p = 20, price.h = 3, price.p = 1)
