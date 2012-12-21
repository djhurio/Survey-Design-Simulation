######## Function: Cost calculation for one interviewer

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
