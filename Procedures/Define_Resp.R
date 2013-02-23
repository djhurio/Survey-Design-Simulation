### Response definition
### Defines probabilities to respond and indicator of response

Resp <- function(frame.1, names.ID, name.by, p, prob = T) {
  
  # Libs
  require(bigmemory)

  # Argument type convertion
  if (!is.big.matrix(frame.1)) frame.1 <- as.data.frame(frame.1)
  name.by <- as.character(name.by)[1]
  p <- as.vector(as.numeric(p))
  prob <- as.logical(prob)[1]
  
  # Testing
  if (!name.by %in% colnames(frame.1))
    stop("ERROR: Can not find by variable in frame.1")
  if (!all(names.ID %in% colnames(frame.1)))
    stop("ERROR: Can not find by ID variables in frame.1")

  values.by <- sort(unique(frame.1[, name.by]))
  
  if (length(values.by) != length(p))
    stop("ERROR: Wrong length of p")
  if (any(p<0, p>1))
    stop("ERROR: Wrong values of p")
  if (any(p == 0))
    print("WARNING: Some values of p are 0")

  # Calc
  IDs <- data.frame(frame.1[, names.ID])
  names(IDs) <- names.ID
  
  resp.p <- p[match(frame.1[, name.by], values.by)]
  
  if (prob) {
    print(data.frame(values.by, p))
    return(data.frame(IDs, resp.p = resp.p))
  } else {
    resp <- as.integer(runif(nrow(frame.1)) < resp.p)
    print(cbind(values.by, p,
      round(prop.table(table(frame.1[, name.by], resp), margin = 1), 2)))
    return(data.frame(IDs, resp = resp))
  }
}


### Dev

# setwd(dir.data)
# frame.1 <- attach.big.matrix("frame.h.desc")
# head(frame.1)
# 
# r1 <- Resp(frame.1, "H_ID", "strata", c(.6, .7, .75, .8), T)
# head(r1)
# table(r1$resp.p)
# 
# r2 <- Resp(frame.1, "H_ID", "strata", c(.6, .7, .75, .8), F)
# head(r2)
# table(r2$resp)
# 
# r1 <- Resp(frame.1, "H_ID", "strata3", c(.6, .7, .8), T)
# head(r1)
# table(r1$resp.p)
# 
# r2 <- Resp(frame.1, "H_ID", "strata3", c(.6, .7, .8), F)
# head(r2)
# table(r2$resp)
