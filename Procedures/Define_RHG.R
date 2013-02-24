### RHG Estimator
### RHG weight correction

RHG <- function(x, name.w, name.rhg, name.resp) {
  
  # Libs
  require(bigmemory)

  # Argument type convertion
  if (!is.data.frame(x)) x <- as.data.frame(x)
  name.w <- as.character(name.w)[1]
  name.rhg <- as.character(name.rhg)[1]
  name.resp <- as.character(name.resp)[1]
  
  # Testing
  if (!name.w %in% colnames(x))
    stop("ERROR: Can not find weight variable in x")
  if (!name.rhg %in% colnames(x))
    stop("ERROR: Can not find response homogenity group variable in x")
  if (!name.resp %in% colnames(x))
    stop("ERROR: Can not find response status variable in x")
  if (".k.rhg" %in% colnames(x))
    print("WARNING: Variable '.k.rhg' exists in x, it will be overwritten")
  if (".w.rhg" %in% colnames(x))
    print("WARNING: Variable '.w.rhg' exists in x, it will be overwritten")
  if (any(is.na(x[, name.w])))
    stop("ERROR: There are NA values in weight")
  if (any(is.na(x[, name.rhg])))
    stop("ERROR: There are NA values in RHG variable")
  if (any(is.na(x[, name.resp])))
    stop("ERROR: There are NA values in response status")
  if (any(x[, name.resp] != 0 & x[, name.resp] != 1))
    stop("ERROR: Response status has to be 0 or 1")

  agg <- aggregate(data.frame(sum.w = x[, name.w],
                              sum.w.resp = x[, name.w] * x[, name.resp]),
                   list(x[, name.rhg]), sum)
  names(agg)[1] <- name.rhg
  agg$.k.rhg <- agg$sum.w / agg$sum.w.resp
  print(agg)
  
  x$.k.rhg <- NULL
  x$.w.rhg <- NULL
  x <- merge(x, agg[c(name.rhg, ".k.rhg")])
  x$.w.rhg <- ifelse(x[, name.resp] == 1, x[, name.w] * x$.k.rhg, 0)
  head(x)
  
  print(aggregate(x[c(name.w, ".w.rhg")], x[name.rhg], sum))
  
  return(x)
}




### Dev

# setwd(dir.proc)
# source("Define_SamplingSRS.R")
# 
# setwd(dir.data)
# frame <- attach.big.matrix("frame.h.desc")
# head(frame)
# 
# r1 <- Resp(frame.1, "H_ID", "strata", c(.6, .7, .75, .8), F)
# head(r1)
# 
# s1 <- SamplingSRS(frame, n = 2e3)
# head(s1)
# nrow(s1)
# 
# s1 <- merge(s1, r1)
# head(s1)
# nrow(s1)
# 
# s1.rhg <- RHG(s1, ".dw", "strata", "resp")
# head(s1.rhg)
# 
# s1.rhg <- RHG(s1, ".dw", "strata3", "resp")
# head(s1.rhg)
# 
# s1.rhg <- RHG(s1, ".dw", "const", "resp")
# head(s1.rhg)
# 
# s1.rhg <- RHG(s1, ".dw", "int.ID", "resp")
# head(s1.rhg)
# 
# 
# ### Test params
# x <- s1
# name.w <- ".dw"
# name.rhg <- "strata3"
# name.resp <- "resp"
