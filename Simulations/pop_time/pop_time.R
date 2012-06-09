########################################
### Population data changing in time ###
########################################


## Reset ###
setwd(projwd)
rm(list = ls())
gc()
source(".Rprofile")



### Libs

require(bigmemory)
require(bigtabulate)
require(foreach)





### Procedures

setwd(dir.proc)

source("Define_SamplingSRSWeek.R")
source("Define_SamplingClusterWeek.R")


### Data

setwd(dir.data)

frame.p <- attach.big.matrix("frame.p.desc")
head(frame.p)

frame.h <- attach.big.matrix("frame.h.desc")
head(frame.h)

pop.eka <- attach.big.matrix("pop.eka.desc")
head(pop.eka)



### True Values

tv.week <- foreach(week = 1:52, .combine = rbind, .final = as.data.frame) %do% {
  c(week, bigtable(pop.eka, paste("eka", week, sep="")))
}

class(tv.week)
head(tv.week)

rownames(tv.week) <- NULL
colnames(tv.week) <- c("week", "empl", "unempl", "inact")
head(tv.week)

tv.week$year <- trunc((tv.week$week - 1) / 52) + 1
tv.week$qrt <- trunc((tv.week$week - 1) / 13) + 1
head(tv.week)

# tv <- as.data.frame(matrix(colMeans(tv.week), 1, 3))

tv <- aggregate(tv.week[, c("empl", "unempl", "inact")], tv.week[, c("year", "qrt")], mean)
tv

tv$pop <- rowSums(tv[c("empl", "unempl", "inact")])
tv$act <- rowSums(tv[c("empl", "unempl")])

tv$r.empl <- tv$empl / tv$pop
tv$r.unempl <- tv$unempl / tv$act

tv

setwd(dir.res)

save(tv, file = "pop.eka_tv.Rdata")


### Sampling

head(frame.p)
head(frame.h)

s1 <- SamplingSRSWeek(frame.p, n=10, weeks = 10)
s2 <- SamplingClusterWeek(frame.p, frame.h, n=10,
                          name.cluster = "H_ID", weeks = 10)

head(s1)
head(s2)

head(pop.eka)



### Simple merge
s1a <- merge(s1, pop.eka[, 1:16], sort = F)
head(s1a)
head(s1a[head(colnames(s1a))])


### Select on H_ID, P_ID
s1b <- pop.eka[paste(pop.eka[, "H_ID"], pop.eka[, "P_ID"]) %in% paste(s1$H_ID, s1$P_ID), ]
head(s1)
head(s1b)


### Select on casenum
s1$casenum
sort(s1$casenum)
s1c <- pop.eka[s1$casenum, ]
head(s1)
head(s1c)


### Select on casenum and week

head(s1)
s1$casenum
s1$.week
s1$.week + 3

head(pop.eka)

s1d <- pop.eka[s1$casenum, s1$.week + 3]
s1d

s1e <- diag(s1d)

cbind(s1, s1e)


dim(pop.eka)
dim(2)

extr.data <- function(x, rows, cols, col.skip = 0, var.name = "var") {
  
  if (is.null(dim(x))) stop("x is not a two dimensional object")
  
  N <- nrow(x)
  M <- ncol(x)
  
  rows <- as.integer(rows)
  cols <- as.integer(cols)
  col.skip <- as.integer(col.skip)[1]
  var.name <- as.character(var.name)[1]
  
  if (min(rows) < 1) stop("wrong id for rows")
  if (max(rows) > N) stop("wrong id for rows")
  if (min(cols) < 1) stop("wrong id for cols")
  if (max(cols) > M) stop("wrong id for cols")

  if (length(rows) != length(cols)) stop("rows and cols has to be the same length")
  
  d2 <- diag(x[rows, cols + col.skip])
  
  if (col.skip > 0) {
    d1 <- data.frame(x[rows, 1:col.skip])
    d <- data.frame(d1, d2)
  } else {
    d <- data.frame(d2)
  }
  
  colnames(d)[col.skip + 1] <- var.name
  
  return(d)
}

extr.data(pop.eka, s1$casenum, s1$.week, 0, "eka")
extr.data(pop.eka, s1$casenum, s1$.week, 3, "eka")

extr.data(pop.eka, s2$casenum, s2$.week, 3, "eka")


tmp <- matrix(rep(1:10, each = 10), 10, 10)
tmp

extr.data(tmp, 10:1, 8:3, 0, "eka")

ed <- extr.data(pop.eka, 1:1e3, rep(1:13, length.out = 1e3), 3, "eka")
head(ed)
nrow(ed)

ed <- extr.data(pop.eka, 1:1e4, rep(1:13, length.out = 1e4), 3, "eka")
head(ed)
nrow(ed)

ed <- extr.data(pop.eka, rep(5, 1e3), rep(5, 1e3), 3, "eka")
head(ed)
nrow(ed)

ed <- extr.data(pop.eka, sort(sample(1:1e6, 1e3, replace = T)), rep(5, 1e3), 3, "eka")
head(ed)
nrow(ed)
