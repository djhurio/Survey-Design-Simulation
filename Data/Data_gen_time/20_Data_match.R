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




### Data

setwd(dir.data.out)

pop.eka <- attach.big.matrix("pop.eka.desc")
head(pop.eka)

frame.p <- attach.big.matrix("frame.p.desc")
head(frame.p)

head(colnames(pop.eka))
colnames(frame.p)

all(pop.eka[,"t0"] == frame.p[, "eka"])

n <- nrow(frame.p)
m <- ncol(frame.p) + 13
ynames <- c(colnames(frame.p), paste("eka", 1:13, sep = ""))
ynames

length(ynames)

### Match

frame.p.eka <- filebacked.big.matrix(nrow = n, ncol = m,
  dimnames = list(NULL, ynames),
  backingfile = "frame.p.eka.bin", descriptorfile = "frame.p.eka.desc")

frame.p.eka[, 1:ncol(frame.p)] <- frame.p[,]
frame.p.eka[, (ncol(frame.p)+1):m] <- pop.eka[,1:13+1]
head(frame.p.eka)



### Test

setwd(dir.data.out)

frame.p <- attach.big.matrix("frame.p.eka.desc")
head(frame.p)
