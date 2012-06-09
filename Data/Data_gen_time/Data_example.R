### Data

dat <- as.data.frame(matrix(round(runif(50)*5), 10, 5))
colnames(dat) <- paste("y", 1:5, sep="")

dat <- data.frame(id = 1:10, dat)
dat


sam <- data.frame(id = sort(sample(1:10, 5)), week = sample(1:5, 5, replace = T))
sam

###

structure(dat)
str(dat)
dput(dat)



###

dat
sam

res <- data.frame(id = sam$id)
res$y <- c(dat[sam$id[1], sam$week[1]+1],
  dat[sam$id[2], sam$week[2]+1],
  dat[sam$id[3], sam$week[3]+1],
  dat[sam$id[4], sam$week[4]+1],
  dat[sam$id[5], sam$week[5]+1])

res

dat
cbind(sam, res)


# setwd("/home/djhurio/temp")
# 
# dput(dat, "dat.txt")
# dput(sam, "sam.txt")
# dput(res, "res.txt")

data.frame(id = sam[,1], y = dat[cbind(sam[,1],(1+sam[,2]))])

dat

dat[1:3, 1:3]
dat[cbind(1:3, 1:3)]


a <- matrix(1:16, 4, 4)
a

a[cbind(1:2, 1:2)]

as.data.frame