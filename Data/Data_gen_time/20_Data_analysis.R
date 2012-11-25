########################################
### Population data changing in time ###
########################################


### Libs

require(reshape2)
require(ggplot2)
require(foreach)
require(bigmemory)
require(bigtabulate)
# require(xlsx)

# citation("ggplot2")


### Reset ###
setwd(projwd)
rm(list = ls())
gc()
source(".Rprofile")


### Options
pdf.options(width = 8, height = 6, reset = T)


### Dir

data.tmp <- "~/temp/pop"
proj.data <- paste(projwd, "Data", sep = "/")
dir.res <- paste(projwd, "Results", "DynPop", sep = "/")



### Data

setwd(proj.data)
load("TM.Rdata")
TM


mpower <- function(m, p) {
  y <- eigen(m)
  return(y$vectors %*% diag(y$values)^p %*% solve(y$vectors))
}



TM.list <- split(TM[, c(3:5)], TM$per)
TM.list[[1]]
class(TM.list[[1]])
TM.list[[2]]
length(TM.list)

TM.list[[1]]

v <- eigen(TM.list[[1]])$vectors[1, ]
v
k <- (1-sum(v)) / 3
v+k
sum(v+k)


TM.list[[1]]
mpower(TM.list[[1]], 10000)[1,]
mpower(TM.list[[2]], 10000)[1,]
mpower(TM.list[[3]], 10000)[1,]
mpower(TM.list[[4]], 10000)[1,]

TM.y <- as.matrix(TM.list[[1]]) %*% as.matrix(TM.list[[2]]) %*%
  as.matrix(TM.list[[3]]) %*% as.matrix(TM.list[[4]])

TM.y
mpower(TM.y, 10000)[1,]


stat.distr <- cbind(mpower(TM.list[[1]], 10000)[1,],
                    mpower(TM.list[[2]], 10000)[1,], 
                    mpower(TM.list[[3]], 10000)[1,],
                    mpower(TM.list[[4]], 10000)[1,])

stat.distr

# rep(stat.distr[1, ], times=3, each=13)
# plot(rep(stat.distr[1, ], times=3, each=13))

###

setwd(dir.data)

# load("frame.p.Rdata")
frame.p.df <- attach.big.matrix("frame.p.desc")

head(frame.p.df)
# test.df(frame.p.df)

nrow(frame.p.df)

# bigtable(frame.p.df, "eka")
# prop.table(bigtable(frame.p.df, "eka"))

eka.distr <- as.numeric(prop.table(bigtable(frame.p.df, "eka")))
eka.distr


rm(frame.p.df)
gc()

###

# setwd(data.tmp)

popg <- attach.big.matrix("pop.eka.desc")
# popg <- attach.big.matrix("file1.desc")
class(popg)
head(popg)



###

# write.table(stat.distr, file = "stat.distr.txt", row.names = F)




###

col.distr <- function(x, skip = 0) {
  m <- ncol(x) - skip
  
  distr <- function(i, x) {
    as.numeric(prop.table(bigtable(x, i)))
  }
  
  data.frame(sapply(skip + 1:m, distr, x = x))
}




###

# distr <- data.frame(sapply(popg, function(x) prop.table(table(x))))
distr <- col.distr(popg, skip = 4)
class(distr)
distr
round(distr, 3)

distr$eka <- as.character(1:3)
distr[, 1:10]

distr.melt <- melt(distr, "eka")
head(distr.melt)

distr.melt$w <- as.integer(substring(as.character(distr.melt$variable), 2)) - 1L
head(distr.melt)
tail(distr.melt)


stat.distr

a.stat.distr <- rowMeans(stat.distr)
a.stat.distr

rep(stat.distr[1, ], times=6, each=13)

Y <- 6
K <- Y*52

stat.distr.p <- data.frame(w = rep(1:K, 3),
                           eka = as.character(rep(1:3, each = K)),
                           p = c(rep(stat.distr[1, ], times = Y, each = 13),
                                 rep(stat.distr[2, ], times = Y, each = 13),
                                 rep(stat.distr[3, ], times = Y, each = 13)),
                           stringsAsFactors = F)
head(stat.distr.p)

### Draw plots

setwd(dir.res)

head(stat.distr.p)
head(distr.melt)

tail(stat.distr.p)
tail(distr.melt)

dim(stat.distr.p)
dim(distr.melt)

class(distr.melt)

intersect(names(distr.melt), names(stat.distr.p))
data.p <- merge(distr.melt, stat.distr.p, all = T)
data.p$variable <- NULL

head(data.p)
tail(data.p)
sum(is.na(data.p))

# plot.eka <- function(x) {
#   qplot(data = stat.distr.p[stat.distr.p$eka %in% x, ], x = time, y = p,
#     geom="step", colour = eka, linetype = "dotted") +
#   geom_line(data = distr.melt[distr.melt$eka %in% x, ],
#     aes(x = time, y = value, color = eka), linetype = "solid") +
#   geom_hline(yintercept = eka.distr[x], linetype = "dashed") + 
#   geom_hline(yintercept = a.stat.distr[x], linetype = "dashed",
#     colour = "orange") + 
#   scale_x_continuous(breaks = seq(0, K, by = 26)) +
#   theme_bw()
# }

plot.eka <- function(x) {
  ggplot(data = data.p[data.p$eka %in% x, ], aes(x = w)) +
  geom_line(aes(y = value, colour = eka), linetype = 1) +
  geom_step(aes(y = p, colour = eka), linetype = 2) +
  geom_hline(yintercept = eka.distr[x], linetype = 3, colour = x) + 
  geom_hline(yintercept = a.stat.distr[x], linetype = 4, colour = x) + 
  scale_x_continuous(breaks = seq(0, K, by = 26)) +
  theme_bw() + scale_colour_identity(guide = "legend")
}

plot.eka(1:3)
plot.eka(1)
plot.eka(2)
plot.eka(3)
plot.eka(c(1,3))

pdf("pop_gen_plot.pdf")
  plot.eka(1:3)
dev.off()


eka.distr
stat.distr
a.stat.distr

res <- cbind(eka.distr, stat.distr, a.stat.distr)
colnames(res) <- c("state.0", paste("stat.state", 1:4, sep = "."), "stat.state.ave")
res

TM

# write.table(res, file = "state.distr.txt", row.names = F)
# write.table(TM, file = "state.distr.txt", row.names = F)

# write.xlsx(res, "res.xlsx", "state.distr")
# write.xlsx(TM, "res.xlsx", "trans.matrix", append = T)


