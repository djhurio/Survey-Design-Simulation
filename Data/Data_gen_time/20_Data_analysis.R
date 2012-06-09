########################################
### Population data changing in time ###
########################################


### Libs

require(reshape2)
require(ggplot2)
require(foreach)
require(bigmemory)
require(bigtabulate)
require(xlsx)

# citation("ggplot2")


### Reset ###
setwd(projwd)
rm(list = ls())
gc()
source(".Rprofile")



### Dir

data.tmp <- "~/temp/pop"



### Data

setwd(projwd)
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

TM.y <- as.matrix(TM.list[[1]]) %*% as.matrix(TM.list[[2]]) %*% as.matrix(TM.list[[3]]) %*% as.matrix(TM.list[[4]])

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

setwd(dir.data.out)

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

col.distr <- function(x) {
  m <- ncol(x)
  
  distr <- function(i, x) {
    as.numeric(prop.table(bigtable(x, i)))
  }
  
  data.frame(sapply(1:m, distr, x = x))
}




###

# distr <- data.frame(sapply(popg, function(x) prop.table(table(x))))
distr <- col.distr(popg)
class(distr)
distr
round(distr, 3)

distr$eka <- as.character(1:3)
distr[, 1:10]

distr.melt <- melt(distr, "eka")
distr.melt$time <- as.integer(substring(as.character(distr.melt$variable), 2)) - 1
head(distr.melt)
tail(distr.melt)


stat.distr

a.stat.distr <- rowMeans(stat.distr)
a.stat.distr

rep(stat.distr[1, ], times=6, each=13)

Y <- 6
K <- Y*52

stat.distr.p <- data.frame(time = rep(1:K-1, 3),
                           eka = as.character(rep(1:3, each = K)),
                           p = c(rep(stat.distr[1, ], times = Y, each = 13),
                                 rep(stat.distr[2, ], times = Y, each = 13),
                                 rep(stat.distr[3, ], times = Y, each = 13)))
head(stat.distr.p)


# Temp - one transition matrix
# stat.distr.p <- data.frame(time = rep(1:K, 3),
#                            eka = as.character(rep(1:3, each = K)),
#                            p = c(rep(stat.distr[1, 1], K),
#                                  rep(stat.distr[2, 1], K),
#                                  rep(stat.distr[3, 1], K)))
# head(stat.distr.p)
# tail(stat.distr.p)
# K*3

# p1 <- ggplot(stat.distr.p, aes(x = time, y = p, group = eka))
# p1 + geom_line(aes(color = eka))

# qplot(time, p, data = stat.distr.p, geom="step", colour = eka, linetype = 2) +
#   scale_x_continuous(breaks = 0:12*13) +
#   theme_bw()

# p <- ggplot(distr.melt, aes(x = time, y = value, group = eka))
# p + geom_line(aes(color = eka)) +
#     geom_hline(aes(yintercept = eka.distr, linetype = 2))

# p + geom_line(aes(color = eka)) +
#     geom_line(data = stat.distr.p, aes(x = time, y = p, group = eka, color = eka), linetype = 2) +
#     geom_hline(aes(yintercept = eka.distr, linetype = 2)) + 
#     scale_x_continuous(breaks = 0:12*13) +
#     theme_bw()

# p + geom_line(aes(color = eka)) +
#     geom_hline(aes(yintercept = eka.distr, linetype = 2)) + 
#     scale_x_continuous(breaks = 0:12*13) +
#     theme_bw()

# qplot(data = stat.distr.p, x = time, y = p, geom="step", colour = eka, linetype = 2) +
#   geom_line(data = distr.melt, aes(x = time, y = value, color = eka), linetype = 1) +
#   geom_hline(aes(yintercept = eka.distr), linetype = 2) + 
#   scale_x_continuous(breaks = 0:12*13) +
#   theme_bw()

# a.stat.distr

plot.eka <- function(x) {
  qplot(data = stat.distr.p[stat.distr.p$eka %in% x, ], x = time, y = p,
        geom="step", colour = eka, linetype = 3) +
    geom_line(data = distr.melt[distr.melt$eka %in% x, ],
              aes(x = time, y = value, color = eka), linetype = "solid") +
    geom_hline(aes(yintercept = eka.distr[x]), linetype = "dashed") + 
    geom_hline(aes(yintercept = a.stat.distr[x]), linetype = "dashed", color = "orange") + 
    scale_x_continuous(breaks = seq(0, K, by = 26)) +
    theme_bw()
}

plot.eka(1:3)
plot.eka(1)
plot.eka(2)
plot.eka(3)
plot.eka(c(1,3))

pdf.options()

pdf("pop_gen_plot.pdf", paper = "a4r")
# pdf("pop_gen_plot_oneTM.pdf")
plot.eka(1:3)
plot.eka(1)
plot.eka(2)
plot.eka(3)
plot.eka(c(1,3))
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

write.xlsx(res, "res.xlsx", "state.distr")
write.xlsx(TM, "res.xlsx", "trans.matrix", append = T)


