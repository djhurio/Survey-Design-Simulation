############# Presentation of Self-rotating Sampling Design

setwd(dir.res)

### Settings
pdf.options(family = "Times")

# library(plotrix)
library(car)
library(ggplot2)

1/2/pi

circleFun <- function(center = c(0,0),diameter = 1/pi, npoints = 100){
    r = diameter / 2
    tt <- seq(0,2*pi,length.out = npoints)
    xx <- center[1] + r * cos(tt)
    yy <- center[2] + r * sin(tt)
    return(data.frame(x = xx, y = yy))
}

set.seed(20100413)
xi=runif(1)
xi


delta=.002

n_v=8
n_w=13



data=matrix(nrow=n_i*n_j*2, ncol=3)

data[, 1] <- rep(1:n_w, each = 2 * n_v)
data[, 2] <- rep(c(0, 9), each = n_v, times = n_w)
data[, 3] <- rep(1:n_v, times = 2 * n_w)
head(data)
tail(data)

data=as.data.frame(data)
head(data)

names(data)=c("w","b","v")

q <- 0
Delta <- q / 13 + (1 + delta) / 8 / 13

data$a=(xi+(data$b/16 + (data$v-1)/8) * (1+delta) + (data$w-1)*Delta) %% 1
data$x=sin(2*pi*data$a)/2/pi
data$y=cos(2*pi*data$a)/2/pi

data$r <- ifelse(data$v %in% c(1,2,5,6), 1, 0)
data$r <- ifelse(data$b == 0, data$r, 2+2*data$r)
table(data$r)

# data$w=recode(data$i, "c(1,3)=1; c(2,4)=2; c(5,7)=3; c(6,8)=4")

head(data)

# The First Octagon

pl1 <- ggplot(data[data$w == 1 & data$b == 0, ], aes(x, y)) +
  geom_point(aes(shape = r), size = 5) +
  geom_path(aes(x, y), data = circleFun(), colour = "gray") +
  geom_text(aes(x, y, label = v), hjust = -1, vjust = -1) +
  scale_shape_identity() +
  theme_bw(base_size = 24) + theme(legend.position="none") + coord_fixed() +
  theme(axis.title = element_blank())
pl1

# The First and The Second Octagon

pl2 <- ggplot(data[data$w == 1, ], aes(x, y)) +
  geom_point(aes(shape = r), size = 5) +
  geom_path(aes(x, y), data = circleFun(), colour = "gray") +
  geom_text(aes(x, y, label = v), hjust = -1, vjust = -1) +
  scale_shape_identity() +
  theme_bw(base_size = 24) + theme(legend.position="none") + coord_fixed() +
  theme(axis.title = element_blank())
pl2

# The Sample of Two Weeks

pl3 <- ggplot(data[data$w %in% 1:2 & data$b == 0, ], aes(x, y)) +
  geom_point(aes(shape = v), size = 3) +
  geom_path(aes(x, y), data = circleFun(), colour = "gray") +
  scale_shape_identity() +
  theme_bw(base_size = 24) + theme(legend.position="none") + coord_fixed() +
  theme(axis.title = element_blank())
pl3

# The Sample of Three Weeks

pl4 <- ggplot(data[data$w %in% 1:3 & data$b == 0, ], aes(x, y)) +
  geom_point(aes(shape = v), size = 3) +
  geom_path(aes(x, y), data = circleFun(), colour = "gray") +
  scale_shape_identity() +
  theme_bw(base_size = 24) + theme(legend.position="none") + coord_fixed() +
  theme(axis.title = element_blank())
pl4

# The Sample of Thirteen Weeks

pl5 <- ggplot(data[data$w %in% 1:13 & data$b == 0, ], aes(x, y)) +
  geom_point(aes(shape = v), size = 3) +
  geom_path(aes(x, y), data = circleFun(), colour = "gray") +
  scale_shape_identity() +
  theme_bw(base_size = 24) + theme(legend.position="none") + coord_fixed() +
  theme(axis.title = element_blank())
pl5

mapply(ggsave, filename = paste0("Rotation", 1:5, ".pdf"),
       plot = list(pl1, pl2, pl3, pl4, pl5))
