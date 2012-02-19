##################
### Clustering ###
##################

head(pop.h)

table(pop.h$nov)

plot.clusters(pop.h[pop.h$strata == 1, "coord_x_p"], 
              pop.h[pop.h$strata == 1, "coord_y_p"],
              pop.h[pop.h$strata == 1, "iec2010"])


y <- pop.h[pop.h$strata == 1 , c("coord_x_p", "coord_y_p")]
y <- pop.h[pop.h$raj == 5 , c("coord_x_p", "coord_y_p")]
y <- pop.h[pop.h$nov == 9647 , c("coord_x_p", "coord_y_p")]
# y <- y[sample(1:nrow(y), 1000), ]
nrow(y)
head(y)


# figure out a good number of clusters use a range of 2 to 10 clusters, with 20 reps each
# s <- stepFlexclust(y, k=seq(20, 200, 20), nrep=10, multicore = T)
# plot(s)


n.clust <- 1100


# y.pam <- pam(y, n.clust, stand=TRUE)
y.pam <- clara(y, n.clust, samples = 5)

class(y.pam)
print(y.pam)

head(y.pam$sample)
head(y.pam$medoids)
head(y.pam$i.med)
head(y.pam$clustering)
head(y.pam$objective)
head(y.pam$clusinfo)
head(y.pam$diss)
y.pam$call
head(y.pam$data)

# plot(y.pam)

# add the clustering vector back to the original dataframe
y$cluster <- y.pam$clustering

# plot the clusters by color
plot(y$coord_x_p,
     y$coord_y_p,
     col = rainbow(n.clust, alpha = 1)[y$cluster],
     cex = 0.5,
     pch = 16,
     asp = 1)

# add the medoids, they are in the same order as the clustering vector
points(y.pam$medoids,
       pch = 15,
       col = rainbow(n.clust, alpha = 1),
       cex = 1.25)

# connect the original points to the centroids with line segments:
for(i in 1:n.clust) segments(x0 = y.pam$medoids[i,][1],
                             y0 = y.pam$medoids[i,][2],
                             x1 = y$coord_x_p[y$cluster == i],
                             y1 = y$coord_y_p[y$cluster ==i],
                             col = "black",
                             lty = 3)

head(y)
tail(y)

y.centre <- aggregate(y[c("coord_x_p", "coord_y_p")], y["cluster"], mean)
head(y.centre)
names(y.centre)[2:3] <- c("coord_x_c", "coord_y_c")
names(y.centre)

y.2 <- merge(y, y.centre)
head(y.2)

y.2$dist <- sqrt((y.2$coord_x_p - y.2$coord_x_c)^2 + (y.2$coord_y_p - y.2$coord_y_c)^2)
head(y.2)

y.3 <- aggregate(y.2["dist"], y.2["cluster"], max)
head(y.3)

plot(y.centre$coord_x_c,
     y.centre$coord_y_c,
     pch = 20,
     cex = .01,
     asp = 1)
for (i in 1:nrow(y.3)) draw.circle(y.centre$coord_x_c[i],
                                   y.centre$coord_y_c[i],
                                   y.3$dist[i],
                                   border = "red")







aggregate(rep(1, nrow(y)), y["cluster"], sum)

