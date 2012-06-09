### TSP testing

library("TSP")
data("USCA50")
USCA50

head(USCA50)

methods <- c("nearest_insertion", "farthest_insertion", "cheapest_insertion", "arbitrary_insertion", "nn", "repetitive_nn", "2-opt")

tours <- sapply(methods, FUN = function(m) solve_TSP(USCA50, method = m), simplify = FALSE)
tours[[1]]

dotchart(c(sapply(tours, FUN = attr, "tour_length"), optimal = 14497), xlab = "tour length", xlim = c(0, 20000))


library("TSP")
data("USCA312")
tsp <- insert_dummy(USCA312, label = "cut")
tsp


tour <- solve_TSP(tsp, method = "farthest_insertion")
tour


path <- cut_tour(tour, "cut")
head(labels(path))
tail(labels(path))


library("maps")
library("sp")
library("maptools")


data("USCA312_map")

head(USCA312_coords)

str(USCA312_basemap)



plot_path <- function(path) {
  plot(as(USCA312_coords, "Spatial"), axes = TRUE)
  plot(USCA312_basemap, add = TRUE, col = "gray")
  points(USCA312_coords, pch = 3, cex = 0.4, col = "red")
  path_line <- SpatialLines(list(Lines(list(Line(USCA312_coords[path,])), ID = "1")))
  plot(path_line, add = TRUE, col = "black")
  points(USCA312_coords[c(head(path, 1), tail(path, 1)),], pch = 19, col = "black")
}

plot_path(path)



data("iris")
head(iris)

?dist
d <- dist(iris[-5])
head(d)



a <- data.frame("x" = c(0, 0, 0, 1, 1, 1), "y" = c(0, 1, 2, 0, 1, 2))
a
plot(a$x, a$y, asp = 1)

d <- dist(a)
d
class(d)
str(d)
attr(d, "Size")




## create a TSP
tsp <- TSP(d, labels = LETTERS[1:attr(d, "Size")])
tsp
## use some methods
n_of_cities(tsp)
labels(tsp)

image(tsp)

t <- solve_TSP(tsp, control = list(start=1L))
t
as.integer(t)

str(t)

attr(t, "tour_length")
tour_length(tsp)
4+2*sqrt(5)

image(tsp)
image(tsp, order = t)

cut_tour(t, "A")
t <- cut_tour(t, "C")

plot(a, axes = TRUE)
points(a, pch = 3, cex = 0.4, col = "red")
path_line <- SpatialLines(list(Lines(list(Line(a[t,])), ID = "1")))
plot(path_line, add = TRUE, col = "black")
points(a[c(head(t, 1), tail(t, 1)),], pch = 19, col = "black")





### Map of Latvia

require(mapdata)

map('worldHires', col=1:10)
map('world2Hires', col=1:10)

map('worldHires', 'Switzerland')
title('Switzerland')

map('world2Hires', 'Switzerland')
title('Switzerland')


map()  # low resolution map of the world
map('usa')	# national boundaries
map('county', 'new jersey')	# county map of New Jersey
map('state', region = c('new york', 'new jersey', 'penn'))	# map of three states
map("state", ".*dakota", border = 0)	# map of the dakotas
map.axes()				# show the effect of border = 0
if(require(mapproj))
  map('state', proj = 'bonne', param = 45)	# Bonne equal-area projection of states

# names of the San Juan islands in Washington state
map('county', 'washington,san', names = TRUE, plot = FALSE)

# national boundaries in one linetype, states in another
# (figure 5 in the reference)
map("state", interior = FALSE)
map("state", boundary = FALSE, lty = 2, add = TRUE)

# plot the ozone data on a base map
# (figure 4 in the reference)
data(ozone)
map("state", xlim = range(ozone$x), ylim = range(ozone$y))
text(ozone$x, ozone$y, ozone$median)
box()



###

plot(as(USCA312_coords, "Spatial"), axes = TRUE)
plot(USCA312_basemap, add = TRUE, col = "gray")
points(USCA312_coords, pch = 3, cex = 0.4, col = "red")
