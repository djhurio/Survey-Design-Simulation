### Draft - plotting trips

ggplot(r1[[2]], aes(x, y)) +
  geom_point(size = 1, color = "red") +
  geom_point(data = r1[[2]][nrow(r1[[2]]), ], size = 2, color = "black") +
  geom_path(color = "blue")

ggplot(r1[[2]], aes(x, y)) +
  geom_point(size = 1, color = "red") +
  geom_point(data = r1[[2]][nrow(r1[[2]]), ], size = 2, color = "black") +
  geom_path(color = "blue")

ggplot() +
  geom_point(data = r1[[2]], aes(x, y), size = 1, color = "red") +
  geom_point(data = r1[[2]][nrow(r1[[2]]), ], aes(x, y), size = 2, color = "black") +
  geom_path(data = r1[[2]], aes(x, y), color = "blue")


ggplot() +
  geom_point(data = r1[[2]], aes(x, y), size = 1, color = "red") +
  geom_point(data = r1[[2]][nrow(r1[[2]]), ], aes(x, y), size = 2, color = "black") +
  geom_path(data = r1[[2]], aes(x, y), color = "blue") +
  geom_point(data = r2[[2]], aes(x, y), size = 1, color = "red") +
  geom_point(data = r2[[2]][nrow(r2[[2]]), ], aes(x, y), size = 2, color = "black") +
  geom_path(data = r2[[2]], aes(x, y), color = "blue")






x <- s[c("coord_x_p", "coord_y_p")]
head(x)

y <- split(x, s$int.ID)
class(y)

y[[1]]

sort(unique(s$int.ID))

frame.int[sort(unique(s$int.ID)), 3:4]

y[["1"]]
