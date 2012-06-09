### try

try(log(1))
t <- try(log("a"), T)
t
t[[1]]
class(t[[1]])
dim(t[[1]])
length(t[[1]])

attr(t, "class")
attr(t, "condition")


cat


sum2 <- function(a, ...) sum(a, ...)

sum2(3, 4, 5, 6)