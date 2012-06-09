x <- as.big.matrix(matrix(1:30, 10, 3))

x[,]

x[1:3, 1:3, drop = F]

x[1, , drop = F]

as.big.matrix(cbind(1:3, 1:3))

x[cbind(1:3, 1:3)]
x[as.big.matrix(cbind(1:3, 1:3))[,]]


x[,]
diag(x[1:3, 1:3])
diag(x[1:3, 3:1])



m <- matrix(1:6, nrow=2, dimnames=list(c("a", "b"), LETTERS[1:3]))
m[cbind(c(1,2,1),3:1)]# matrix numeric index
ci <- cbind(c("a", "b", "a"), c("A", "C", "B"))
m[ci]                 # matrix character index
