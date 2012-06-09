as.integer(2*10^9)
as.integer(2*10^10)

seed <- runif(1, 0, 2*10^9)
set.seed(seed)



## the default random seed is 626 integers, so only print a few
runif(1)
.Random.seed[1:6]
runif(1)
.Random.seed[1:6]
## If there is no seed, a "random" new one is created:
rm(.Random.seed)
runif(1)
.Random.seed[1:6]


.Random.seed[2]
.Random.seed[.Random.seed[2] + 2]


set.seed(.Random.seed[.Random.seed[2] + 3])
runif(1)

rm(.Random.seed)
runif(1)
.Random.seed[1:6]

set.seed(11)
.Random.seed[1:6]

set.seed(2)
.Random.seed[1:6]


?sink


write(NULL, "log.txt")
a <- foreach(i = 1:10, .combine = c) %dopar% {
  write(i, "log.txt", append = T)
  i
}

a

message(4)


cat("a", "b", "c", sep = "")

cat(as.character(Sys.time()))


set.seed(1); a <- runif(10)
.Random.seed[1:6]
