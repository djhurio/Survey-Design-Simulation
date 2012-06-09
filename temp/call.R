runif(1)

eval(runif(1))


run <- function(n = 1) n^2


sim <- function(proc, I = 5) {
  for (i in 1:I) print(call(proc))
}


sim(expression(run()), 6)


match.fun(run)


lapply

do.call(run, list(1:5))

as.list(1:5)

do.call(run, list(1:5))

df
length(df)