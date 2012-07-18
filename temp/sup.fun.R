### Dev area

sup.fun <- function(a, b) a ** b

sup.fun(1, 4)
sup.fun(3, 5)

arg.frame <- data.frame(a = 1:3, b = 3:5)
arg.frame

do.call(sup.fun, arg.frame[1, ])
do.call(sup.fun, arg.frame[2, ])
do.call(sup.fun, arg.frame[3, ])

arg.frame <- data.frame(b = 3:5, a = 1:3)
arg.frame

do.call(sup.fun, arg.frame[1, ])
do.call(sup.fun, arg.frame[2, ])
do.call(sup.fun, arg.frame[3, ])


arg.frame <- data.frame(b = 3:5, a = 1:3, c = 5)
arg.frame

do.call(sup.fun, arg.frame[1, ])
do.call(sup.fun, arg.frame[2, ])
do.call(sup.fun, arg.frame[3, ])


class(arg.frame[1, ])
as.list(arg.frame[1, ])