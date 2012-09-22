### Function for plots

### Density plotting

### Ver01

# gr.den <- function(data, x, by) {
#   
#   x <- as.character(x)[1]
#   by <- as.character(by)[1]
#   
#   data <- data[!is.na(data[, x]), ]
#   
#   ggplot(data, aes_string(x = x, fill = by)) +
#     geom_density(alpha = 0.2) +
#     #geom_vline(xintercept = tv[, x], color="red", size=.5) +
#     xlab(x)
# }


### Ver02


d <- data.frame(a = rep(1:3, each = 5), a2 = rep(4:6, each = 5), b = 1:15)

agg <- aggregate(d$b, list(d$a, d$a2), mean)
agg
dim(agg)
class(agg)
agg[3]^2

c("a", "b", NULL)

gr.den <- function(x, data, tv = NULL,
                   fill = NULL, linetype = NULL, color = NULL, size = NULL,
                   adjust = 1,
                   title = "Plot") {
  
  x <- as.character(x)[1]
  
  data <- data[!is.na(data[, x]), ]
  
  agg <- aggregate(data[x], data[c(fill, linetype, color, size)], mean)
  print(agg)
  
  ggplot(data,
    aes_string(x = x, fill = fill, linetype = linetype, color = color, size = size)) +
    geom_density(alpha = 0.2, adjust = adjust) +
    xlab(x) + 
    theme_bw() + 
    opts(title = title) +
    geom_vline(data = agg, aes_string(xintercept = x), linetype = "dashed", size = .5) +
    if (!is.null(tv)) geom_vline(data = tv, aes_string(xintercept = x), color = "red", size = .5)
}

