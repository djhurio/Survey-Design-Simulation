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

gr.den <- function(x, data, tv = NULL,
                   fill = NULL, linetype = NULL, color = NULL,
                   adjust = 1,
                   title = "Plot") {
  
  x <- as.character(x)[1]
  
  data <- data[!is.na(data[, x]), ]
  
  ggplot(data,
    aes_string(x = x, fill = fill, linetype = linetype, color = color)) +
    geom_density(alpha = 0.2, adjust = adjust) +
    geom_vline(xintercept = tv[1, x], color = "red", size = .5) +
    xlab(x) + 
    theme_bw() + 
    opts(title = title)
}

