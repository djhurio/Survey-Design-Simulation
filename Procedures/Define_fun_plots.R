### Function for plots
### Density plotting

### Ver03
# Fixes regarding changes in ggplot2
# See ggplot2 0.9.2 at http://cran.r-project.org/web/packages/ggplot2/NEWS
# Default title is empty
# Added testing if x is in tv even tv is defined

gr.den <- function(x, data, tv = NULL,
                   fill = NULL, linetype = NULL, colour = NULL, size = NULL,
                   adjust = 1,
                   title = "") {
  
  require(ggplot2)
  
  x <- as.character(x)[1]
  
  data <- data[!is.na(data[, x]), ]
  
  agg <- aggregate(data[x], data[c(fill, linetype, colour, size)], mean)
  
  ggplot(data,
    aes_string(x = x, fill = fill, linetype = linetype, colour = colour,
               size = size)) +
    geom_density(alpha = 0.2, adjust = adjust) +
    xlab(x) + 
    theme_bw() + 
    labs(title = title) +
    geom_vline(data = agg,
               aes_string(xintercept = x), linetype = "dashed", size = .5) +
    if (!is.null(tv) & x %in% names(tv)) {
      geom_vline(data = tv, aes_string(xintercept = x),
                 colour = "red", size = .5)
    }
}
