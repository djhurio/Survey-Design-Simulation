### The .Rprofile file in the project's main directory (if any) is sourced by R
### http://rstudio.org/docs/using/projects

# Get the working directory of the project
projwd <- getwd()

dir.data <- "~/DATA/LU/Work"
dir.proc <- paste(projwd, "Procedures", sep = "/")
dir.res <- paste(projwd, "Results/temp", sep = "/")
dir.tmp <- "~/temp/sim"


test.df <- function(x) {
  print(head(x))
  print(sapply(x, function(y) sum(is.na(y))))
}


## If you want to source() a bunch of files, something like
## the following may be useful:
sourceDir <- function(path, trace = TRUE, ...) {
  for (nm in list.files(path, pattern = "\\.[RrSsQq]$")) {
    if(trace) cat(nm,":")           
    source(file.path(path, nm), ...)
    if(trace) cat("\n")
  }
}
