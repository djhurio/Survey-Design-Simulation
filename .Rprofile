### The .Rprofile file in the project's main directory (if any) is sourced by R
### http://rstudio.org/docs/using/projects

# Get the working directory of the project
projwd <- getwd()

dir.proc <- paste(projwd, "Procedures", sep = "/")

if (.Platform$OS.type == "windows") {
  dir.data.source <- "T:/!Vadiba/MatNodrD/Prakses/Data"
  dir.data <- "C:/DATA/SDS/data"
  dir.res <- "C:/DATA/SDS/res"
  dir.tmp <- "C:/DATA/SDS/sim"
} else {
  dir.data.source <- "~/DATA/LU/Work"
  dir.data <- "~/temp/data"
  dir.res <- "~/temp/res"
  dir.tmp <- "~/temp/sim"
}


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
