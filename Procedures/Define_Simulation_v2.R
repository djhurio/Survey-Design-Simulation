### Function: Simulation

### Ver02
# Redesign of function
# Single core

### Ver03
# Multi core

### Ver04
# Nested loop

### Ver05
# Option for seed - on/off

### Ver06
# Arguments for a function are provided as data.frame
# Default log to F

### Ver06
# Arguments for a function are provided as data.frame
# Default log to F

### Ver07
# Arguments added to the output of the results

### Ver08
# arg defined as list

Sim <- function(fun, arg, I = 5,
                name = "res",
                print = F,
                log = F,
                seed = NA,
                cores = multicore:::detectCores()) {
  
  require(foreach)
  require(doMC)
  
  registerDoMC(cores = cores)
  
  # Argument type convertion
  fun <- as.character(fun)[1]
  arg <- as.list(arg)
  I <- as.integer(I)[1]
  name <- as.character(name)[1]
  print <- as.logical(print)[1]
  log <- as.logical(log)[1]
  
  # Testing
  if (I<=0) stop("I has to be 1 or larger")
  if (!is.na(seed) & length(seed) != I) stop("Wrong length of seed")
  
  # Function to convert arg as one row data.frame
  transf <- function(x) data.frame(lapply(x, function(x) t(data.frame(x))))
  
  # Test run
  test <- do.call(fun, arg[[1]])
  m1 <- length(test)
  
  arg2 <- transf(arg[[1]])
  m2 <- ncol(arg2)
  
  cat("Simulation name:", name, "\n")
  cat("Number of iterations:", I, "\n")
  cat("Number of cores:", cores, "\n")
  
  filename <- name
  
  t1 <- Sys.time()
  
  R <- foreach(a = 1L:length(arg), .combine = rbind, .inorder = F) %:%
    foreach(i = 1L:I, .combine = rbind, .inorder = F) %dopar% {
      if (log) cat(as.character(Sys.time()), ", ",
                   i, " of ", I, ", ", round(i/I*100, 1), "%\n",
                   file = paste(filename, ".log", sep = ""),
                   sep = "", append = T)
      
      if (!is.na(seed)) set.seed(seed[i])
      
      tr <- try(do.call(fun, arg[[a]]), T)

      res <- data.frame(t1, name, a, i, seed[i], transf(arg[[a]]))
      rownames(res) <- NULL
      
      if (class(tr) == "try-error")
        res <- data.frame(res, tr[[1]], matrix(NA, 1, m1)) else
        res <- data.frame(res, NA, tr)
      
      colnames(res) <- paste("v", 1:(5 + m2 + 1 + m1), sep = "")
      
      res
    }

  t2 <- Sys.time()
  time.run <- as.numeric(t2 - t1, units="secs")
  
  colnames(R) <- make.names(c("timestamp", "name", "a", "i", "seed",
                   colnames(arg2), "err", colnames(test)), unique = T)
  rownames(R) <- NULL
  
  assign(name, R)
  save(list = name, file = paste(filename, ".Rdata", sep=""))
  
  cat("Total time:", time.run, "sec\n")
  cat("Average time:", time.run / I, "sec\n")
  
  if (print) {
    cat("Rows in results:", nrow(R), "rows", "\n")
    cat("First results:", "\n")
    print(head(R))
  }
  
  return(list(R, time.run))
}
