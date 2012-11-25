### Function: Simulation

### Ver2.0
# Redesign of function


### Temp parameters
# proc <- expression(runif(1))
# proc <- expression(data.frame(a = runif(1), b = runif(1)))
# class(proc)
# eval(proc)
# I <- 5
# name <- "tmp"
# ret <- T
# pr <- T


# ### Single core ###
# 
# Sim.1 <- function(proc, I = 5, name = "res", ret = F, pr = F) {
# 
#   # Argument type convertion
#   
#   proc <- as.expression(proc)
#   I <- as.integer(I)[1]
#   name <- as.character(name)[1]
#   ret <- as.logical(ret)[1]
#   pr <- as.logical(pr)[1]
# 
#   # Testing
#   
#   if (I<=0) stop("I has to be 1 or larger")
# 
#   cat(paste("Simulation name:", name, "\n", sep=" "))
#   cat(paste("Number of iterations:", I, "\n", sep=" "))
#   
#   
#   # Test run
#   test <- eval(proc)
#   #test
#   #length(test)
# 
#   R <- data.frame(name = name, i = 1:I, err = 0, matrix(as.numeric(NA), I, length(test)))
#   names(R)[-(1:3)] <- names(test)
# 
#   t1 <- Sys.time()
# 
#   pb <- txtProgressBar(min = 0, max = I, style = 3)
#   for (i in 1:I) {
#     set.seed(i)
#     tr <- try(eval(proc), T)
#     if (class(tr) == "try-error") R[i, "err"] <- tr else R[i, -(1:3)] <- tr
#     setTxtProgressBar(pb, i)
#   }
#   close(pb)
# 
#   t2 <- Sys.time()
#   # time.run <- t2 - t1
#   time.run <- as.numeric(t2 - t1, units="secs")
#   
#   assign(name, R)
#   save(list = name, file = paste(name, ".Rdata", sep=""))
# 
#   # cat(paste("Total time:", time.run, "\n", sep=" "))
#   # cat(paste("Average time:", time.run / I, "\n", sep=" "))
#   cat(paste("Total time:", time.run, "\n", sep=" "))
#   cat(paste("Average time:", time.run / I, "\n", sep=" "))
#   
#   if (pr) {
#     cat(paste("Rows in results", nrow(R), "ieraksti", "\n", sep=" "))
#     cat(paste("First results:", "\n", sep=" "))
#     print(head(R))
# 	  # cat(paste("Last results:", "\n", sep=" "))
#     # print(tail(R))
#   }
# 
#   if (ret) return(time.run) else return(NULL)
# }
# 
# 
# 
# ### Multi core ###
# 
# Sim.2 <- function(proc, I = 5, name = "res", ret = F, pr = F) {
#   
#   require(foreach)
#   require(doMC)
#   
#   registerDoMC()
#   
#   # Argument type convertion
#   
#   proc <- as.expression(proc)
#   I <- as.integer(I)[1]
#   name <- as.character(name)[1]
#   ret <- as.logical(ret)[1]
#   pr <- as.logical(pr)[1]
#   
#   # Testing
#   
#   if (I<=0) stop("I has to be 1 or larger")
#   
#   cat(paste("Simulation name:", name, "\n", sep=" "))
#   cat(paste("Number of iterations:", I, "\n", sep=" "))
#   
#   # Test run
#   test <- eval(proc)
#   m <- length(test)
#   
#   t1 <- Sys.time()
#   
#   #   pb <- txtProgressBar(min = 0, max = I, style = 3)
#   #   for (i in 1:I) {
#   #     set.seed(i)
#   #     tr <- try(eval(proc), T)
#   #     if (class(tr) == "try-error") R[i, "err"] <- tr else R[i, -(1:3)] <- tr
#   #     setTxtProgressBar(pb, i)
#   #   }
#   #   close(pb)
# 
#   R <- foreach(i = 1L:I, .combine = rbind, .inorder = F) %dopar% {
#     tr <- try(eval(proc), T)
#     if (class(tr) == "try-error") data.frame(t1, name, i, tr, rep(NA, m))
#     else data.frame(t1, name, i, NA, tr)
#   }
#   
#   t2 <- Sys.time()
#   time.run <- as.numeric(t2 - t1, units="secs")
#   
#   colnames(R) <- c("time", "name", "i", "err", names(test))
#   rownames(R) <- NULL
# 
#   assign(name, R)
#   save(list = name, file = paste(name, " ", t1, ".Rdata", sep=""))
#   
#   cat(paste("Total time:", time.run, "\n", sep=" "))
#   cat(paste("Average time:", time.run / I, "\n", sep=" "))
#   
#   if (pr) {
#     cat(paste("Rows in results", nrow(R), "ieraksti", "\n", sep=" "))
#     cat(paste("First results:", "\n", sep=" "))
#     print(head(R))
#   }
#   
#   if (ret) return(time.run) else return(NULL)
# }
# 
# 
# ### Multi core ###
# 
# Sim.3 <- function(proc, I = 5, name = "res", print = F, log = T, seed = 1:I) {
#   
#   require(foreach)
#   require(doMC)
#   
#   registerDoMC()
#   
#   # Argument type convertion
#   
#   proc <- as.expression(proc)
#   I <- as.integer(I)[1]
#   name <- as.character(name)[1]
#   print <- as.logical(print)[1]
#   log <- as.logical(log)[1]
#   
#   # Testing
#   if (I<=0) stop("I has to be 1 or larger")
#   if (length(seed) != I) stop("Wrong length of seed")
#   
#   # Test run
#   test <- eval(proc)
#   m <- length(test)
#   
#   cat("Simulation name:", name, "\n")
#   cat("Number of iterations:", I, "\n")
#   
#   t1 <- Sys.time()
#   
#   filename <- paste(name, t1)
#   
#   R <- foreach(i = 1L:I, .combine = rbind, .inorder = F) %dopar% {
#     if (log) cat(as.character(Sys.time()), ", ", i, " of ", I, ", ", round(i/I*100, 1), "%\n",
#                  file = paste(filename, ".log", sep = ""), sep = "", append = T)
#     set.seed(seed[i])
#     tr <- try(eval(proc), T)
#     if (class(tr) == "try-error") res <- data.frame(t1, name, i, seed[i], tr[[1]], matrix(NA, 1, m))
#                              else res <- data.frame(t1, name, i, seed[i], NA, tr)
#     colnames(res) <- paste("v", 1:(5+m), sep = "")
#     res
#   }
#   
#   t2 <- Sys.time()
#   time.run <- as.numeric(t2 - t1, units="secs")
#   
#   colnames(R) <- c("time", "name", "i", "seed", "err", names(test))
#   rownames(R) <- NULL
#   
#   assign(name, R)
#   save(list = name, file = paste(filename, ".Rdata", sep=""))
#   
#   cat("Total time:", time.run, "\n")
#   cat("Average time:", time.run / I, "\n")
#   
#   if (print) {
#     cat("Rows in results", nrow(R), "ieraksti", "\n")
#     cat("First results:", "\n")
#     print(head(R))
#   }
#   
#   return(list(R, time.run))
# }
# 


### Ver04
### Nested loop

# Sim.4 <- function(fun, arg, I = 5,
#                   name = "res",
#                   print = F,
#                   log = T,
#                   seed = 1:I) {
#   
#   require(foreach)
#   require(doMC)
#   
#   registerDoMC()
#   
#   # Argument type convertion
#   
#   fun <- as.character(fun)[1]
#   arg <- arg
#   I <- as.integer(I)[1]
#   name <- as.character(name)[1]
#   print <- as.logical(print)[1]
#   log <- as.logical(log)[1]
#   
#   # Testing
#   if (I<=0) stop("I has to be 1 or larger")
#   if (length(seed) != I) stop("Wrong length of seed")
#   
#   # Test run
#   test <- eval(call(fun, arg[1]))
#   m <- length(test)
#   
#   cat("Simulation name:", name, "\n")
#   cat("Number of iterations:", I, "\n")
#   
#   t1 <- Sys.time()
#   
#   filename <- paste(name, t1)
#   
#   R <- foreach(a = arg, .combine = rbind, .inorder = F) %:%
#     foreach(i = 1L:I, .combine = rbind, .inorder = F) %dopar% {
#       if (log) cat(as.character(Sys.time()), ", ",
#                    i, " of ", I, ", ", round(i/I*100, 1), "%\n",
#                    file = paste(filename, ".log", sep = ""),
#                                 sep = "", append = T)
#       set.seed(seed[i])
#       tr <- try(eval(call(fun, a)), T)
#       if (class(tr) == "try-error")
#         res <- data.frame(t1, name, i, seed[i], tr[[1]], matrix(NA, 1, m))
#       else res <- data.frame(t1, name, i, seed[i], NA, tr)
#       colnames(res) <- paste("v", 1:(5+m), sep = "")
#       res
#   }
#   
#   t2 <- Sys.time()
#   time.run <- as.numeric(t2 - t1, units="secs")
#   
#   colnames(R) <- c("timestamp", "name", "i", "seed", "err", names(test))
#   rownames(R) <- NULL
#   
#   assign(name, R)
#   save(list = name, file = paste(filename, ".Rdata", sep=""))
#   
#   cat("Total time:", time.run, "\n")
#   cat("Average time:", time.run / I, "\n")
#   
#   if (print) {
#     cat("Rows in results", nrow(R), "ieraksti", "\n")
#     cat("First results:", "\n")
#     print(head(R))
#   }
#   
#   return(list(R, time.run))
# }



# ### Ver05
# ### Option for seed - on/off
# 
# Sim <- function(fun, arg, I = 5,
#                 name = "res",
#                 print = F,
#                 log = T,
#                 seed = NA) {
#   
#   require(foreach)
#   require(doMC)
#   
#   registerDoMC()
#   
#   # Argument type convertion
#   
#   fun <- as.character(fun)[1]
#   arg <- arg
#   I <- as.integer(I)[1]
#   name <- as.character(name)[1]
#   print <- as.logical(print)[1]
#   log <- as.logical(log)[1]
#   
#   # Testing
#   if (I<=0) stop("I has to be 1 or larger")
#   if (!is.na(seed) & length(seed) != I) stop("Wrong length of seed")
#   
#   # Test run
#   test <- eval(call(fun, arg[1]))
#   m <- length(test)
#   
#   cat("Simulation name:", name, "\n")
#   cat("Number of iterations:", I, "\n")
#   
#   t1 <- Sys.time()
#   
#   filename <- paste(name, t1)
#   
#   R <- foreach(a = arg, .combine = rbind, .inorder = F) %:%
#     foreach(i = 1L:I, .combine = rbind, .inorder = F) %dopar% {
#       if (log) cat(as.character(Sys.time()), ", ",
#                    i, " of ", I, ", ", round(i/I*100, 1), "%\n",
#                    file = paste(filename, ".log", sep = ""),
#                    sep = "", append = T)
#       
#       if (!is.na(seed)) set.seed(seed[i])
#       
#       tr <- try(eval(call(fun, a)), T)
#       
#       if (class(tr) == "try-error")
#         res <- data.frame(t1, name, i, seed[i], tr[[1]],
#                           matrix(NA, 1, m)) else
#         res <- data.frame(t1, name, i, seed[i], NA, tr)
#       
#       colnames(res) <- paste("v", 1:(5+m), sep = "")
#       
#       res
#     }
#   
#   t2 <- Sys.time()
#   time.run <- as.numeric(t2 - t1, units="secs")
#   
#   colnames(R) <- c("timestamp", "name", "i", "seed", "err", names(test))
#   rownames(R) <- NULL
#   
#   assign(name, R)
#   save(list = name, file = paste(filename, ".Rdata", sep=""))
#   
#   cat("Total time:", time.run, "\n")
#   cat("Average time:", time.run / I, "\n")
#   
#   if (print) {
#     cat("Rows in results", nrow(R), "ieraksti", "\n")
#     cat("First results:", "\n")
#     print(head(R))
#   }
#   
#   return(list(R, time.run))
# }



# ### Ver06
# ### Arguments for a function are provided as data.frame
# ### Default log to F
# 
# Sim <- function(fun, arg, I = 5,
#                 name = "res",
#                 print = F,
#                 log = F,
#                 seed = NA) {
#   
#   require(foreach)
#   require(doMC)
#   
#   registerDoMC()
#   
#   # Argument type convertion
#   
#   fun <- as.character(fun)[1]
#   arg <- as.data.frame(arg)
#   I <- as.integer(I)[1]
#   name <- as.character(name)[1]
#   print <- as.logical(print)[1]
#   log <- as.logical(log)[1]
#   
#   # Testing
#   if (I<=0) stop("I has to be 1 or larger")
#   if (!is.na(seed) & length(seed) != I) stop("Wrong length of seed")
#   
#   # Test run
#   # test <- eval(call(fun, arg[1]))
#   test <- do.call(fun, arg[1, ])
#   m <- length(test)
#   
#   cat("Simulation name:", name, "\n")
#   cat("Number of iterations:", I, "\n")
#   
#   t1 <- Sys.time()
#   
#   filename <- paste(name, t1)
#   
#   R <- foreach(a = 1L:nrow(arg), .combine = rbind, .inorder = F) %:%
#     foreach(i = 1L:I, .combine = rbind, .inorder = F) %dopar% {
#       if (log) cat(as.character(Sys.time()), ", ",
#                    i, " of ", I, ", ", round(i/I*100, 1), "%\n",
#                    file = paste(filename, ".log", sep = ""),
#                    sep = "", append = T)
#       
#       if (!is.na(seed)) set.seed(seed[i])
#       
#       # tr <- try(eval(call(fun, a)), T)
#       tr <- try(do.call(fun, arg[a, ]), T)
#       
#       if (class(tr) == "try-error")
#         res <- data.frame(t1, name, i, seed[i], tr[[1]],
#                           matrix(NA, 1, m)) else
#         res <- data.frame(t1, name, i, seed[i], NA, tr)
#       
#       colnames(res) <- paste("v", 1:(5+m), sep = "")
#       
#       res
#     }
#   
#   t2 <- Sys.time()
#   time.run <- as.numeric(t2 - t1, units="secs")
#   
#   colnames(R) <- c("timestamp", "name", "i", "seed", "err", names(test))
#   rownames(R) <- NULL
#   
#   assign(name, R)
#   save(list = name, file = paste(filename, ".Rdata", sep=""))
#   
#   cat("Total time:", time.run, "\n")
#   cat("Average time:", time.run / I, "\n")
#   
#   if (print) {
#     cat("Rows in results", nrow(R), "ieraksti", "\n")
#     cat("First results:", "\n")
#     print(head(R))
#   }
#   
#   return(list(R, time.run))
# }



### Ver06
### Arguments for a function are provided as data.frame
### Default log to F

### Ver07
### Arguments added to the output of the results


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
  arg <- as.data.frame(arg)
  I <- as.integer(I)[1]
  name <- as.character(name)[1]
  print <- as.logical(print)[1]
  log <- as.logical(log)[1]
  
  # Testing
  if (I<=0) stop("I has to be 1 or larger")
  if (!is.na(seed) & length(seed) != I) stop("Wrong length of seed")
  
  # Test run
  # test <- eval(call(fun, arg[1]))
  test <- do.call(fun, arg[1, , drop = F])
  m <- length(test)
  
  cat("Simulation name:", name, "\n")
  cat("Number of iterations:", I, "\n")
  cat("Number of cores:", cores, "\n")
  
  t1 <- Sys.time()
  
  #filename <- paste(name, t1)
  filename <- name
  
  R <- foreach(a = 1L:nrow(arg), .combine = rbind, .inorder = F) %:%
    foreach(i = 1L:I, .combine = rbind, .inorder = F) %dopar% {
      if (log) cat(as.character(Sys.time()), ", ",
                   i, " of ", I, ", ", round(i/I*100, 1), "%\n",
                   file = paste(filename, ".log", sep = ""),
                   sep = "", append = T)
      
      if (!is.na(seed)) set.seed(seed[i])
      
      # tr <- try(eval(call(fun, a)), T)
      tr <- try(do.call(fun, arg[a, , drop = F]), T)
      
      if (class(tr) == "try-error")
        res <- data.frame(t1, name, a, i, seed[i], arg[a, , drop = F],
                          tr[[1]], matrix(NA, 1, m)) else
        res <- data.frame(t1, name, a, i, seed[i], arg[a, ], NA, tr)
      
      colnames(res) <- paste("v", 1:(5 + ncol(arg) + 1 + m), sep = "")
      
      res
    }
  
  t2 <- Sys.time()
  time.run <- as.numeric(t2 - t1, units="secs")
  
  colnames(R) <- make.names(c("timestamp", "name", "a", "i", "seed",
                   colnames(arg), "err", colnames(test)), unique = T)
  rownames(R) <- NULL
  
  assign(name, R)
  save(list = name, file = paste(filename, ".Rdata", sep=""))
  
  cat("Total time:", time.run, "\n")
  cat("Average time:", time.run / I, "\n")
  
  if (print) {
    cat("Rows in results", nrow(R), "ieraksti", "\n")
    cat("First results:", "\n")
    print(head(R))
  }
  
  return(list(R, time.run))
}



################
### Dev Area ###
################

# # libs
# require(bigmemory)
# require(bigtabulate)
# require(rbenchmark)
# 
# 
# # Dir
# dir.proc <- paste(projwd, "Procedures", sep = "/")
# dir.data <- "~/DATA/LU/Work"
# dir.work <- "~/temp/sim"
# 
# 
# # Procedures
# setwd(dir.proc)
# source("Define_SamplingSRSWeek.R")
# source("Define_SamplingClusterWeek.R")
# source("Define_Trip.R")
# 
# 
# 
# # DATA
# setwd(dir.data)
# 
# frame.p <- attach.big.matrix("frame.p.desc")
# frame.h <- attach.big.matrix("frame.h.desc")
# load("frame.PSU.Rdata")
# load("frame.int.Rdata")
# 
# head(frame.p)
# head(frame.h)
# head(frame.PSU)
# head(frame.int)
# 
# M <- nrow(frame.h)
# N <- nrow(frame.p)
# 
# m <- 1000
# n <- round(m * N / M)
# 
# M.h <- as.vector(bigtable(frame.h, "strata"))
# m.h <- round(m * M.h / M)
# sum(m.h)
# 
# 
# # p <- rbind(c("sum","Empl",NA), c("sum","Unempl",NA),
# #   c("mean","Empl",NA), c("ratio","Unempl","Act"))
# # p
# 
# design.1 <- expression(SamplingSRSWeek(frame.1 = frame.p, n = n, weeks = 13))
# design.2 <- expression(SamplingClusterWeek(frame.1 = frame.p, frame.2 = frame.h, n = m,
#   name.cluster = "H_ID", weeks = 13))
# 
# class(design.1)
# str(design.1)
#  
# s.1 <- eval(design.1)
# s.2 <- eval(design.2)
# 
# head(s.1)
# head(s.2)
# 
# t1 <- vTrip.fast.1(s.1[c("int.ID", ".week", "coord_x_p", "coord_y_p")],
#                    frame.int[c("int.ID", "x_int", "y_int")])
# t2 <- vTrip.fast.1(s.2[c("int.ID", ".week", "coord_x_p", "coord_y_p")],
#                    frame.int[c("int.ID", "x_int", "y_int")])
# 
# t1 / 1e3
# t2 / 1e3
# 
# 
# 
# run <- function(design) {
#   s <- eval(design)
#   t <- vTrip.fast.1(s[c("int.ID", ".week", "coord_x_p", "coord_y_p")],
#                     frame.int[c("int.ID", "x_int", "y_int")])
#   names(t) <- "dist"
#   return(t / 1e3)
# }
# 
# a <- run(design.1)
# a
# names(a)
# 
# run(design.1)
# run(design.2)
# 
# 
# 
# 
# 
# # WorkDir
# setwd(dir.work)
# 
# # Sim.1(expression(run(design.1)), 5, "test", T, T)
# # Sim.2(expression(run(design.1)), 5, "test", T, T)
# # 
# # Sim.1(expression(run(design.2)), 5, "test", T, T)
# # Sim.2(expression(run(design.2)), 5, "test", T, T)
# 
# r1 <- Sim.3(expression(run(design.1)), 5, "test", T, T)
# class(r1)
# 
# r1[[1]]
# set.seed(r1[[1]][5, "seed"])
# run(design.1)
# 
# r1[[2]]
# 
# # load("res.Rdata")
# # res
# 
# # Simulation(design=design.3, param=p, I=1000, ret = F)
# 
# 
# 
# ### Benchmarking
# 
# benchmark(Sim.1(expression(run(design.1)), 5, "test", T, T),
#           Sim.2(expression(run(design.1)), 5, "test", T, T),
#           Sim.3(expression(run(design.1)), 5, "test", T, T),
#           columns=c("test", "replications", "elapsed", "relative"),
#           order="relative", replications=1)
