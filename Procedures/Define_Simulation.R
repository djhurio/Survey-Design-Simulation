# ### Function: Simulation

# ### Ver01
# # Initial version

# Simulation.1 <- function(design, param, I = 5, name = "res") {
# 
#   # Argument type convertion
#   
#   param <- as.matrix(param)
#   I <- as.integer(I[1])
#   name <- as.character(name[1])
#   design <- as.expression(design)
# 
#   # Testing
#   
#   if (!all(param[,1] %in% c("sum", "mean", "ratio"))) stop("Error in param 1")
#   if (ncol(param) != 3) stop("Error in param 2")
#   if (I<=0) stop("I has to be 1 or larger")
# 
#   R <- NULL
# 
#   for (i in 1:I) {
#     t1 <- Sys.time()
#     s <- eval(design)
#     e <- data.frame(Estimation(x = s, w = s$.dw, param = param))
#     t2 <- Sys.time()
#     time.run <- t2 - t1
#     e2 <- data.frame(name, i, time.run, e)
#     R <- rbind(R, e2)
#     cat(paste("Simulation name: ", name, ". Iteration: ", i, " from ", I, ". Time: ", round(time.run, 3), "\n", sep=""))
#     flush.console()
#   }
#   
#   assign(name, R)
#   save(list = name, file = paste(name, ".Rdata", sep=""))
# 
#   cat(paste("Average time:", round(mean(R[,"time.run"]), 3), "\n", sep=" "))
#   return(R)
# }


### Ver02
# Introduced parameter "ret" to controle the return of the function
# Introduced progress bar

# Simulation.2 <- function(design, param, I = 5, name = "res", ret = T) {
# 
#   # Argument type convertion
#   
#   param <- as.matrix(param)
#   I <- as.integer(I[1])
#   name <- as.character(name[1])
#   design <- as.expression(design)
# 
#   # Testing
#   
#   if (!all(param[,1] %in% c("sum", "mean", "ratio"))) stop("Error in param 1")
#   if (ncol(param) != 3) stop("Error in param 2")
#   if (I<=0) stop("I has to be 1 or larger")
# 
#   cat(paste("Simulation name:", name, "\n", sep=" "))
#   cat(paste("Number of iterations:", I, "\n", sep=" "))
# 
#   R <- NULL
#   t1 <- Sys.time()
# 
#   pb <- txtProgressBar(min = 0, max = I, style = 3)
#   for (i in 1:I) {
#     s <- eval(design)
#     e <- data.frame(Estimation(x = s, w = s$.dw, param = param))
#     e2 <- data.frame(name, i, e)
#     R <- rbind(R, e2)
#     setTxtProgressBar(pb, i)
#   }
#   close(pb)
# 
#   t2 <- Sys.time()
#   time.run <- t2 - t1
#   
#   assign(name, R)
#   save(list = name, file = paste(name, ".Rdata", sep=""))
# 
#   cat(paste("Total time:", time.run, "\n", sep=" "))
#   cat(paste("Average time:", time.run / I, "\n", sep=" "))
#   
#   if (ret) return(time.run)
# }



### Ver03
# Preallocate the object for results

# Simulation.3 <- function(design, param, I = 5, name = "res", ret = F, pr = F) {
# 
#   # Argument type convertion
#   
#   param <- as.matrix(param)
#   I <- as.integer(I[1])
#   name <- as.character(name[1])
#   design <- as.expression(design)
# 
#   # Testing
#   
#   if (!all(param[,1] %in% c("sum", "mean", "ratio"))) stop("Error in param 1")
#   if (ncol(param) != 3) stop("Error in param 2")
#   if (I<=0) stop("I has to be 1 or larger")
# 
#   cat(paste("Simulation name:", name, "\n", sep=" "))
#   cat(paste("Number of iterations:", I, "\n", sep=" "))
# 
#   R <- data.frame(cbind(i = 1:I, matrix(as.numeric(NA), I, 2+nrow(param))))
#   R <- cbind(name = name, R)
# 
#   t1 <- Sys.time()
# 
#   pb <- txtProgressBar(min = 0, max = I, style = 3)
#   for (i in 1:I) {
#     s <- eval(design)
#     e <- data.frame(Estimation(x = s, w = s$.dw, param = param))
# 	R[i, -(1:2)] <- e
#     setTxtProgressBar(pb, i)
#   }
#   close(pb)
# 
#   colnames(R)[-(1:2)] <- colnames(e)
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
# 	cat(paste("First results:", "\n", sep=" "))
#     print(head(R))
# 	# cat(paste("Last results:", "\n", sep=" "))
#     # print(tail(R))
#   }
# 
#   if (ret) return(time.run)
# }


### Ver04
# Add tour length

# Simulation.4 <- function(design, param, I = 5, name = "res", ret = F, pr = F) {
# 
#   # Argument type convertion
#   
#   param <- as.matrix(param)
#   I <- as.integer(I[1])
#   name <- as.character(name[1])
#   design <- as.expression(design)
# 
#   # Testing
#   
#   if (!all(param[,1] %in% c("sum", "mean", "ratio"))) stop("Error in param 1")
#   if (ncol(param) != 3) stop("Error in param 2")
#   if (I<=0) stop("I has to be 1 or larger")
# 
#   cat(paste("Simulation name:", name, "\n", sep=" "))
#   cat(paste("Number of iterations:", I, "\n", sep=" "))
# 
#   R <- data.frame(cbind(i = 1:I, matrix(as.numeric(NA), I, 3+nrow(param))))
#   R <- cbind(name = name, R)
# 
#   t1 <- Sys.time()
# 
#   pb <- txtProgressBar(min = 0, max = I, style = 3)
#   for (i in 1:I) {
#     s <- eval(design)
#     trip <- Trip(s, "coord_x_p", "coord_y_p", "int.ID")
#     e <- data.frame(Estimation(x = s, w = s$.dw, param = param))
#     R[i, -(1:2)] <- cbind(trip, e)
#     setTxtProgressBar(pb, i)
#   }
#   close(pb)
# 
#   colnames(R)[-(1:2)] <- c("trip", colnames(e))
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
# 	cat(paste("First results:", "\n", sep=" "))
#     print(head(R))
# 	# cat(paste("Last results:", "\n", sep=" "))
#     # print(tail(R))
#   }
# 
#   if (ret) return(time.run)
# }


### Ver05
# Assignment of interviewers

# Simulation <- function(design, param, I = 5, name = "res", ret = F, pr = F) {
# 
#   # Argument type convertion
#   
#   param <- as.matrix(param)
#   I <- as.integer(I[1])
#   name <- as.character(name[1])
#   design <- as.expression(design)
# 
#   # Testing
#   
#   if (!all(param[,1] %in% c("sum", "mean", "ratio"))) stop("Error in param 1")
#   if (ncol(param) != 3) stop("Error in param 2")
#   if (I<=0) stop("I has to be 1 or larger")
# 
#   cat(paste("Simulation name:", name, "\n", sep=" "))
#   cat(paste("Number of iterations:", I, "\n", sep=" "))
# 
#   R <- data.frame(cbind(i = 1:I, matrix(as.numeric(NA), I, 3+nrow(param))))
#   R <- cbind(name = name, R)
# 
#   t1 <- Sys.time()
# 
#   pb <- txtProgressBar(min = 0, max = I, style = 3)
#   for (i in 1:I) {
#     s <- eval(design)
#     #print(class(s))
#     if (is.data.frame(s)) {
#       s$int.week <- s$int.ID * 100 + s$.week
#       trip <- Trip(s, "coord_x_p", "coord_y_p", "int.week")
#       e <- data.frame(Estimation(x = s, w = s$.dw, param = param))
#       R[i, -(1:2)] <- cbind(trip, e)
#     }
#     setTxtProgressBar(pb, i)
#   }
#   close(pb)
# 
#   colnames(R)[-(1:2)] <- c("trip", colnames(e))
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
#   cat(paste("First results:", "\n", sep=" "))
#     print(head(R))
# 	# cat(paste("Last results:", "\n", sep=" "))
#     # print(tail(R))
#   }
# 
#   if (ret) return(time.run)
# }


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
# # dir <- "/home/djhurio/temp"
# # dir <- "C:/DATA/LU/Results"
# dir.proc <- "~/Dropbox/LU/Darbs/Simulation/050_Simulation/20_Procedures"
# dir.data <- "~/DATA/LU/Work"
# dir.work <- "~/temp/sim"
# 
# 
# # Procedures
# setwd(dir.proc)
# source("Define_SamplingSRS.R")
# source("Define_SamplingCluster.R")
# source("Define_SamplingClusterStr.R")
# source("Define_Estimation.R")
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
# ls()
# 
# head(frame.p)
# head(frame.h)
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
# p <- rbind(c("sum","Empl",NA), c("sum","Unempl",NA),
#   c("mean","Empl",NA), c("ratio","Unempl","Act"))
# p
# 
# design.1 <- expression(SamplingSRS(frame.1 = frame.p, n = n))
# design.2 <- expression(SamplingCluster(frame.1 = frame.p, frame.2 = frame.h, n = m,
#   name.cluster = "H_ID"))
# design.3 <- expression(SamplingClusterStr4(frame.1 = frame.p, frame.2 = frame.h,
#   n = m.h, name.cluster = "H_ID", name.strata = "strata"))
# 
# class(design.1)
# str(design.1)
#  
# s.1 <- eval(design.1)
# s.2 <- eval(design.2)
# s.3 <- eval(design.3)
# 
# Trip(s.1, "coord_x_p", "coord_y_p", "int.ID")
# Trip(s.2, "coord_x_p", "coord_y_p", "int.ID")
# Trip(s.3, "coord_x_p", "coord_y_p", "int.ID")
# 
# head(s.1)
# head(s.2)
# head(s.3)
# 
# Estimation(x=s.1, w=s.1$.dw, param=p)
# Estimation(x=s.2, w=s.2$.dw, param=p)
# Estimation(x=s.3, w=s.3$.dw, param=p)
# 
# 
# # Temp param
# 
# design <- design.1
# param <- p
# name <- "tmp"
# I <- 5
# 
# 
# 
# # WorkDir
# setwd(dir.work)
# 
# Simulation.1(design=design.1, param=p, I=10)
# Simulation.1(design=design.2, param=p, I=10)
# Simulation.1(design=design.3, param=p, I=10)
# 
# Simulation.2(design=design.1, param=p, I=10, ret = F)
# Simulation.2(design=design.2, param=p, I=10, ret = F)
# Simulation.2(design=design.3, param=p, I=10, ret = F)
# 
# Simulation.3(design=design.1, param=p, I=10, ret = F)
# Simulation.3(design=design.1, param=p, I=10, ret = T)
# Simulation.3(design=design.1, param=p, I=10, ret = F, pr = T)
# Simulation.3(design=design.2, param=p, I=10, ret = F, pr = T)
# Simulation.3(design=design.3, param=p, I=10, ret = F, pr = T)
# 
# Simulation.4(design=design.1, param=p, I=10, ret = F)
# Simulation.4(design=design.1, param=p, I=10, ret = T)
# Simulation.4(design=design.1, param=p, I=10, ret = F, pr = T)
# Simulation.4(design=design.2, param=p, I=10, ret = F, pr = T)
# Simulation.4(design=design.3, param=p, I=10, ret = F, pr = T)
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
# # benchmark(Simulation.1(design=design.1, param=p, I=20),
#   # Simulation(design=design.1, param=p, I=20, ret = F),
#   # columns=c("test", "replications", "elapsed", "relative"),
#   # order="relative", replications=10)
# 
# # benchmark(Simulation.1(design=design.2, param=p, I=20),
#   # Simulation(design=design.2, param=p, I=20, ret = F),
#   # columns=c("test", "replications", "elapsed", "relative"),
#   # order="relative", replications=10)
# 
# # benchmark(Simulation.1(design=design.3, param=p, I=20),
#   # Simulation(design=design.3, param=p, I=20, ret = F),
#   # columns=c("test", "replications", "elapsed", "relative"),
#   # order="relative", replications=10)
# 
# benchmark(Simulation.1(design=design.3, param=p, I=20),
#   Simulation.2(design=design.3, param=p, I=20, ret = F),
#   Simulation.3(design=design.3, param=p, I=20, ret = F),
#   columns=c("test", "replications", "elapsed", "relative"),
#   order="relative", replications=5)
# 
