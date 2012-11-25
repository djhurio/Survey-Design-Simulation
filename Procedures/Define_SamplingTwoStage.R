############# Define two stage sampling procedure

### Ver01
# Initial version

SamplingTwoStage <- function(frame.PSU,
                             frame.SSU,
                             frame.TSU,
                             name.weight.s1 = ".dw1",
                             name.weight.s2 = ".dw2",
                             name.weight = ".dw",
                             name.week = "week",
                             name.PSU,
                             name.SSU,
                             name.strata,
                             param) {
  
  ### Libs
  require(bigmemory)
  require(bigtabulate)
 
  ### Argument type convertion
  if (!is.big.matrix(frame.PSU)) frame.PSU <- data.frame(frame.PSU)
  if (!is.big.matrix(frame.SSU)) frame.SSU <- data.frame(frame.SSU)
  if (!is.big.matrix(frame.TSU)) frame.TSU <- data.frame(frame.TSU)

  name.weight.s1 <- as.character(name.weight.s1[1])
  name.weight.s2 <- as.character(name.weight.s2[1])
  name.weight <- as.character(name.weight[1])

  name.PSU <- as.character(name.PSU[1])
  name.SSU <- as.character(name.SSU[1])

  name.strata <- as.character(name.strata[1])
  
  param <- data.frame(param)
  

  ### Testing
  if (name.weight %in% colnames(frame.PSU))
    print("WARNING: Weight variable exists in frame.PSU, it will be overwrited")
  if (name.weight %in% colnames(frame.SSU))
    print("WARNING: Weight variable exists in frame.SSU, it will be overwrited")
  if (name.weight %in% colnames(frame.TSU))
    print("WARNING: Weight variable exists in frame.TSU, it will be overwrited")

  if (!name.PSU %in% colnames(frame.PSU))
    stop("ERROR: Can not find PSU variable in frame.PSU")
  if (!name.PSU %in% colnames(frame.SSU))
    stop("ERROR: Can not find PSU variable in frame.SSU")
  if (!name.PSU %in% colnames(frame.TSU))
    stop("ERROR: Can not find PSU variable in frame.TSU")

  if (!name.SSU %in% colnames(frame.SSU))
    stop("ERROR: Can not find SSU variable in frame.SSU")
  if (!name.SSU %in% colnames(frame.TSU))
    stop("ERROR: Can not find SSU variable in frame.TSU")

  if (!name.strata %in% colnames(frame.PSU))
    stop("ERROR: Can not find strata variable in frame.PSU")

  if (any(frame.PSU[, name.strata] != sort(frame.PSU[, name.strata])))
    stop("ERROR: frame.PSU is not sorted by strata")
  if (any(frame.PSU[, name.PSU] != sort(frame.PSU[, name.PSU])))
    stop("ERROR: frame.PSU is not sorted by PSU")

  if (any(frame.SSU[, name.strata] != sort(frame.SSU[, name.strata])))
    stop("ERROR: frame.SSU is not sorted by strata")
  if (any(frame.SSU[, name.PSU] != sort(frame.SSU[, name.PSU])))
    stop("ERROR: frame.SSU is not sorted by PSU")

	
	# Function nemesis
	nemesis <- function(s, A, b, W, d, Q, w, M, hi, add) {

    sample.step <- Q / W + (1 + d) / A / W
		
    sample.PSU <- data.frame(strata = s,
                             b = b,
                             i = rep(1:A, w),
                             week = rep(1:w, each=A))
		
    sample.PSU$a <- (hi + 
      (b - 1) * (1 + d) * (A + 1) / (A * b) +
      (sample.PSU$i - 1) * (1 + d) / A +
			(sample.PSU$week - 1) * sample.step) %% 1
		
    sample.PSU$x <- sin(2 * pi * sample.PSU$a) / 2 / pi
		sample.PSU$y <- cos(2 * pi * sample.PSU$a) / 2 / pi
		
    sample.PSU$A <- sample.PSU$a * M + add
		
    return(sample.PSU)
	}


	# Random number
	param$hi <- runif(nrow(param))

  # Add
	param$add <- cumsum(param$M) - param$M

	# Function for param.2
	fun.tmp1 <- function(b) data.frame(param[param$B >= b, ], b=b)

	# param.2
	param.2 <- do.call(rbind, lapply(1:max(param$B), fun.tmp1))
	param.2 <- param.2[order(param.2$s, param.2$b), ]
	rownames(param.2) <- NULL
  #	param.2

	# Sampling
	s.1 <- mapply(nemesis,
                s = param.2$s,
                A = param.2$A,
                W = param.2$W,
                d = param.2$d,
                Q = param.2$Q,
                w = param.2$w,
                M = param.2$M,
                hi = param.2$hi,
                add = param.2$add,
                b = param.2$b,
                SIMPLIFY = F)
  #head(s.1[[1]])

	# Sample file as data.frame
	s.2 <- do.call(rbind, s.1)
	#head(s.2)


	# frame.PSU
	#head(frame.PSU)
	frame.PSU[, "cum.size"] <- cumsum(frame.PSU[, "size"])
	frame.PSU[, "a"] <- frame.PSU[, "cum.size"] - frame.PSU[, "size"]
	frame.PSU[, "b"] <- frame.PSU[, "cum.size"]
	#head(frame.PSU)

	
	### Selecting of PSUs

	# List of sampling points
  #head(s.2)
	s.3 <- s.2$A
	#head(s.3)


	### Function to select PSUs by sampling points
	f.sel <- function(a, b, x) {
		a <- unlist(a)
		b <- unlist(b)
		x <- as.numeric(x[1])
		n <- length(a)
		return((1:n)[x > a & x < b])
	}

	# Indexes of sampled PSUs
	s.4 <- unlist(sapply(s.3, f.sel, a = frame.PSU$a, b = frame.PSU$b))
	#head(s.4)
	#head(s.2)

  if (length(s.4) != nrow(s.2))
    stop("Kļūda s.4 vai s.2 -- nav vienāds garums")
  
  # PSU sample
	s.5 <- data.frame(frame.PSU[s.4, ], s.2)
	#nrow(s.5)
	#head(s.5)

	if (length(unique(s.5[, name.PSU])) < nrow(s.5))
    stop("Dublicate in PSU sample")
	#if (length(unique(s.5[, name.PSU])) < nrow(s.5)) return(NULL)

	# PSU sample (IDs only)
	s.6 <- s.5[, name.PSU]
	#class(s.6)
	#head(s.6)
	#head(sort(s.6))

	#param
	param$total.m <- param$A * param$B * param$w
	#param
	#sum(param$total.m * param$m)

	#head(frame.PSU)
	frame.PSU$sampled <- as.numeric(frame.PSU[, name.PSU] %in% s.6)
  #head(frame.PSU)
  
  # (Error)
  # frame.PSU$m <- param$m[frame.PSU[, name.strata]] * frame.PSU$sampled

  frame.PSU <- merge(frame.PSU, param[c("s", "m", "M", "total.m")],
                     by.x = name.strata, by.y = "s")
  frame.PSU$m <- frame.PSU$m * frame.PSU$sampled
  #head(frame.PSU)
	#sum(frame.PSU$m)

	# (Error)
  # frame.PSU[, name.weight.s1] <- param$M[frame.PSU[, name.strata]] /
	#	(frame.PSU$size * param$total.m[frame.PSU[, name.strata]]) *
  # frame.PSU$sampled
  
  frame.PSU[, name.weight.s1] <- frame.PSU$M /
    (frame.PSU$size * frame.PSU$total.m) * frame.PSU$sampled

	#head(frame.PSU)
	#sum(frame.PSU$sampled)
	#sum(frame.PSU[, name.weight.s1])


	### Second stage sampling

  #head(frame.PSU)
  #head(frame.SSU)
  
	sample.TSU <- SamplingClusterStr(frame.TSU,
                                   frame.SSU,
                                   frame.PSU$m,
                                   name.weight.s2,
                                   name.SSU,
                                   name.PSU)
	#head(sample.TSU)
	#length(unique(sample.TSU$H_ID))

	sample.TSU <- merge(sample.TSU, frame.PSU[, c(name.PSU, name.weight.s1)])
	#head(sample.TSU)

	sample.TSU[, name.weight] <- sample.TSU[, name.weight.s1] *
    sample.TSU[, name.weight.s2]
	#sum(sample.TSU[, name.weight])
  
  
  ### Add week  
  
  #head(s.5)
  s.week <- s.5[, c(name.PSU, "week")]
  #head(s.week)
  
  #nrow(sample.TSU)
  sample.TSU <- merge(sample.TSU, s.week)
  #nrow(sample.TSU)

  names(sample.TSU)[ncol(sample.TSU)] <- name.week
	
	return(sample.TSU)
}



# ################
# ### Dev area ###
# ################
# 
# ### Reset
# setwd(projwd)
# rm(list = ls())
# gc()
# source(".Rprofile")
# 
# 
# ### Libs
# library(bigmemory)
# library(bigtabulate)
# library(biganalytics)
# library(rbenchmark)
# 
# require(xtable)
# 
# ### Proc
# 
# setwd(dir.proc)
# source("Define_SamplingClusterStr.R")
# 
# 
# ### DATA
# 
# setwd(dir.data)
# 
# frame.p <- attach.big.matrix("frame.p.desc")
# frame.h <- attach.big.matrix("frame.h.desc")
# 
# head(frame.p)
# head(frame.h)
# 
# load("frame.PSU.Rdata")
# test.df(frame.PSU)
# table(frame.PSU$strata)
# 
# sum(frame.PSU$size) == nrow(frame.h)
# 
# 
# ### Strata selection (if necesary)
# 
# # strata.selected <- 1:4
# # 
# # 
# # # frame.p
# # head(frame.p)
# # bigtable(frame.p, "strata")
# # class(frame.p)
# # frame.p <- frame.p[frame.p[, "strata"] %in% strata.selected, ]
# # class(frame.p)
# # 
# # # frame.h
# # head(frame.h)
# # bigtable(frame.h, "strata")
# # class(frame.h)
# # frame.h <- frame.h[frame.h[, "strata"] %in% strata.selected, ]
# # class(frame.h)
# # 
# # # frame.PSU
# # head(frame.PSU)
# # bigtable(frame.PSU, "strata")
# # class(frame.PSU)
# # frame.PSU <- frame.PSU[frame.PSU[, "strata"] %in% strata.selected, ]
# # class(frame.PSU)
# 
# 
# 
# ### Sample design parameters
# 
# N <- nrow(frame.p)
# N
# 
# M <- nrow(frame.h)
# M
# 
# M.h <- as.vector(bigtable(frame.h, "strata"))
# M.h
# sum(M.h)
# sum(M.h) == M
# 
# max.PSU.size.h <- aggregate(frame.PSU["size"], frame.PSU["strata"], max)[, 2]
# max.PSU.size.h
# 
# # delta <- 1 / max.PSU.size.h
# delta <- max.PSU.size.h / M.h
# delta
# 
# weeks <- 13
# 
# 
# sampl.des.par <- data.frame(s = 1:4,
#                             A = 8,
#                             B = c(1, 2, 2, 2),
#                             W = 13,
#                             d = delta,
#                             Q = 0,
#                             w = weeks,
#                             M = M.h,
#                             m = c(10, 7, 8, 9))
# 
# # sampl.des.par <- data.frame(s = 4,
# #                             A = 8,
# #                             B = 2,
# #                             W = 13,
# #                             d = 0,
# #                             Q = 0,
# #                             w = weeks,
# #                             M = M.h,
# #                             m = 9)
# 
# sampl.des.par
# names(sampl.des.par)
# 
# print(xtable(sampl.des.par, digits = c(0, 0, 0, 0, 0, 6, 0, 0, 0, 0)),
#       include.rownames = F, only.contents = T)
# 
# m <- sum(sampl.des.par$A * sampl.des.par$B * sampl.des.par$w * sampl.des.par$m)
# m
# 
# 
# 
# ### Temp param
# 
# frame.PSU <- frame.PSU
# frame.SSU <- frame.h
# frame.TSU <- frame.p
# name.weight.s1 <- ".dw1"
# name.weight.s2 <- ".dw2"
# name.weight <- ".dw"
# name.week <- "week"
# name.PSU <- "iec2010"
# name.SSU <- "H_ID"
# name.strata <- "strata"
# param <- sampl.des.par
# 
# 
# 
# 
# ### Testing 1
# 
# s1 <- SamplingTwoStage(frame.PSU, frame.h, frame.p,
#                        ".dw1", ".dw2", ".dw", "week",
#                        "iec2010", "H_ID", "strata",
#                        sampl.des.par)
# 
# head(s1)
# nrow(s1)
# length(unique(s1$H_ID))
# length(unique(s1$H_ID)) == m
# 
# 
# ### Test 2
# sampl.des.par <- data.frame(s = 1:4,
#                             A = 1,
#                             B = 1,
#                             W = 12,
#                             d = delta,
#                             Q = 4,
#                             w = 12,
#                             M = M.h,
#                             m = 10)
# 
# sampl.des.par
# m <- sum(sampl.des.par$A * sampl.des.par$B * sampl.des.par$w * sampl.des.par$m)
# m
# 
# s1 <- SamplingTwoStage(frame.PSU, frame.h, frame.p,
#                        ".dw1", ".dw2", ".dw", "week",
#                        "iec2010", "H_ID", "strata",
#                        sampl.des.par)
# head(s1)
# nrow(s1)
# length(unique(s1$H_ID))
# length(unique(s1$H_ID)) == m
# 
# 
# 
# 
# n <- round(m * N / M)
# n
# 
# m.h <- round(m * M.h / M)  # Proportional sample allocation
# m.h
# sum(m.h)
# sum(m.h) == m
# 
# 
# 
# 
# ### Testing 2
# 
# set.seed(1)
# 
# # s1 <- SamplingTwoStage(frame.PSU = frame.PSU,
# #   frame.SSU = frame.h,
# #   frame.TSU = frame.p,
# #   name.weight.s1 = ".dw1",
# #   name.weight.s2 = ".dw2",
# #   name.weight = ".dw",
# #   name.week = "week",
# #   name.PSU = "iec2010",
# #   name.SSU = "H_ID",
# #   name.strata = "strata",
# #   param = sampl.des.par)
# 
# s1 <- SamplingTwoStage(frame.PSU, frame.h, frame.p,
#                        ".dw1", ".dw2", ".dw", "week",
#                        "iec2010", "H_ID", "strata",
#                        sampl.des.par)
# head(s1)
# nrow(s1)
# length(unique(s1$H_ID))
# 
# table(s1$.dw)
# sum(s1$.dw)
# 
# N.h <- as.vector(bigtable(frame.p, "strata"))
# N.h
# 
# cbind(N.h, tapply(s1$.dw, s1$strat, sum))
# 
# table(s1$week)
# 
# 
# # fix(s1)
# 
# benchmark(
#   SamplingTwoStage(frame.PSU, frame.h, frame.p, ".dw1", ".dw2", ".dw", "week", "iec2010", "H_ID",
#  "strata", sampl.des.par),
#   columns=c("test", "replications", "elapsed", "relative"),
#   order="relative",
#   replications=10
# )
