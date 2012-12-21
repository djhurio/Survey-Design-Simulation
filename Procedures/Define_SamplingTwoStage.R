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

	# Sample file as data.frame
	s.2 <- do.call(rbind, s.1)


	# frame.PSU
	frame.PSU[, "cum.size"] <- cumsum(frame.PSU[, "size"])
	frame.PSU[, "a"] <- frame.PSU[, "cum.size"] - frame.PSU[, "size"]
	frame.PSU[, "b"] <- frame.PSU[, "cum.size"]

	
	### Selecting of PSUs
	s.3 <- s.2$A


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

  if (length(s.4) != nrow(s.2))
    stop("Error in s.4 or s.2 -- the length is not equal")
  
  # PSU sample
	s.5 <- data.frame(frame.PSU[s.4, ], s.2)

	if (length(unique(s.5[, name.PSU])) < nrow(s.5))
    stop("Dublicate in PSU sample")

	# PSU sample (IDs only)
	s.6 <- s.5[, name.PSU]

	#param
	param$total.m <- param$A * param$B * param$w

	frame.PSU$sampled <- as.numeric(frame.PSU[, name.PSU] %in% s.6)
  
  frame.PSU <- merge(frame.PSU, param[c("s", "m", "M", "total.m")],
                     by.x = name.strata, by.y = "s")
  frame.PSU$m <- frame.PSU$m * frame.PSU$sampled

  frame.PSU[, name.weight.s1] <- frame.PSU$M /
    (frame.PSU$size * frame.PSU$total.m) * frame.PSU$sampled


  ### Second stage sampling
	sample.TSU <- SamplingClusterStr(frame.TSU,
                                   frame.SSU,
                                   frame.PSU$m,
                                   name.weight.s2,
                                   name.SSU,
                                   name.PSU)

	sample.TSU <- merge(sample.TSU, frame.PSU[, c(name.PSU, name.weight.s1)])

	sample.TSU[, name.weight] <- sample.TSU[, name.weight.s1] *
    sample.TSU[, name.weight.s2]
  
  
  ### Add week  
  s.week <- s.5[, c(name.PSU, "week")]

  sample.TSU <- merge(sample.TSU, s.week)

  names(sample.TSU)[ncol(sample.TSU)] <- name.week
	
	return(sample.TSU)
}
