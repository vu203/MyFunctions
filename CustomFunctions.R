### Useful custom functions:


#-------------------------------------------------------------------------------
# General functions:

Count <- function(x) length(x[!is.na(x)])			 

SE <- function(x) sd(x, na.rm=TRUE)/sqrt(Count(x))	 							
CI <- function(x) 1.96*sd(x, na.rm=TRUE)/sqrt(Count(x))     

MovingAverage <- function(x, w = 5) {
		filter(x, rep(1/w, w), sides = 2)
}

RemoveOutliers <- function(x, mincut, maxcut) {
	y <- x
	y[x < mincut] <- NA
	y[x > maxcut] <- NA
	y
}

MedianOfPositive <- function(x) {   
	median(subset(x, x > 0), na.rm = TRUE)
}
MedianOfNegative <- function(x) {
	median(subset(x, x < 0), na.rm = TRUE)
}

MedianOfExtending <- function(x) {
	median(subset(x, x > threshold.ext.per.t), na.rm = TRUE)
}

MedianOfRetracting <- function(x) {
	median(subset(x, x < threshold.retr.per.t), na.rm = TRUE)
}



#-------------------------------------------------------------------------------
# NA-related functions:

FirstNonNA <- function(x) {
	nonNA.index <- which(!is.na(x))
	first.nonNA <- min(nonNA.index, na.rm = TRUE)
	last.nonNA <- max(nonNA.index, na.rm = TRUE)
	return(first.nonNA)
}
LastNonNA <- function(x) {
	nonNA.index <- which(!is.na(x))
	last.nonNA <- max(nonNA.index, na.rm = TRUE)
	return(last.nonNA)
}

NonNArange <- function(x) {  
	FirstNonNA(x):LastNonNA(x)
	}


#-------------------------------------------------------------------------------
# Autocorrelation & cross-correlation functions:

# Construct an auto-correlation table:

AcfTable <- function(x, L) {
	y <- data.frame(matrix(NA, ncol = ncol(x), nrow = L))
	colnames(y) <- colnames(x)
	for (i in 1:ncol(x)) {
		acf.i <- as.vector(acf(x[, i], na.action = na.pass, lag.max = MaxLag, plot = FALSE)[[1]])
		y[, i] <- acf.i
		rm(acf.i)
		}
	y	
}

# To extract the root of autocorrelation from the output of the AcfTable function, use:

FirstNegative <- function(x) {
	if(sum(!is.na(x)) > 0) {				
		min (which(x <= 0), na.rm = TRUE)	# requires at least one non-NA value in vector (if >3 elements in the x column acf is not computed, so acf is NA throughout, which returns Inf) 
	} else {
		NA									# returns NA in place of Inf
	}
}

# example of use in Bounder modules:
# acf.dctm <- AcfTable(dctm, MaxLag) 
# acf.dctm.roots <- apply (acf.dctm, 2, FirstNegative)


#-------------------------------------------------------------------------------
# Block randomisation:

extractBlockIndex <- function(which.block, block.size, ...) {
	start <- ((which.block-1) * block.size) + 1
	end <- ((which.block) * block.size)
	c(start:end)
}

BlockReshuffle <- function(x, block.size = 12) {
	
	stopifnot(length(x) > block.size)
	
	n.blocks <- length(x) %/% block.size
	overhang <- length(x) %% block.size
			
	included <- 1:(block.size*n.blocks)
	excluded.overhang <- setdiff(seq_along(x), included) 
	
	x.in.blocks <- list()
	for(i in 1:n.blocks) {
		x.in.blocks[[i]] <- x[extractBlockIndex(i, 12)]
	}
	
	# which blocks to keep in place (full of NAs), which blocks to swap over?
	
	max.NA.per.block <- 0.25 * block.size 
	blocks.to.shuffle <- which(lapply(x.in.blocks, Count) > max.NA.per.block)
	blocks.to.keep <- which(lapply(x.in.blocks, Count) <= max.NA.per.block)	
	
	# generate permuted blocks, plus insert NA blocks into their respective positions

	set.seed(0.1)
	new.order <- c(sample(blocks.to.shuffle))
	for (j in blocks.to.keep) {
		new.order <- append(new.order, j, after = j-1)
	}
	
	# new vector
		
	for(k in new.order) {
		
		if(exists("z") == FALSE) {z <- c()}
		
		z <- append(z, x.in.blocks[[k]])
	}
	z <- append(z, x[excluded.overhang])
	z	
}


#-------------------------------------------------------------------------------
# Graphing-related functions:

DrawErrorAsPolygon <- function(x, y1, y2, tt, col = 'grey') {
    polygon(c(x[tt], rev(x[tt])), c(y1[tt], rev(y2[tt])), 
    col = col,
    border = NA)			
    }



