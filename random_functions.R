## Bodo Winter
## June 6, 2016
## Functions for generating random norms

## Function for generating random dataset based on means:

random_df <- function(df) {
	## Initialize means and sds per modality:
	
	xmeans <- apply(df[, modalities], 2, mean)
	xsds <- apply(df[, modalities], 2, sd)
	
	## Generate data frame template to be filled:
	
	xdf <- matrix(numeric(nrow(df) * 5), ncol = 5)
	colnames(xdf) <- names(xmeans)
	
	for (i in 1:nrow(xdf)) {
		this_word <- rnorm(5, mean = xmeans, sd = xsds)		
		if (any(this_word > 5)) {				# cut-off to preserve to range given by Lynott & Connell
			this_word[this_word > 5] <- 5
			}
		if (any(this_word < 0)) {
			this_word[this_word < 0] <- 0
			}
		xdf[i, ] <- this_word		
		}
	
	return(xdf)
	}

## Re-generating distributions just based on mean and SD:

random_excl1 <- function(df) {
	## Initialize means and sds per modality:
	
	xmeans <- apply(df[, modalities], 2, mean)
	xsds <- apply(df[, modalities], 2, sd)
	
	xres <- numeric(nrow(df))
	
	for (i in 1:nrow(df)) {
		this_word <- rnorm(5, mean = xmeans, sd = xsds)		
		if (any(this_word > 5)) {				# cut-off to preserve to range given by Lynott & Connell
			this_word[this_word > 5] <- 5
			}
		if (any(this_word < 0)) {
			this_word[this_word < 0] <- 0
			}
		if (sum(this_word) != 0) {
			xres[i] <- diff(range(this_word)) / sum(this_word)		# save exclusivity	
			} else {		# if all five modalities are below 0 then exclusivity is 0 as well
				xres[i] <- 0
				}
		}
	return(xres)	
	}
