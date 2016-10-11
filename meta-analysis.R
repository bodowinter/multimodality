## Bodo Winter
## October 11, 2016
## Analysis of synesthetic metaphor tables

## Options:

options(stringsAsFactors = F)

## Load in data:

setwd('/Users/winterb/Research/senses_sensory_modalities/meta-analysis/tables/')
strik_eng <- read.csv('strik_lievers_2015_german.csv')
strik_ita <- read.csv('strik_lievers_2015_italian.csv')
winter <- read.csv('winter_2016_types.csv')
day_eng <- read.csv('day1996_english.csv')
day_ger <- read.csv('day1996_buddenbrooks.csv')
keats <- read.csv('ullman1945_keats.csv')
byron <- read.csv('ullman1945_byron.csv')

## Load in the Erzsebet ones:

erz1 <- read.csv('erzsebet1976_lorinc.csv')[, -1]
erz2 <- read.csv('erzsebet1976_arpad.csv')[, -1]
erz3 <- read.csv('erzsebet1976_dezso.csv')[, -1]
erz4 <- read.csv('erzsebet1976_gyula.csv')[, -1]
erz5 <- read.csv('erzsebet1976_mihaly.csv')[, -1]

## Find raw counts for Erzsebet ones:
# (smallest minimum number that makes the numbers right)

# ??????

## Get rid of extra spaces in Francesca's table:

strik_eng$modality <- gsub(' ', '', strik_eng$modality)
strik_ita$modality <- gsub(' ', '', strik_ita$modality)

## For Day and Ullman, combine temperature and touch:

keats <- apply(keats[, -1], 2, function(x) ifelse(is.na(x), 0, x))
byron <- apply(byron[, -1], 2, function(x) ifelse(is.na(x), 0, x))
keats[1, ] <- keats[1, ] + keats[2, ]
byron[1, ] <- byron[1, ] + byron[2, ]
keats[, 1] <- keats[, 1] + keats[, 2]
byron[, 1] <- byron[, 1] + byron[, 2]
keats <- keats[-2, -2]
byron <- byron[-2, -2]

day_eng <- apply(day_eng[, -1], 2, function(x) ifelse(is.na(x), 0, x))
day_ger <- apply(day_ger[, -1], 2, function(x) ifelse(is.na(x), 0, x))
day_eng[6, ] <- day_eng[6, ] + day_eng[4, ]
day_ger[6, ] <- day_ger[6, ] + day_ger[4, ]
day_eng[, 6] <- day_eng[, 6] + day_eng[, 4]
day_ger[, 6] <- day_ger[, 6] + day_ger[, 4]
day_eng <- day_eng[-4, -4]
day_ger <- day_ger[-4, -4]

## Structure all the tables the same:

strik_eng <- strik_eng[c(3, 4, 5, 1, 2), c(3, 4, 5, 1, 2) + 1]
strik_ita <- strik_ita[c(3, 4, 5, 1, 2), c(3, 4, 5, 1, 2) + 1]

byron <- byron[, c(1:3, 5, 4)]
byron <- byron[c(1:3, 5, 4), ]

keats <- keats[, c(1:3, 5, 4)]
keats <- keats[c(1:3, 5, 4), ]

day_eng <- t(day_eng)[5:1, 5:1]		# Day (1996) reverses order of target/source
day_ger <- t(day_ger)[5:1, 5:1]

winter <- winter[, -1]

## Create object names:

allobs <- c('strik_eng', 'strik_ita',
	'byron', 'keats', 'day_eng', 'day_ger', 'winter')

## Make all diagonals 0 and perform chisquare tests:

for (i in 1:length(allobs)) {
	x <- get(allobs[i])
	diag(x) <- 0
	assign(allobs[i], x)
	assign(paste0(allobs[i], '.chisq'), chisq.test(x)$stdres)
	}

## Make a 5 * 5 plot of the distributions of all the standardized residuals:

modalities <- c('Touch', 'Taste', 'Smell', 'Sight', 'Sound')
quartz('', 11, 7)
par(mfrow = c(5, 5), mai = rep(0.1, 4), omi = c(0.5, 0.75, 0.5, 0))
for (i in 1:5) {
	these_stdres <- numeric(length(allobs))
	for (j in 1:5) {
		for (k in 1:length(allobs)) {
			these_stdres[k] <- get(paste0(allobs[k], '.chisq'))[i, j]
			}
		dens_obj <- density(these_stdres)	# extract density
		dens <- as.data.frame(dens_obj[1:2])

		if (i != j) {
			if(t.test(these_stdres, mu = 0)$p.val < 0.05) {
				this_color <- 'steelblue'
				} else {
					this_color <- 'lightgray'
					}
			plot(1, 1, type = 'n', xlim = c(-6, 6),
				xlab = '', ylab = '', xaxt = 'n', yaxt = 'n',
				main = '', ylim = c(0, max(dens$y) + 0.25 * max(dens$y)),
				yaxs = 'i')
			polygon(x = c(dens$x, rev(dens$x)),
				y = c(dens$y, rep(0, length(dens$x))),
				col = this_color, border = F)
			abline(v = 0, lty = 2)
			abline(v = mean(these_stdres), lwd = 2)
			} else {
				plot(1, 1, xlim = c(-6, 6),
					type = 'n', xaxt = 'n', yaxt = 'n',
					xlab = '', ylab = '', main = '', bty = 'n')
				}
		# if (j == 1) {
			# mtext(text = modalities[i],
				# side = 2, font = 2, cex = 2, line = 1.25)
			# }
		# if (i == 1) {
			# mtext(text = modalities[j],
				# side = 3, font = 2, cex = 2, line = 1.25)			
			# }
		if (i == 5) {
			axis(side = 1, at = seq(-6, 6, 3), font = 2)
			}
		}
	}

## Define the matrix of fitting cases:

M <- matrix(c(F, T, T, T, T,
	F, F, T, T, T,
	F, F, F, T, T,
	F, F, F, F, T,
	F, F, F, T, F), nrow = 5, byrow = T)
M_inv <- matrix(c(F, F, F, F, F,
	T, F, F, F, F,
	T, T, F, F, F,
	T, T, T, F, F,
	T, T, T, F, T), nrow = 5, byrow = 5)

## Loop through all and get proportion of fitting values:

for (i in 1:length(allobs)) {
	x <- get(allobs[i])
	print(paste(allobs[i], round(sum(x[M]) / sum(x), 2), sep = ' = '))
	}

## Test whether overall the studentized residuals in the upper-right triangle are higher:

upright_means <- numeric(length(allobs))
lowleft_means <- numeric(length(allobs))
for (i in 1:length(allobs)) {
	x <- get(paste0(allobs[i], '.chisq'))
	upright_means[i] <- mean(x[M])
	lowleft_means[i] <- mean(x[M_inv])
	}
t.test(upright_means, lowleft_means)


