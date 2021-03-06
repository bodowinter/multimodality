## Bodo Winter
## December 20, 2015
## Analysis for "Sensory words are multimodal, but not extremely so"

##------------------------------------------------------------------
## Load in data and preprocessing:
##------------------------------------------------------------------

## Load in packages:

library(dplyr)
library(lme4)
library(MuMIn)

## Source custom plotting functions:

setwd('/Users/winterb/Research/senses_sensory_modalities/multimodality/analysis/')
source('plotting_functions.R')

## Load in modality norms:

l <- read.csv('lynott_connell_2009_adj_norms.csv')
n <- read.csv('lynott_connell_2013_noun_norms.csv')
v <- read.csv('winter_2016_verb_norms.csv')

## Load in valence norms:

val <- read.csv('warriner_2013_valence_norms.csv')

## Compute absolute valence:

val <- val %>% rename(Val = V.Mean.Sum) %>%
	mutate(AbsV = abs(Val - mean(Val)))

## Vector of modality names:

modalities <- grep('StrengthMean', colnames(l), value = T)

## Load in COCA data:

COCA <- read.csv('COCA_adj_noun.csv')

## Create unique adjective-noun pair identifier:

COCA$ID <- paste(COCA$Word, COCA$Noun, sep = ':')

## Create super-set of all possible adjective-noun combinations:

adj <- data.frame(Word = rep(l$Word, each = nrow(n)),
	Noun = rep(n$Word, times = nrow(l)))

## Create unique pair identifier:

adj$ID <- paste(adj$Word, adj$Noun, sep = ':')

## Create a column for whether it is attested or not:

adj$Attested <- 'no'
attesteds <- adj$ID %in% COCA$ID
adj[attesteds, ]$Attested <- 'yes'
rm(attesteds)

## Add adjective perceptual strengths:

adj_norms <- l[match(adj$Word, l$Word),
	c(modalities, 'DominantModality')]
colnames(adj_norms) <- paste0('Adj', colnames(adj_norms))
adj <- cbind(adj, adj_norms)
rm(adj_norms)

## Add noun perceptual strengths:

noun_norms <- n[match(adj$Noun, n$Word),
	c(modalities, 'DominantModality')]
colnames(noun_norms) <- paste0('Noun', colnames(noun_norms))
adj <- cbind(adj, noun_norms)
rm(noun_norms)

## Change rownames back to normal:

rownames(adj) <- 1:nrow(adj)

## Add COCA frequency to adj:

adj$Freq <- COCA[match(adj$ID, COCA$ID), ]$Freq

## Compute log frequency:

adj <- mutate(adj, LogFreq = log10(Freq))



##------------------------------------------------------------------
## Modality norms reported in Lynott & Connell (2009):
##------------------------------------------------------------------

## Overall effect of modality differences:

anova(xmdl <- lm(ModalityExclusivity ~ DominantModality, l))

## Get predictions:

mod_order <- c('Visual', 'Haptic', 'Auditory', 'Gustatory', 'Olfactory')
newpreds <- data.frame(DominantModality = mod_order)
newpreds$fit <- predict(xmdl, newpreds)
newpreds$SE <- predict(xmdl, newpreds, se.fit = T)$se.fit
newpreds$UB <- newpreds$fit + newpreds$SE * 1.96
newpreds$LB <- newpreds$fit - newpreds$SE * 1.96

## Order by exclusivity:

newpreds <- newpreds[order(newpreds$fit), ]

## Graph of this:

setup_plots(N = 1)
emptyplot(xlim = c(0, 6), ylim = c(0.25, 0.75), yaxs = 'i')
left_axis(text = '', at = seq(0, 1, 0.25), type = 1)
mtext(text = 'Exclusivity (%)', side = 2, line = 5,
	font = 2, cex = 2)
axis(side = 1, at = 1:5,
	labels = substr(newpreds$DominantModality, 1, 3),
	font = 2, cex.axis = 1.5, lwd.ticks = 2)
segments(x0 = 1:5, x1 = 1:5,
	y0 = newpreds$LB, y1 = newpreds$UB, lwd = 2)
points(1:5, newpreds$fit, pch = 15, cex = 1.5)

## Make a timo-style plot with barplots:

xcols <- c('#FF655E', '#FF894B', '#FDBD54',
	'#385DA7', '#00BC81')
xfac <- 0.1
yfac <- 0.02
i <- 5
quartz('', 10.5, 6.5)
par(mai = c(0, 0.1, 0.2, 0.1))
plot(1, 1, type = 'n', xlim = c(0, 6), ylim = c(0.25, 0.75),
	bty = 'n', xlab = '', ylab = '',
	xaxt = 'n', yaxt = 'n')
if(i >= 1){
rect(xleft = 0.5 + xfac, xright = 1.5 - xfac, ybottom = 0,
	ytop = newpreds[1, 'fit'], col = xcols[4], border = NA)
	text(x = 1, y = newpreds[1, 'fit'] + yfac,
		labels = paste0(round(newpreds[1, 'fit'], 2) * 100, '%'),
		col = xcols[4], font = 2, cex = 2.6)
	}
if(i > 1 ){
rect(xleft = 1.5 + xfac, xright = 2.5 - xfac, ybottom = 0,
	ytop = newpreds[2, 'fit'], col = xcols[2], border = NA)
	text(x = 2, y = newpreds[2, 'fit'] + yfac,
		labels = paste0(round(newpreds[2, 'fit'], 2) * 100, '%'),
		col = xcols[2], font = 2, cex = 2.6)
	}
if(i > 2){
rect(xleft = 2.5 + xfac, xright = 3.5 - xfac, ybottom = 0,
	ytop = newpreds[3, 'fit'], col = xcols[5], border = NA)
	text(x = 3, y = newpreds[3, 'fit'] + yfac,
		labels = paste0(round(newpreds[3, 'fit'], 2) * 100, '%'),
		col = xcols[5], font = 2, cex = 2.6)	
	}
if(i > 3){
rect(xleft = 3.5 + xfac, xright = 4.5 - xfac, ybottom = 0,
	ytop = newpreds[4, 'fit'], col = xcols[1], border = NA)
	text(x = 4, y = newpreds[4, 'fit'] + yfac,
		labels = paste0(round(newpreds[4, 'fit'], 2) * 100, '%'),
		col = xcols[1], font = 2, cex = 2.6)	
	}
if(i > 4){
rect(xleft = 4.5 + xfac, xright = 5.5 - xfac, ybottom = 0,
	ytop = newpreds[5, 'fit'], col = xcols[3], border = NA)
	text(x = 5, y = newpreds[5, 'fit'] + yfac,
		labels = paste0(round(newpreds[5, 'fit'], 2) * 100, '%'),
		col = xcols[3], font = 2, cex = 2.6)	
	}



##------------------------------------------------------------------
## Creating a table a la Ullman:
##------------------------------------------------------------------

## Load in Strik Lievers (2015) nouns:

strik <- read.csv('strik_lievers_2015_nouns_with_freq.csv')
strik <- filter(strik, Instrument == 'no')

## Merge:

COCA_strik <- COCA
COCA_strik$NounMod <- strik[match(COCA_strik$Noun, strik$Word), ]$Modality
COCA_strik$AdjMod <- l[match(COCA_strik$Word, l$Word), ]$DominantModality
COCA_strik <- COCA_strik %>% filter(!is.na(COCA_strik$NounMod))

## Create a type table:

type_table <- table(COCA_strik$AdjMod, COCA_strik$NounMod)
type_table <- type_table[c(3, 2, 4, 5, 1), c(3, 2, 4, 5, 1)]

## Check which cells are significantly different:

x.chisq <- chisq.test(type_table)
x.chisq$streds

## Make a 0 diagonal one for proportion:

type_table0 <- type_table
diag(type_table0) <- 0
M <- matrix(c(F, T, T, T, T,
	F, F, T, T, T,
	F, F, F, T, T,
	F, F, F, F, T,
	F, F, F, T, F), nrow = 5, byrow = T)
sum(type_table0[M]) / (sum(type_table0[!M]) + sum(type_table0[M]))

## Check which cells are significantly different:

x0.chisq <- chisq.test(type_table0)
x0.chisq$stdres



##------------------------------------------------------------------
## Section 2, randomizing modality norms:
##------------------------------------------------------------------

## Source custom random norm generation functions:

source('random_functions.R')

## Take descriptive means (Table 1):

apply(l[, grep('StrengthMean', colnames(l))], 2, mean)	# adj
apply(n[, grep('StrengthMean', colnames(n))], 2, mean)	# nouns
apply(v[v$RandomSet == 'no',
	grep('StrengthMean', colnames(v))], 2, mean)	# verbs 1
apply(v[v$RandomSet == 'yes',
	grep('StrengthMean', colnames(v))], 2, mean)	# verbs 2

## Results vectors for storing randomly generated exclusivities:

nrep <- 1000
l_excl <- numeric(nrep)
n_excl <- numeric(nrep)
v_excl <- numeric(nrep)

## Create a population of modality exclusivities:

set.seed(42)
for (i in 1:nrep) {
	l_excl[i] <- mean(random_excl1(l))
	n_excl[i] <- mean(random_excl1(n))
	v_excl[i] <- mean(random_excl1(v))
	if (i %% 100 == 0) cat(paste(i, '\n'))
	}

## Make a plot of the exclusivity distributions:

quartz('', 11, 4)
par(mfrow = c(1,3), mai = c(0.25, 0.25, 0.25, 0.25),
	omi = c(0.6, 0.6, 0.25, 0.25))
# Plot 1
plot(1, 1, type = 'n', xlim = c(0.3, 0.6), ylim = c(0, 100), yaxs = 'i',
	xlab = '', ylab = '', xaxt = 'n', yaxt = 'n')
axis(side = 1, at = seq(0.3, 0.6, 0.1), font = 2, lwd.ticks = 2, cex.axis = 1.35)
mtext(side = 2, line = 1.5, text = 'Density', font = 2, cex = 1.75)
mtext(side = 1, line = 3.5, text = 'Modality Exclusivity', font = 2, cex = 1.5)
plot_density(na.omit(l_excl), mean = F)
abline(v = mean(l$ModalityExclusivity), lwd = 2, lty = 2)
box(lwd = 2)
mtext(side = 3, text = 'Adjectives', font = 2, line = 1, cex = 1.5)
# Plot 2
plot(1, 1, type = 'n', xlim = c(0.3, 0.6), ylim = c(0, 100), yaxs = 'i',
	xlab = '', ylab = '', xaxt = 'n', yaxt = 'n')
axis(side = 1, at = seq(0.3, 0.6, 0.1), font = 2, lwd.ticks = 2, cex.axis = 1.35)
mtext(side = 1, line = 3.5, text = 'Modality Exclusivity', font = 2, cex = 1.5)
plot_density(na.omit(n_excl), mean = F)
abline(v = mean(n$ModalityExclusivity), lwd = 2, lty = 2)
box(lwd = 2)
mtext(side = 3, text = 'Nouns', font = 2, line = 1, cex = 1.5)
# Plot 3
plot(1, 1, type = 'n', xlim = c(0.3, 0.6), ylim = c(0, 100), yaxs = 'i',
	xlab = '', ylab = '', xaxt = 'n', yaxt = 'n')
axis(side = 1, at = seq(0.3, 0.6, 0.1), font = 2, lwd.ticks = 2, cex.axis = 1.35)
mtext(side = 1, line = 3.5, text = 'Modality Exclusivity', font = 2, cex = 1.5)
plot_density(na.omit(v_excl), mean = F)
abline(v = mean(v[v$RandomSet == 'no', ]$ModalityExclusivity), lwd = 2, lty = 2)
abline(v = mean(v[v$RandomSet == 'yes', ]$ModalityExclusivity), lwd = 2, lty = 3, col = 'darkgrey')
box(lwd = 2)
mtext(side = 3, text = 'Verbs', font = 2, line = 1, cex = 1.5)

## Significance (simulated p-value):

sum((l_excl >= mean(l$ModalityExclusivity)))
(1000 - sum((n_excl <= mean(n$ModalityExclusivity)))) / nsim
sum((v_excl >= mean(v[v$RandomSet == 'no', ]$ModalityExclusivity)))
(1000 - sum((v_excl >= mean(vrand$ModalityExclusivity)))) / nsim



##------------------------------------------------------------------
## Section 3, cosine analysis:
##------------------------------------------------------------------

## Cosine similarity function:

cosine_sim <- function(x, y) {
	numerator <- x %*% y
	denominator <- sqrt(x %*% x * y %*% y)
	return(as.vector(numerator / denominator))
	}

## Create empty cosine column to fill with modality fit:

adj$Cosine <- NA

## Loop through and compute the cosine of the adj and the noun modality strenths:

for (i in 1:nrow(adj)) {
	A <- unlist(adj[i, grep('Noun(.)+StrengthMean', colnames(adj))])
	B <- unlist(adj[i, grep('Adj(.)+StrengthMean', colnames(adj))])
	adj[i, ]$Cosine <- cosine_sim(A, B)
	if (i %% 1000 == 0) {cat(paste(i, '\n'))}
	}

## Write to table:

# write.table(adj, 'combinations_with_cosines.csv',
	# sep = ',', row.names = F)
adj <- read.csv('combinations_with_cosines.csv')

## Make a plot of the cosine distribution for the attested pairs:

setup_plots(N = 1)
emptyplot(xlim = c(0, 1), ylim = c(0, 5), yaxs = 'i')
plot_density(adj[adj$Attested == 'yes', ]$Cosine,
	powerpoint = F, mean = F)
left_axis(text = 'Density', at = seq(0, 5, 1), type = 1)
mtext(side = 3, line = 1, text = 'Modality Compatibility',
	cex = 1.5, font = 2)
lower_axis(style = 'continuous', lab = 'Cosine Similarity',
	at = seq(0, 1, 0.25), type = 1)
box(lwd = 2)
segments(x0 = 0.95, x1 = 0.65, y0 = 2, y1 = 3, lwd = 3)
text(x = 0.65, y = 3.15, labels = 'abrasive contact',
	font = 2, cex = 1.35)
segments(x0 = 0.32, x1 = 0.25, y0 = 0.1, y1 = 1, lwd = 3)
text(x = 0.25, y = 1.15, labels = 'sweet music',
	font = 2, cex = 1.35)

## Compare cosines of unattested and attested combinations:

aggregate(Cosine ~ Attested, adj, mean)
wilcox.test(Cosine ~ Attested, adj)
t.test(Cosine ~ Attested, adj)

## Cosine by frequency linear model:

summary(cos_freq_lmer <- lmer(LogFreq ~ Cosine +
	(1 + Cosine|Word) + (1 + Cosine|Noun),
	adj[adj$Attested == 'yes', ], REML = F))
summary(cos_freq_lmer_null <- lmer(LogFreq ~ 1 +
	(1 + Cosine|Word) + (1 + Cosine|Noun),
	adj[adj$Attested == 'yes', ], REML = F))
# summary(cos_freq_lmer_pois <- glmer(LogFreq ~ Cosine +
	# (1 + Cosine|Word) + (1|Noun), adj, family = 'poisson'))

## Likelihood ratio test:

anova(cos_freq_lmer_null, cos_freq_lmer, test = 'Chisq')

## R-squareds:

r.squaredGLMM(cos_freq_lmer)
r.squaredGLMM(cos_freq_lmer_pois)

## Cosine by frequency graph:

setup_plots(N = 1)
emptyplot(xlim = c(0, 1), ylim = c(-0.5, 5), yaxs = 'i')
lower_axis(style = 'continuous', lab = '',
	at = seq(0, 1, 0.25), type = 1)
mtext('Cosine Similarity', line = 3,
	cex = 1.8, font = 2, side = 1)
mtext('Frequency (log10)', line = 3,
	cex = 1.8, font = 2, side = 2)
left_axis(text = '', at = seq(0, 5, 1), type = 1)
points(adj$Cosine, adj$LogFreq,
	col = rgb(0, 0, 0, 0.4), pch = 19)
box(lwd = 2)

## Add weights to data frame:

adj$Weight <- adj_red$Weight

## Check cosine of different modality strengths:

cos_agr <- aggregate(Cosine ~ Word, adj, mean, w = Weight)
cos_agr$DominantModality <- l[match(cos_agr$Word,
	l$Word), ]$DominantModality
cos_agr$ModalityExclusivity <- l[match(cos_agr$Word,
	l$Word), ]$ModalityExclusivity

## Take aggregate cosine per modality:

aggregate(Cosine ~ DominantModality, cos_agr, mean)
summary(xmdl <- lm(Cosine ~ DominantModality, cos_agr))
anova(xmdl)

## Corelate the average cosine with the exclusivity:

with(cos_agr, cor.test(Cosine, ModalityExclusivity))
anova(xmdl)
with(cos_agr, plot(Cosine, ModalityExclusivity))

## Correlate modality exclusivity with cosine similarity:

summary(xmdl <- lm(Cosine ~ ModalityExclusivity, cos_agr))
anova(xmdl)

## Make a plot of this:

setup_plots(N = 1)
emptyplot(xlim = c(0, 1), ylim = c(0, 1.1), yaxs = 'i')
lower_axis(style = 'continuous', lab = '',
	at = seq(0, 1, 0.25), type = 1)
mtext('Cosine Similarity', line = 3,
	cex = 1.8, font = 2, side = 1)
mtext('Modality Exclusivity', line = 5,
	cex = 1.8, font = 2, side = 2)
left_axis(text = '', at = seq(0, 1, 0.25), type = 1)
# points(cos_agr$Cosine, cos_agr$ModalityExclusivity,
	# col = rgb(0, 0, 0, 0.4), pch = 19)
text(cos_agr$Cosine, cos_agr$ModalityExclusivity,
	labels = cos_agr$Word, col = as.numeric(cos_agr$DominantModality))
box(lwd = 2)

## Add valence to this:

cos_agr$AbsV <- val[match(cos_agr$Word, val$Word), ]$AbsV
cos_agr$Val <- val[match(cos_agr$Word, val$Word), ]$Val

## Make a plot of valence against an adjective's cosine similarity:

setup_plots(N = 1)
emptyplot(xlim = c(0, 1), ylim = c(0, 3.5), yaxs = 'i')
lower_axis(style = 'continuous', lab = '',
	at = seq(0, 1, 0.25), type = 1)
mtext('Cosine Similarity', line = 3,
	cex = 1.8, font = 2, side = 1)
mtext('Absolute Valence', line = 5,
	cex = 1.8, font = 2, side = 2)
left_axis(text = '', at = seq(0, 3.5, 1), type = 1)
# points(cos_agr$Cosine, cos_agr$AbsV,
	# col = rgb(0, 0, 0, 0.4), pch = 19)
text(cos_agr$Cosine, cos_agr$AbsV,
	labels = cos_agr$Word,
	col = as.numeric(cos_agr$DominantModality))
box(lwd = 2)

## Make a 3D plot of the relationship between these three variables:

library(rgl)	# need to start XQuartz before
plot3d(cos_agr$Cosine,
	cos_agr$ModalityExclusivity,
	cos_agr$AbsV)

library(plotly)
plot_ly(cos_agr, x = ~Cosine, y = ~ModalityExclusivity,
	z = ~AbsV)

## Make a model:

adj$AbsV <- val[match(adj$Word, val$Word), ]$AbsV
summary(lm(LogFreq ~ AbsV * Cosine,
	adj[adj$Attested == 'yes', ]))




##------------------------------------------------------------------
## Section 4, correlations:
##------------------------------------------------------------------

## Easiest to now work with subset of attested combinations:

adj_red <- adj[adj$Attested == 'yes', ]

## For creating weights, create frequencies:

adj_freqs <- aggregate(Freq ~ Word, adj_red, sum)

## For creating weights, merge the frequencies into the table:

adj_red$AdjFreq <- adj_freqs[match(adj_red$Word, adj_freqs$Word), ]$Freq

## Create weights:

adj_red <- mutate(adj_red, Weight = Freq / AdjFreq)

## Get the reducd dataset with averages (weighted mean):

group_by(adj_red, Word) %>% summarise(Freq = sum(Freq),
	AdjVisualStrengthMean = mean(AdjVisualStrengthMean),
	AdjAuditoryStrengthMean = mean(AdjAuditoryStrengthMean),
	AdjHapticStrengthMean = mean(AdjHapticStrengthMean),
	AdjGustatoryStrengthMean = mean(AdjGustatoryStrengthMean),
	AdjOlfactoryStrengthMean = mean(AdjOlfactoryStrengthMean),
	NounVisualStrengthMean = weighted.mean(NounVisualStrengthMean,
		w = Weight),
	NounAuditoryStrengthMean = weighted.mean(NounAuditoryStrengthMean,
		w = Weight),
	NounHapticStrengthMean = weighted.mean(NounHapticStrengthMean,
		w = Weight),
	NounGustatoryStrengthMean = weighted.mean(NounGustatoryStrengthMean,
		w = Weight),
	NounOlfactoryStrengthMean = weighted.mean(NounOlfactoryStrengthMean,
		w = Weight)) -> adj_agr

## Set up matrix for first-order correlations:

noun_cors <- matrix(rep(NA, 5 * 5), nrow = 5)
rownames(noun_cors) <- paste0('Adj', modalities)
colnames(noun_cors) <- paste0('Noun', modalities)

## Set up matrix for p-values:

noun_cors_pvals <- matrix(rep(NA, 5 * 5), nrow = 5)
rownames(noun_cors_pvals) <- rownames(noun_cors)
colnames(noun_cors_pvals) <- colnames(noun_cors)

## Loop through matrix to fill each cell with first order correlations:

for (i in 1:5) {
	adjective_modality <- rownames(noun_cors)[i]
	
	for (j in 1:5) {
		noun_modality <- colnames(noun_cors)[j]
		
		cor.temp <- cor.test(unlist(adj_agr[,adjective_modality]),
			unlist(adj_agr[,noun_modality]))
		
		noun_cors[i, j] <- cor.temp$estimate	# correlation coefficients
		noun_cors_pvals[i, j] <- cor.temp$p.value		# p-values
		
		}
	}

## Multiple comparisons corrected p-values:

round(noun_cors, 2)
noun_cors_pvals < (0.05/25)

## Create correlations of unattested pairs:

adj_red <- adj[adj$Attested == 'no', ]

## Group:

group_by(adj_red, Word) %>%
	summarise(AdjVisualStrengthMean = mean(AdjVisualStrengthMean),
	AdjAuditoryStrengthMean = mean(AdjAuditoryStrengthMean),
	AdjHapticStrengthMean = mean(AdjHapticStrengthMean),
	AdjGustatoryStrengthMean = mean(AdjGustatoryStrengthMean),
	AdjOlfactoryStrengthMean = mean(AdjOlfactoryStrengthMean),
	NounVisualStrengthMean = mean(NounVisualStrengthMean),
	NounAuditoryStrengthMean = mean(NounAuditoryStrengthMean),
	NounHapticStrengthMean = mean(NounHapticStrengthMean),
	NounGustatoryStrengthMean = mean(NounGustatoryStrengthMean),
	NounOlfactoryStrengthMean = mean(NounOlfactoryStrengthMean)) -> 
		adj_agr

## Set up matrix for first-order correlations:

noun_cors_unat <- matrix(rep(NA, 5 * 5), nrow = 5)
rownames(noun_cors_unat) <- paste0('Adj', modalities)
colnames(noun_cors_unat) <- paste0('Noun', modalities)

## Set up matrix for p-values:

noun_cors_unat_pvals <- matrix(rep(NA, 5 * 5), nrow = 5)
rownames(noun_cors_unat_pvals) <- rownames(noun_cors_unat)
colnames(noun_cors_unat_pvals) <- colnames(noun_cors_unat)

## Loop through matrix to fill each cell with first order correlations:

for (i in 1:5) {
	adjective_modality <- rownames(noun_cors_unat)[i]
	
	for (j in 1:5) {
		noun_modality <- colnames(noun_cors_unat)[j]
		
		cor.temp <- cor.test(unlist(adj_agr[, adjective_modality]),
			unlist(adj_agr[, noun_modality]))
		
		noun_cors_unat[i, j] <- cor.temp$estimate	# correlation coefficients
		noun_cors_unat_pvals[i, j] <- cor.temp$p.value		# p-values
		
		}
	}



