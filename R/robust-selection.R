#############################################################
# Calculation of deterministic index and robust selection
#
# Calculation of deterministic indices uses economic weights
# and genetic data from IO009 tables 1, 2 and 3
# and Kumar interview to fill gaps.
#
# Luis A. Apiolaza
#############################################################

library(tidyverse)
source('./R/selection-index.R')
source('./R/miscellaneous-auxiliary-functions.R')

set.seed(20190131) # Reproducibility to 20190131 run

# File with breeding values
candidates <- read_csv('./data/bvs.csv')

### Calculating index values
# This assumes that breeding values are in columns 2 to 6
# and in order 'DBH', 'STR', 'BR9', 'DEN', 'MoE'
candidates_bv <- as.matrix(candidates[,2:6])
candidates$idxS <- as.vector(candidates_bv %*% bDS)

#############################################
# Simulating random relative economic weights
# for robust selection
#############################################

## Controlling simulation
# Number of simulations
nsim <- 1000

# Minimum mode and maximum for relative economic weights
volwt <- c(0, 4247, 8494)
swewt <- c(-998, -499, 0)
brswt <- c(-110, -55, 0)
moewt <- c(0, 700, 1400)

# Number of candidates with breeding values and their IDs
n_bvs <- nrow(candidates_bv)
ids <- candidates[,1]

# Matrices for storing ranks, index values, and their summary stats
candidates_rank <- matrix(0, nrow = n_bvs, ncol = nsim)
candidates_idx <- candidates_rank

summary_rank <- matrix(0, nrow = n_bvs, ncol = 4)

######################
### Core of simulation
######################
for(sim in 1:nsim) {
	# Random economic weights
	rweights <-c(rtriang(volwt[1], volwt[2], volwt[3]),
	             rtriang(swewt[1], swewt[2], swewt[3]),
	             rtriang(brswt[1], brswt[2], brswt[3]),
	             rtriang(moewt[1], moewt[2], moewt[3]))

	# Random index coefficients -> random selection index
	rcoefs <- solve(Gss) %*% Gso %*% rweights
	idx <- candidates_bv %*% rcoefs

  # Storing ranking and index
	candidates_rank[, sim] <- rank(-idx)
	candidates_idx[, sim] <- idx
}


### Descriptive stats for rankings
for(geno in 1:nrow(candidates_rank)){
	summary_rank[geno,] <- describe(candidates_rank[geno,])
}

# Fixing presentation	of results
summary_rank <- as.tibble(summary_rank)
names(summary_rank) <- c('best_ranking', 'worst_ranking', 'median_ranking', 'sd_ranking')
summary_rank$Ortet <- candidates$Ortet

# Merging original breeding values with robust selection results,
# sorting by median ranking value. Setting the variable robust
# to TRUE if ranking never below to average
candidates %>%
  left_join(summary_rank, by = 'Ortet') %>%
  arrange(median_ranking) %>%
  mutate(robust = ifelse(worst_ranking < n_bvs/2, TRUE, FALSE)) -> candidates_choices

write_csv(candidates_choices, './reports/candidates-choices.csv')
