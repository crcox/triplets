# devtools::install_github("crcox/triplets")
library('triplets')

# Load data ----
data("legacy3D")
str(legacy3D)

# Distance matrix ----
rownames(legacy3D) <- legacy3D$word
D.full <- as.matrix(dist(legacy3D[, -1]))

# Triplet indices for 30 words ----
tt <- triplets(30)

# Simulate many selections using different strategies ----
# a. Global optimization of the sum of distances
# --> Sample N words at random
# --> Sum the pairwise differences
# --> Repeat, retaining the N words with largest sum.
words.30.simA <- replicate(1e3, choose_distinct_concepts_maxFun(D.full, 30, niter = 1e4, max.fun = "sum"), simplify = FALSE)
words.30.simA.stats <- summarize_simulation_dist(words.30.simA, D.full)
with(words.30.simA.stats, plot_simulation_dist(stats, stats.full))

# b. Iterative optimization of the sum of distances
# --> Sample one word at random
# --> Find the word with the largest distance from this word.
# --> Then find the word with the largest sum of difference to these two words.
# --> Repeat until N words hvae been selected.
words.30.simB <- replicate(1e3, choose_distinct_concepts_iterMaxSum(D.full, 30), simplify = FALSE)
words.30.simB.stats <- summarize_simulation_dist(words.30.simB, D.full)
with(words.30.simB.stats, plot_simulation_dist(stats, stats.full))

# Select 30 words ----
# The iterative method is more consistent at selecting words that are all well
# spread from each other.
words.30 <- best_simulation_dist(words.30.simB, words.30.simB.stats)
D.30 <- D.full[words.30$ix, words.30$ix]

# Generate all triplets for the 30 words selected ----
# Strength is pmax(x, y) / x + y, where x and y are the distances of the two
# options from the reference word.
tt.mat <- triplets(30)
tt <- get_distances(tt.mat[1, ], tt.mat[2, ], tt.mat[3, ], D.30, as_df = TRUE)
tt$strength <- strength(tt$dist_ref_i, tt$dist_ref_j)
hist(tt$strength, freq = FALSE)

# Select a validation set ----
# We want the validation set to have a strength distribution that is similar to
# the full set of triplets. We especially do not want to over or under sample
# triplets with very high or very low strengths.
tt.samp.mat <- triplets(nrow(D), 6)
tt.samp <- get_distances(tt.samp.mat[1, ], tt.samp.mat[2, ], tt.samp.mat[3, ], D.full, as_df = TRUE)
tt.samp$strength <- strength(tt.samp$dist_ref_i, tt.samp$dist_ref_j)
hist(tt.samp$strength, freq = FALSE, ylim = c(0, 6))

# We also would like good representation of words as reference and as options
table(c(tt.samp$i, tt.samp$j))
table(tt.samp$ref)
