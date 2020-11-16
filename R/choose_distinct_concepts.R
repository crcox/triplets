choose_distinct_concepts_maxFun <- function(D, nwords = 30L, niter = 1e3L, max.fun = "sum") {
  m_best <- 0
  ix_best <- numeric(nwords)
  d_best <- matrix(0, nrow=nwords, ncol=nwords)
  for (i in 1:niter) {
    ix <- sample(1:ncol(D), size = nwords, replace = FALSE)
    d <- D[ix, ix]
    v <- d[lower.tri(d)]
    m <- do.call(max.fun, list(v))
    if (m > m_best) {
      ix_best <- ix
      d_best <- d
      m_best <- m
    }
  }
  return(list(
    objective = m_best,
    ix = ix_best,
    d = d_best
  ))
}

choose_distinct_concepts_iterMaxSum <- function(D, nwords, init = NULL) {
  n <- nrow(D)
  x <- seq_len(nrow(D))
  w <- if (is.null(init)) sample(x, 1) else init
  while (length(w) < nwords) {
    d <- colSums(D[w, , drop = FALSE])
    d[w] <- NA
    w <- c(w, which.max(d), use.names = FALSE)
  }
  if (is.null(rownames(D)))
    return(list(words = NULL, ix = w))
  else
    return(list(words = rownames(D)[w], ix = w))
}

summarize_simulation_dist <- function(Sims, FullDistMat, stats = c('mean', 'median', 'min', 'max')) {
  g <- function(sim, D, stats, standardize) {return(dstats(sim$ix, D, stats, standardize))}
  h <- function(stat, x, x0 = 1) {hist(x[stat, ] / x0[stat], main = stat, xlab = "distance")}
  stats.full <- dstats(1:nrow(FullDistMat), FullDistMat, stats)
  stats.out <- vapply(Sims, g, numeric(4), D = FullDistMat, stats = stats, standardize = FALSE)
  return(list(stats = stats.out, stats.full = stats.full))
}
plot_simulation_dist <- function(stats, .scale = 1, .mfrow = c(2, 2)) {
  h <- function(stat) {hist(x[[stat]], main = stat, xlab = "distance")}
  old <- par(mfrow = .mfrow)
  if (length(.scale) > 1) {
    stopifnot(dim(stats)[1] == length(.scale))
  }
  f <- factor(rownames(stats), levels = rownames(stats))
  x <- split(stats / .scale, f)
  invisible(lapply(names(x), FUN = h))
  par(old)
}
best_simulation_dist <- function(Sims, Stats, stat = "mean") {
  return(Sims[[which.max(Stats$stats[stat, ])]])
}
