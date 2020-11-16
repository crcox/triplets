get_distances <- function(ref, i, j, D, as_df = FALSE) {
  get_dists <- function(ref, i, j, D) {
    return(D[ref, c(i, j)])
  }
  x <- mapply(get_dists, ref, i, j, MoreArgs = list(D = D))
  if (as_df)
      return(data.frame("ref" = ref, "i" = i, "j" = j, "dist_ref_i" = x[1, ], "dist_ref_j" = x[2, ]))
  else
      return(x)
}
dstats <- function(ix, FullDistMat, stats = c('mean', 'median', 'min', 'max'), standardize = FALSE) {
    d <- FullDistMat[ix, ix]
    v <- d[lower.tri(d)]
    y <- vapply(stats, do.call, FUN.VALUE = numeric(1), as.list(v))
    if (standardize) {
        vf <- FullDistMat[lower.tri(FullDistMat)]
        x <- vapply(stats, do.call, FUN.VALUE = numeric(1), as.list(vf))
        return(y / x)
    } else {
        return(y)
    }
}
summarize_distances <- function(x, y = NULL) {
    X <- if (is.null(y)) x else rbind(x, y)
    distance_summary <- summary(c(X))
    strength_summary <- summary(colStrengths(X))
    par(mfrow = c(1, 2))
    hist(c(X))
    hist(colStrengths(X))
    return(list(distance_summary, strength_summary))
}
strength <- function(x, y) {
  return(pmax(x, y) / (x + y))
}
rowStrengths <- function(x) {
  return(strength(x[, 1], x[, 2]))
}
colStrengths <- function(x) {
  return(strength(x[1, ], x[2, ]))
}
which_top_n <- function(x, n) {
    m <- length(x) - n
    return(rank(x, ties.method = "first") > m)
}
which_mid_n <- function(x, n) {
    a <- floor((length(x) - n) / 2)
    b <- n + a + 1
    r <- rank(x, ties.method = "first")
    return(r > a & r < b)
}
