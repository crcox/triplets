select_triplets_with_strength <- function(s, each, DistMat) {
  get_triplets_with_strength_for_ref <- function(ref, s, DistMat, n = 1)  {
    v <- DistMat[ref, ]
    ij <- combn(v[-ref], 2) # each pair is a column
    S <- strength(v[ij[1, ]], v[ij[2, ]])
    ix <- order((S - s)^2)
    ij <- ij[, ix]
    S <- S[ix]
    return(t(rbind(ref, ij[, seq_len(n)], S[seq_len(n)])))
  }
  X <- matrix(vapply(nrow(DistMat), select_triplets_with_strength_for_ref, numeric(3 * each),
                     s = s, DistMat = D, n = each), nrow = 3, ncol = nrow(DistMat) * each)
  d <- data.frame(ref = X[1, ], opt1 = X[2, ], opt2 = X[3, ], strength = X[4, ])
  return(d)
}
