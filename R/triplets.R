n_triplets <- function(n) {
  return(n * choose(n - 1, 2))
}
triplets <- function(n, each = choose(n - 1, 2)) {
  tripletsWithRef <- function(ref, n, size) {
    N <- choose(n - 1, 2)
    ix <- if (size < N) sort(sample(N, size)) else seq_len(N)
    return(rbind(ref, combn(seq_len(n)[-ref], 2))[, ix])
  }
  return(do.call('cbind', lapply(seq_len(n), tripletsWithRef, n = n, size = each)))
}
