test_that("distances are extracted from matrix as expected (as column-bound matrix)", {
  DistMat <- rbind(
      c(0, 2, 1, 3),
      c(2, 0, 2, 1),
      c(1, 2, 0, 4),
      c(3, 1, 4, 0)
  )
  expect_equal(get_distances(c(2, 3, 1), c(3, 3, 2), c(4, 4, 3), D = DistMat),
               cbind(c(2, 1), c(0, 4), c(2, 1)))
})

test_that("distances are extracted from matrix as expected (as data frame)", {
  DistMat <- rbind(
      c(0, 2, 1, 3),
      c(2, 0, 2, 1),
      c(1, 2, 0, 4),
      c(3, 1, 4, 0)
  )
  expect_equal(get_distances(c(2, 3, 1), c(3, 3, 2), c(4, 4, 3), D = DistMat, as_df = TRUE),
               data.frame("ref" = c(2, 3, 1), "i" = c(3, 3, 2), "j" = c(4, 4, 3),
                          "dist_ref_i" = c(2, 0, 2), "dist_ref_j" = c(1, 4, 1)))
})

test_that("strengths are computed accuratly by column", {
  DistMat <- rbind(
      c(0, 2, 1, 3),
      c(2, 0, 2, 1),
      c(1, 2, 0, 4),
      c(3, 1, 4, 0)
  )
  X <- get_distances(c(2, 3, 1), c(3, 3, 2), c(4, 4, 3), D = DistMat)
  expect_equal(colStrengths(X), c(2/3, 4/4, 2/3))
  expect_equal(rowStrengths(t(X)), c(2/3, 4/4, 2/3))
})

test_that("strengths are computed accuratly when x any y are provided as vectors", {
  DistMat <- rbind(
      c(0, 2, 1, 3),
      c(2, 0, 2, 1),
      c(1, 2, 0, 4),
      c(3, 1, 4, 0)
  )
  d <- get_distances(c(2, 3, 1), c(3, 3, 2), c(4, 4, 3), D = DistMat, as_df = TRUE)
  expect_equal(strength(d$dist_ref_i, d$dist_ref_j), c(2/3, 4/4, 2/3))
  expect_equal(strength(d$dist_ref_j, d$dist_ref_i), c(2/3, 4/4, 2/3))
})
