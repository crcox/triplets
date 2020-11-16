test_that("the iterative distance maximization rule works as expected", {
  DistMat <- rbind(
      c(0, 2, 1, 3),
      c(2, 0, 2, 1),
      c(1, 2, 0, 4),
      c(3, 1, 4, 0)
  )
  expect_equal(choose_distinct_concepts_iterMaxSum(DistMat, 4, 1)$ix, c(1, 4, 3, 2))
})
