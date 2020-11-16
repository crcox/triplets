test_that("all triplets are unique", {
  expect_false(any(duplicated(triplets(10), MARGIN = 2)))
})

test_that("all triplets are generated", {
  expect_equal(ncol(triplets(10)), n_triplets(10))
})

test_that("all indexes are >0 and <=n", {
  expect_true(all(triplets(10) > 0 & triplets(10) <= 10))
})
