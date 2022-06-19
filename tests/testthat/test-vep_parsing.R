test_that("biotype_rankings works", {
  expect_s3_class(biotype_rankings(),class = "data.frame")
  expect_gt(nrow(biotype_rankings()), expected = 0)
  expect_equal(ncol(biotype_rankings()), expected = 3)
})

test_that("biotype_rankings works", {
  expect_s3_class(effect_rankings(),class = "data.frame")
  expect_gt(nrow(effect_rankings()), expected = 0)
  expect_equal(ncol(effect_rankings()), expected = 3)
})

