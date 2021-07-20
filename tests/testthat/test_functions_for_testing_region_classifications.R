test_that("function for testing regional classifications produce logical vectors",{
  expect_equal(length(is_region_code(c("KU005", "R2D2"))), 2)
  expect_equal(length(is_region_name(c("Kajaani", "Coruscant"))), 2)
  expect_equal(is.logical(is_region_code(c("KU005", "R2D2"))), TRUE)
  expect_equal(is.logical(is_region_name(c("Kajaani", "Coruscant"))), TRUE)
})
