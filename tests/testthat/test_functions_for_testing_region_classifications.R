test_that("function for testing regional classifications produce logical vectors",{
  expect_equal(length(is_region_code(c("KU005", "R2D2"))), 2)
  expect_equal(length(is_region_name(c("Kajaani", "Coruscant"))), 2)
  expect_equal(is.logical(is_region_code(c("KU005", "R2D2"))), TRUE)
  expect_equal(is.logical(is_region_name(c("Kajaani", "Coruscant"))), TRUE)
})

test_that("codes_to_names gives NA for unrecognized code", {
  expect_equal(codes_to_names("R2D2", set_region_codes = FALSE), as.character(NA))
})

test_that("names_to_codes gives NA for unrecognized name", {
  expect_equal(names_to_codes("Coruscant"), as.character(NA))
})
