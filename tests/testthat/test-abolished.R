test_that("join_abolished_mun works", {
  df <- data.frame(kunta_code =  factor(c("KU414", "KU609", "KU429", "KU272")), values = rnorm(4))
  expect_equal(as.character(join_abolished_mun(x = df, "kunta_code")$kunta_code), c("KU005", "KU609", "KU272", "KU272"))
})

test_that("join_abolished_mun works with SSS", {
  df <- data.frame(kunta_code =  factor(c("KU414", "SSS", "KU429", "KU272")), values = rnorm(4))
  expect_equal(as.character(join_abolished_mun(x = df, "kunta_code")$kunta_code), c("KU005", "SSS", "KU272", "KU272"))
})
