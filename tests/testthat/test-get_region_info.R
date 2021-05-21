test_that("get_regionkey() basic works", {
  expect_named(get_regionkey(),
               c("kunta_name", "seutukunta_name", "maakunta_name",
                 "kunta_code", "seutukunta_code", "maakunta_code"))
})


test_that("get_regionkey() named predefined works", {
  expect_named(get_regionkey("kunta", "seutukunta", only_codes = TRUE),
               c("kunta_code", "seutukunta_code"))
})

test_that("get_regionkey() named non-predefined works", {
  expect_named(get_regionkey("kunta", "kuntaryhmitys", only_codes = TRUE, offline = FALSE),
               c("kunta_code", "kuntaryhmitys_code"))
})


