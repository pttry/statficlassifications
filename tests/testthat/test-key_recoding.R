v <- c("a", "b", "a", "c", "b", "b")

var1 <- v

key_data.frame <- data.frame(var1 = letters[1:4],
                            var2 = c("first letter",
                                     "second letter",
                                     "third letter",
                                     "fourth letter"))

key_named_vector <- c("a" = "first letter",
                     "b" = "second letter",
                     "c" = "third letter",
                     "d" = "fourth letter")

# Vector input

test_that("Vector input, data.frame key, by names, names do not match.", {
     expect_equal(key_recode(v, key_data.frame),
                  v
     )
})

test_that("Vector input, data.frame key, by names, names match.", {
  expect_equal(key_recode(var1, key_data.frame),
               c("first letter",  "second letter", "first letter",  "third letter",  "second letter", "second letter")
  )
})

test_that("Vector input, data.frame key, by values.", {
        expect_equal(key_recode(v, key_data.frame, by = "values"),
                     c("first letter",  "second letter", "first letter",  "third letter",  "second letter", "second letter")
                     )
})

test_that("Vector input, data.frame, by values, both ways.", {
  expect_equal(key_recode(key_recode(v, key_data.frame, by = "values"), key_data.frame, by = "values"),
               v
  )
})

test_that("Vector input, named vector key.", {
 expect_equal(key_recode(v, key_named_vector),
              c("first letter",  "second letter", "first letter",  "third letter",  "second letter", "second letter")
 )
})

test_that("Vector input, named vector key, both ways.", {
  expect_equal(key_recode(key_recode(v, key_named_vector), key_named_vector),
               v
  )
})

# Factor input

     f <- factor(c("a", "b", "a", "c", "b", "b"))
     var1 <- f

test_that("Factor input, named vector key.", {
  expect_equal(key_recode(f, key_named_vector),
               factor(c("first letter",  "second letter", "first letter",  "third letter",  "second letter", "second letter"))
  )
})

test_that("Factor input, data.frame key by values.", {
  expect_equal(key_recode(f, key_data.frame, by = "values"),
               factor(c("first letter",  "second letter", "first letter",  "third letter",  "second letter", "second letter"))
  )
})

test_that("Factor input, data.frame key by names.", {
  expect_equal(key_recode(var1, key_data.frame),
               factor(c("first letter",  "second letter", "first letter",  "third letter",  "second letter", "second letter"))
  )
})

# data.frame input

y = rnorm(6)
df <- data.frame(var1 = v,
                 y = y)

test_that("Data.frame input, data.frame key.", {
  expect_equal(key_recode(df, key_data.frame),
               data.frame(var2 = c("first letter",  "second letter", "first letter",  "third letter",  "second letter", "second letter"),
                      y = y)
  )
})

test_that("tibble input, data.frame key.", {
  expect_equal(key_recode(tibble::tibble(df), key_data.frame),
               tibble::tibble(var2 = c("first letter",  "second letter", "first letter",  "third letter",  "second letter", "second letter"),
                          y = y)
  )
})


test_that("Data.frame input, named vector key.", {
  expect_equal(key_recode(df, key_named_vector),
               data.frame(var1 = c("first letter",  "second letter", "first letter",  "third letter",  "second letter", "second letter"),
                      y = y)
  )
})

df <- data.frame(v = v, y = y)

test_that("Data.frame input, data.frame key, by values.", {
  expect_equal(key_recode(df, key_data.frame, by = "values"),
               data.frame(var2 = c("first letter",  "second letter", "first letter",  "third letter",  "second letter", "second letter"),
                      y = y)
  )
})

test_that("Data.frame input, named vector key.", {
  expect_equal(key_recode(df, key_named_vector),
               data.frame(v = c("first letter",  "second letter", "first letter",  "third letter",  "second letter", "second letter"),
                      y = y)
  )
})

test_that("Data.frame input, named vector key, both ways.", {
  expect_equal(key_recode(key_recode(df, key_named_vector), key_named_vector),
               data.frame(df))
})

test_that("Data.frame input, named vector key, leave original", {
  expect_equal(key_recode(df, key_named_vector, add = TRUE),
               data.frame(v.1 = v,
                      v.2 = c("first letter",  "second letter", "first letter",  "third letter",  "second letter", "second letter"),
                      y = y)
  )
})

key <- data.frame(var1 = letters[1:4],
                  var2 = c("first letter",
                           "second letter",
                           "third letter",
                           "fourth letter"),
                  var3 = 1:4)

test_that("Data.frame input, named vector key, multiple to columns in key.", {
  expect_equal(key_recode(df, key, by = "values"),
               data.frame(var2 = c("first letter",  "second letter", "first letter",  "third letter",  "second letter", "second letter"),
                      var3 = c(1,2,1,3,2,2),
                      y = y))
})

df <- data.frame(var1 = v,
                 y = y)

test_that("Data.frame input, named vector key, multiple to columns in key, use to.", {
  expect_equal(key_recode(df, key, to = "var2"),
               data.frame(var2 = c("first letter",  "second letter", "first letter",  "third letter",  "second letter", "second letter"),
                      y = y))
})

# This is not working quite how it maybe should.
test_that("Vector input, named vector key, multiple to columns in key, use to.", {
  expect_equal(key_recode(v, key, by = "values"),
               data.frame(var2 = c("first letter",  "second letter", "first letter",  "third letter",  "second letter", "second letter"),
                      var3 = c(1,2,1,3,2,2)))
})

# THIS IS NOT WORKING!
test_that("Data.frame input, named vector key, multiple to columns in key, use to.", {
  expect_equal(key_recode(df, key, from = "var1", to = "var2"),
               data.frame(var2 = c("first letter",  "second letter", "first letter",  "third letter",  "second letter", "second letter"),
                      y = y))
})

