context("test-scaps.R")

strings <- c("concept1", "concept2")
test_that("multiple string-input produces output of length greater than 1L", {
  expect_equal(length(scaps(strings)), length(strings))
})

test_that("unidentified object input returns error", {
  expect_error(scaps(x), "^object.+?not found$", perl = TRUE)
})
