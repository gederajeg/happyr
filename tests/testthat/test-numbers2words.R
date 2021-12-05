context("test-numbers2words.R")

test_that("numbers2words produces string", {
  expect_type(numbers2words(20L), "character")
  expect_match(numbers2words(15), "fifteen")
  expect_match(numbers2words(351), "three hundred and fifty one")
  expect_identical(length(unlist(strsplit(numbers2words(99), " "))), 2L)
  expect_identical(length(unlist(strsplit(numbers2words(351), " "))), 5L)
  expect_match(numbers2words(2000000), "two million")
  expect_match(numbers2words(200000000), "two hundred million")
})

test_that("numbers2words can produce error", {
  expect_error(numbers2words(200000000000000000000), "is too large\\!")
})

test_that("numbers2words can handle multiple input vector", {
  expect_equal(length(numbers2words(c(1, 2))), 2L)
})
