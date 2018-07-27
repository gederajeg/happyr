context("test-numbers2words.R")

test_that("numbers2words produces string", {
  expect_type(numbers2words(20L), "character")
  expect_match(numbers2words(1000000), "one million")
  expect_match(numbers2words(351), "three hundred and fifty one")
  expect_identical(length(unlist(strsplit(numbers2words(99), " "))), 2L)
  expect_identical(length(unlist(strsplit(numbers2words(351), " "))), 5L)
})
