context("test-ttr.R")

test_that("`ttr()` produces six column outputs and 62 rows of metaphors type", {
  expect_identical(dim(ttr(phd_data_metaphor))[2], 6L)
  expect_identical(dim(ttr(phd_data_metaphor))[1], 62L)
})

test_that("the column names of `ttr()` outputs", {
  expect_output(str(ttr(phd_data_metaphor)), "\\$ (metaphors|(perc_)?token|(perc_)?type(_lu|_per_token_lu)?)", all = TRUE, perl = TRUE)
})

test_that("the perc data type is double and the other numeric data is integer", {
  expect_type(ttr(phd_data_metaphor)[["perc_token"]], "double")
  expect_type(ttr(phd_data_metaphor)[["token"]], "integer")
})
