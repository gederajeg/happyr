context("test-ttr.R")

ttr_res <- ttr(df = phd_data_metaphor,
               schema_var = "metaphors",
               lexunit_var = "lu")
metaphor_type_length <- length(unique(phd_data_metaphor$metaphors))
test_that("`ttr()` produces six column outputs and 62 rows of metaphors type", {
  expect_identical(dim(ttr_res)[2], 6L)
  expect_identical(dim(ttr_res)[1], metaphor_type_length)
})

test_that("the column names of `ttr()` outputs", {
  expect_output(str(ttr_res), "\\$ (metaphors|(perc_)?token|(perc_)?type(_lu|_per_token_lu)?)",
                all = TRUE,
                perl = TRUE)
})

test_that("the perc data type is double and the other numeric data is integer", {
  expect_type(ttr_res[["perc_token"]], "double")
  expect_type(ttr_res[["token"]], "integer")
})

test_that("`ttr()` produces error message when `df` is still NULL", {
  expect_error(ttr(), "The `df` argument is NULL")
})
