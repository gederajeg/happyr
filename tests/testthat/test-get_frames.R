context("test-get_frames.R")

test_that("`get_frames()` outputs 5 columns with no `synonyms` column when `word` argument is NULL", {
  expect_identical(dim(get_frames(metaphor = "desired goal", df = phd_data_metaphor))[2], 5L)
})

test_that("`get_frames()` outputs 6 columns, including the `synonyms` column, when `word` argument is specified", {
  expect_identical(dim(get_frames(metaphor = "desired goal", word = "^kesenangan", df = phd_data_metaphor))[2], 6L)
})

test_that("`get_frames()` throws error when the `df` argument is NULL", {
  expect_error(get_frames("desired goal", "^kesenangan"), "The `df` argument is NULL; please specify it with `phd_data_metaphor`!")
})
