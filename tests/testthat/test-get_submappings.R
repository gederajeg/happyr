context("test-get_submappings.R")

test_that("`get_submappings()` throws error when the `df` argument is NULL", {
  expect_error(get_submappings("desired goal", "^kesenangan"), "The `df` argument is NULL; please specify it with `phd_data_metaphor`!")
})

test_that("`get_submappings()` outputs 5 columns with no `synonyms` column when `word` argument is NULL", {
  expect_identical(dim(get_submappings(metaphor = "desired goal", df = phd_data_metaphor))[2], 5L)
})

test_that("`get_submappings()` outputs 6 columns, including the `synonyms` column, when `word` argument is specified", {
  expect_identical(dim(get_submappings(metaphor = "desired goal", word = "^kesenangan", df = phd_data_metaphor))[2], 6L)
  expect_output(str(get_submappings(metaphor = "desired goal", word = "^kesenangan", df = phd_data_metaphor)), "\\$ synonyms")
})
