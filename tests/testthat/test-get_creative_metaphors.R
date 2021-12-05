context("test-get_creative_metaphors.R")

test_that("`get_creative_metaphors()` throws error message when `df_ttr_out` argument is NULL", {
  expect_error(get_creative_metaphors(min_token = 5, top_n_limit = 10), "The `df_ttr_out` argument is NULL")
})

ttr_metaphor <- ttr(df = phd_data_metaphor,
                    schema_var = "metaphors",
                    lexunit_var = "lu",
                    float_digits = 2)
creative_metaphors <- get_creative_metaphors(df_ttr_out = ttr_metaphor,
                                                      min_token = 3,
                                                      top_n_limit = 10)

test_that("the correct columns output of `get_cic_meta_lu()`", {
  expect_equal(object = dim(creative_metaphors)[2], expected = 6)
  expect_output(str(creative_metaphors), "\\$ ((type_(lu|per_token_lu))|(perc_)?(token|type_lu))", all = TRUE, perl = TRUE)
})
