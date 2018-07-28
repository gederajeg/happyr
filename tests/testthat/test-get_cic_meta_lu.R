context("test-get_cic_meta_lu.R")

test_that("`get_cic_meta_lu()` throws error message when `df_ttr_out` argument is NULL", {
  expect_error(get_cic_meta_lu("possessable object"), "The `df_ttr_out` argument is NULL")
})

ttr_metaphor <- ttr(df = phd_data_metaphor,
                    metaphor_var = "metaphors",
                    lexunit_var = "lu",
                    float_digits = 2)
cic <- get_cic_meta_lu("desired goal", df_ttr_out = ttr_metaphor)

test_that("the correct columns output of `get_cic_meta_lu()`", {
  expect_output(str(cic), "4 variables", all = FALSE, perl = TRUE)
  expect_output(str(cic), "\\$ (Lexical_units|Gloss|N|Perc_overall)", all = TRUE, perl = TRUE)
})
