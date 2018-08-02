context("test-plot_cxn_interrater.R")

test_that("output of `plot_cxn_interrater()` is a list of 9 elements", {
  expect_output(str(plot_cxn_interrater(df = top_cxn_data)), "(List of 9|\\$(data|layers|scales|mapping|theme|coordinates|facet|plot_env|labels)|\\.\\.\\$ (synonyms|cxn_pattern|n))", all = TRUE, perl = TRUE)
})
