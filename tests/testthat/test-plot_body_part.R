context("test-plot_body_part.R")

test_that("output of `plot_body_part()` is a list of 9 elements", {
  expect_output(str(plot_body_part(df = phd_data_metaphor)), "(List of 9|\\$(data|layers|scales|mapping|theme|coordinates|facet|plot_env|labels)|\\.\\.\\$ (body_part_terms|n|gloss|body_parts)|of 4 variables)", all = TRUE, perl = TRUE)
})
