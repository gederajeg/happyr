context("test-mdca.R")

mdca_res <- mdca(phd_data_metaphor, correct_holm = FALSE)
test_that("`mdca()` does not output Holm's correction column when `correct_holm = FALSE`", {
  expect_identical(dim(mdca_res)[2], 6L)
  expect_output(str(mdca_res), "\\$ (metaphors|assocstr|p_binomial|n|synonyms)", all = TRUE, perl = TRUE)
  expect_type(mdca_res$assocstr, "double")
  expect_type(mdca_res$p_binomial, "character")
})

mdca_res <- mdca(phd_data_metaphor, correct_holm = TRUE)
test_that("`mdca()` outputs Holm's correction column when `correct_holm = TRUE`", {
  expect_identical(dim(mdca_res)[2], 8L)
  expect_output(str(mdca_res), "\\$ (p_holm|dec)", all = TRUE, perl = TRUE)
})

test_that("`mdca()` throws error when data frame is not specified", {
  expect_error(mdca(), "no applicable method for 'groups'")
})

repelled <- subset(mdca_res, n < exp)[["assocstr"]]
test_that("`assocstr` when `n` is less than `exp` is less than 0 or minus", {
  for (i in seq_along(repelled)) {
    expect_less_than(repelled[i], 0)
  }
})

attracted <- subset(mdca_res, n > exp)[["assocstr"]]
test_that("`assocstr` when `n` is greater than `exp` is greater than 0 or positive", {
  for (i in seq_along(attracted)) {
    expect_gt(attracted[i], 0)
  }
})
