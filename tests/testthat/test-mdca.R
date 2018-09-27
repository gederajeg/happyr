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
  expect_error(mdca(), "The `df` argument is NULL")
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

df_count <- dplyr::count(happyr::phd_data_metaphor, synonyms, metaphors, sort = TRUE)
mdca_res_count <- mdca(df_count, already_count_table = TRUE)
mdca_res_raw <- mdca(df = phd_data_metaphor, already_count_table = FALSE)
testthat::test_that("`mdca()` returns the same output of rows/cases when the input is raw or already co-occurrence table, and equal results", {

  testthat::expect_equal(nrow(mdca_res_count), nrow(mdca_res_raw))
  testthat::expect_equal(subset(mdca_res_raw, synonyms=="kebahagiaan" & assocstr >= 2)[["assocstr"]][1], subset(mdca_res_count, synonyms=="kebahagiaan" & assocstr >= 2)[["assocstr"]][1])
  testthat::expect_equal(subset(mdca_res_raw, synonyms=="kegembiraan" & assocstr >= 2)[["p_binomial"]][1], subset(mdca_res_count, synonyms=="kegembiraan" & assocstr >= 2)[["p_binomial"]][1])

})

mdca_res_long <- mdca(df = phd_data_metaphor, concise_output = FALSE)
mdca_res_short <- mdca(df = phd_data_metaphor, concise_output = TRUE)
testthat::test_that("`mdca()` produces more column when concise = FALSE than when concise = TRUE", {
  testthat::expect_true(ncol(mdca_res_long) > ncol(mdca_res_short))
})


colnames_long <- colnames(mdca_res_long)
colnames_long_rgx <- paste("(", paste(colnames(mdca_res_long), collapse = "|"), ")", sep = "")
testthat::test_that("the output columns when concise = FALSE are correctly printed/matched", {

  testthat::expect_true(all(stringr::str_detect(colnames_long, colnames_long_rgx)))

})
