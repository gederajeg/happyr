context("test-kappa_tidy.R")

test_that("kappa_tidy() produces five column output", {
  expect_equal(dim(kappa_tidy(df = df_cxn_pattern,
                              var_names = "^pattern",
                              round = "pre_disc"))[2], 5)
  expect_equal(dim(kappa_tidy(df = df_meta_use_1st, var_names = "^use_(coder|author)$", round = "pre_disc"))[2], 5)
  expect_equal(dim(kappa_tidy(df = df_meta_use_2nd,
                              var_names = "^use_2nd_",
                              round = "post_disc"))[2], 5)
})
