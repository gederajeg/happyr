context("test-mdca_attr_repel.R")

mdca_res <- mdca(df = phd_data_metaphor,
                 cxn_var = "synonyms",
                 coll_var = "metaphors",
                 correct_holm = TRUE,
                 concise_output = TRUE,
                 already_count_table = FALSE,
                 assocstr_digits = 3)

test_that("`mdca_attr()` and `mdca_repel()` throw messages when the `filter_by` argument is NULL", {
  expect_message(mdca_attr(mdca_res, filter_by = NULL), "given minimum AssocStr")
  expect_message(mdca_repel(mdca_res, filter_by = NULL), "given minimum AssocStr")
})

test_that("`mdca_attr()` and `mdca_repel()` throw error when the `cxn_type` argument is NULL, the `cxn_var` is NULL, and when the `df` argument is NULL", {
  expect_error(mdca_attr(mdca_res, filter_by = "cxn"), "NO input for `cxn_type` argument")
  expect_error(mdca_attr(mdca_res, filter_by = "cxn", cxn_var = NULL), "NO input for `cxn_var` argument")
  expect_error(mdca_repel(mdca_res, filter_by = "cxn"), "NO input for `cxn_type` argument")
  expect_error(mdca_repel(mdca_res, filter_by = "cxn", cxn_var = NULL), "NO input for `cxn_var` argument")
  expect_error(mdca_attr(cxn_type = "kesenangan"), "The `df` argument is NULL")
  expect_error(mdca_repel(cxn_type = "kegembiraan"), "The `df` argument is NULL")
})

test_that("test output of mdca_attr and mdca_repel when filtered by collocates/collexemes; they throws error when the `coll_var` and `coll_type` arguments are NULL", {
  expect_error(mdca_attr(mdca_res, filter_by = "colloc", coll_var = NULL, coll_type = "desired goal"), "NO input for `coll_var`")
  expect_error(mdca_attr(mdca_res, filter_by = "colloc", coll_var = "metaphors", coll_type = NULL), "NO input for `coll_type`")
  expect_error(mdca_repel(mdca_res, filter_by = "colloc", coll_var = NULL, coll_type = "desired goal"), "NO input for `coll_var`")
  expect_error(mdca_repel(mdca_res, filter_by = "colloc", coll_var = "metaphors", coll_type = NULL), "NO input for `coll_type`")

})

test_that("`mdca_attr()` and `mdca_repel()` output the Holm's correction when `mdca()` includes correction of the Binomial p-value (i.e. 8-column tibble)", {
  expect_output(str(mdca_attr(mdca_res, cxn_type = "kesenangan")), "(\\$ (p_holm|dec)|assocstr|tbl(_df)?|8 variables)", all = TRUE, perl = TRUE)
  expect_output(str(mdca_repel(mdca_res, cxn_type = "kesenangan")), "(\\$ (p_(holm|binomial)|dec)|assocstr|tbl(_df)?|8 variables)", all = TRUE, perl = TRUE)
})

mdca_res_uncorrected <- mdca(df = phd_data_metaphor,
                             cxn_var = "synonyms",
                             coll_var = "metaphors",
                             correct_holm = FALSE,
                             concise_output = TRUE,
                             already_count_table = FALSE,
                             assocstr_digits = 3)

test_that("`mdca_attr()` and `mdca_repel()` do not output the Holm's correction when `mdca()` excludes correction of the Binomial p-value (i.e. 6-column tibble)", {
  expect_output(str(mdca_attr(mdca_res_uncorrected, cxn_type = "kesenangan")), "(\\$ (p_binomial|assocstr)|tbl(_df)?|6 variables)", all = TRUE, perl = TRUE)
  expect_output(str(mdca_repel(mdca_res_uncorrected, cxn_type = "kesenangan")), "(\\$ (p_binomial|assocstr)|tbl(_df)?|6 variables)", all = TRUE, perl = TRUE)
})

test_that("`mdca_attr()` and `mdca_repel()` can filter by collocates/collexeme types", {
  expect_output(str(mdca_attr(mdca_res, filter_by = "colloc", coll_var = "metaphors", coll_type = "desired")), "(data\\.frame|tbl_df|tbl)")
  expect_output(str(mdca_repel(mdca_res, filter_by = "colloc", coll_var = "metaphors", coll_type = "desired")), "(data\\.frame|tbl_df|tbl)")
})
