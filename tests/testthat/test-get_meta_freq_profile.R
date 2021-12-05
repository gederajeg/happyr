ttr_metaphor <- ttr(df = phd_data_metaphor)

test_that("`get_meta_freq_profiles()` works", {
  expect_equal(dim(get_meta_freq_profiles("desired", token, type_lu, type_per_token_lu, df = ttr_metaphor))[2], 4L)
  expect_equal(dim(get_meta_freq_profiles("desired", token, type_lu, df = ttr_metaphor))[2], 3L)
  expect_equal(dim(get_meta_freq_profiles("desired", token, df = ttr_metaphor))[2], 2L)
  expect_output(str(get_meta_freq_profiles("desired", token, df = ttr_metaphor)), "(tbl(_df)?|data\\.frame)")
})
