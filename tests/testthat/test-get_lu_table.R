context("test-get_lu_table.R")


test_that("`incl_submappings = TRUE` does not output submapping perc. info unless `submapping_perc = TRUE`", {
  expect_equal(dim(get_lu_table("desired goal", df = phd_data_metaphor, incl_submappings = T, submapping_perc = F))[2], 4L)
})

test_that("`incl_submappings = TRUE` outputs submapping perc. info when `submapping_perc = TRUE`", {
  expect_equal(dim(get_lu_table("desired goal", df = phd_data_metaphor, incl_submappings = T, submapping_perc = T))[2], 6L)
})