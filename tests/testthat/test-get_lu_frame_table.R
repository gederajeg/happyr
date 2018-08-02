context("test-get_lu_frame_table.R")

test_that("column length for when `print_all = FALSE` is the same with when `print_all = TRUE`", {
  expect_equal(dim(get_lu_frame_table(metaphor = "possessable", word = "^kebahagiaan$", print_all = FALSE, limit = 20, df = phd_data_metaphor))[1],
               dim(get_lu_frame_table(metaphor = "possessable", word = "^kebahagiaan$", print_all = TRUE, limit = 20, df = phd_data_metaphor))[1])
})

test_that("column length for when `word = NULL` is the same with when `word` is not NULL", {
  expect_equal(dim(get_lu_frame_table(df = phd_data_metaphor, metaphor = "liquid in a", word = NULL))[2],
               dim(get_lu_frame_table(df = phd_data_metaphor, metaphor = "liquid in a", word = "kebahagiaan"))[2])
})
