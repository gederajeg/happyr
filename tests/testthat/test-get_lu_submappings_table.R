df_submapping <- get_lu_submappings_table(metaphor = "liquid in a container", df = phd_data_metaphor)
test_that("`get_lu_submappings_table()` works without specifying the synonyms", {
  expect_output(str(df_submapping), "(tbl(_df)?|data\\.frame)")
})

df_submapping1 <- get_lu_submappings_table(metaphor = "liquid in a container", df = phd_data_metaphor, word = "kegembiraan")
test_that("`get_lu_submappings_table()` works by specifying the synonyms", {
  expect_output(str(df_submapping), "(tbl(_df)?|data\\.frame)")
})

df_submapping2 <- get_lu_submappings_table(metaphor = "liquid in a container", df = phd_data_metaphor, top_n = TRUE, limit = 5)
test_that("`get_lu_submappings_table()` works when limiting to the top-n items", {
  expect_output(str(df_submapping2), "(tbl(_df)?|data\\.frame)")
  expect_equal(dim(df_submapping2)[1], 5L)
})

test_that("`get_lu_submappings_table()` works when printing all items", {
  expect_output(str(get_lu_submappings_table(metaphor = "liquid in a container", df = phd_data_metaphor, print_all = TRUE)), "(tbl(_df)?|data\\.frame)")
})
