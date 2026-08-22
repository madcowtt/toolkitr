test_that("converts to lowercase by default", {
  df <- data.frame(ColumnA = 1, ColumnB = 2, check.names = FALSE)
  result <- clean_col_names(df)

  expect_equal(names(result), c("columna", "columnb"))
})

test_that("converts to uppercase when specified", {
  df <- data.frame(columna = 1, columnb = 2, check.names = FALSE)
  result <- clean_col_names(df, case = "upper")

  expect_equal(names(result), c("COLUMNA", "COLUMNB"))
})

test_that("replaces spaces with underscore", {
  df <- data.frame(`Column A` = 1, check.names = FALSE)
  result <- clean_col_names(df)

  expect_equal(names(result), "column_a")
})

test_that("replaces special characters like & with underscore", {
  df <- data.frame(`A & B` = 1, check.names = FALSE)
  result <- clean_col_names(df)

  expect_equal(names(result), "a_b")
})

test_that("collapses multiple consecutive underscores into one", {
  df <- data.frame(`A___B` = 1, check.names = FALSE)
  result <- clean_col_names(df)

  expect_equal(names(result), "a_b")
})

test_that("collapses mixed symbols and underscores into one", {
  df <- data.frame(`A_&_B` = 1, check.names = FALSE)
  result <- clean_col_names(df)

  expect_equal(names(result), "a_b")
})

test_that("trims leading and trailing symbols", {
  df <- data.frame(`__Column__` = 1, `!Weird!` = 2, check.names = FALSE)
  result <- clean_col_names(df)

  expect_equal(names(result), c("column", "weird"))
})

test_that("numbers duplicate names starting from _1, including the first", {
  df <- data.frame(`A & B` = 1, `A___B` = 2, check.names = FALSE)
  result <- clean_col_names(df)

  expect_equal(names(result), c("a_b_1", "a_b_2"))
})

test_that("leaves unique names without a numeric suffix", {
  df <- data.frame(`A & B` = 1, `C & D` = 2, check.names = FALSE)
  result <- clean_col_names(df)

  expect_equal(names(result), c("a_b", "c_d"))
})

test_that("handles three or more duplicates correctly", {
  df <- data.frame(`A & B` = 1, `A___B` = 2, `A!!B` = 3, check.names = FALSE)
  result <- clean_col_names(df)

  expect_equal(names(result), c("a_b_1", "a_b_2", "a_b_3"))
})

test_that("errors on invalid case argument", {
  df <- data.frame(A = 1, check.names = FALSE)

  expect_error(clean_col_names(df, case = "sideways"))
})

test_that("works with numbers already in column names", {
  df <- data.frame(`Col 1` = 1, `Col_2` = 2, check.names = FALSE)
  result <- clean_col_names(df)

  expect_equal(names(result), c("col_1", "col_2"))
})
