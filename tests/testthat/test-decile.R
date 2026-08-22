test_that("decile summary matches expected group stats", {
  result <- decile(
    toolkitr::profile_example,
    unique_id = source_id,
    value_var = market_decile_nbrx_pre,
    tie_breaker_var = source_id,
    num_groups = 3,
    new_col = "quintile",
    calc_type = "group by value",
    verbose = TRUE
  )

  summary <- attr(result, "decile_summary")

  expected <- tibble::tibble(
    quintile = c("0", "1", "2", "3", "total"),
    n = c(17L, 892L, 331L, 635L, 1875L),
    sum_market_decile_nbrx_pre = c(0L, 3825L, 2317L, 5497L, 11639L),
    min_market_decile_nbrx_pre = c(0L, 1L, 7L, 8L, 0L),
    max_market_decile_nbrx_pre = c(0L, 6L, 7L, 10L, 10L)
  )

  expect_equal(summary, expected, ignore_attr = TRUE)
})
