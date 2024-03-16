
# moving_ranges -----------------------------------------------------------

test_that("moving ranges are correctly calculated", {
  fifth_moving_range <- moving_ranges(pounds_scrapped, date, value) |>
    dplyr::slice(5) |>
    dplyr::pull(moving_range)

  expect_identical(fifth_moving_range, 1)
})

test_that("empty dataframe returns dataframe with moving_range column", {
  empty_df <- tibble::tibble(date = numeric(), value = numeric())
  result <- moving_ranges(empty_df, date, value)
  expect_true("moving_range" %in% colnames(result))
  expect_equal(nrow(result), 0)
})

test_that("single row dataframe returns correct moving_range", {
  single_row_df <- tibble::tibble(date = as.Date("2021-01-01"), value = 100)
  result <- moving_ranges(single_row_df, date, value)
  expect_true(is.na(result$moving_range[1]) || result$moving_range[1] == 0)
})

test_that("NA values are handled appropriately", {
  df_with_na <- tibble::tibble(date = as.Date(c("2021-01-01", "2021-01-02")), value = c(100, NA))
  result <- moving_ranges(df_with_na, date, value)
  expect_true(is.na(result$moving_range[2]))
})


# average_moving_range ----------------------------------------------------



