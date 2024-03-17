
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

# Test for basic functionality with average values
test_that("average value is correctly calculated", {
  df <- tibble(date = as.Date('2020-01-01') + 0:4, value = c(1, 2, 3, 4, 5))
  expect_equal(compute_average(df, date, value, moving_ranges = FALSE), mean(c(1, 2, 3, 4, 5)))
})

# Test for basic functionality with average moving ranges
test_that("average moving range is correctly calculated", {
  df_moving_range <- tibble(date = as.Date('2020-01-01') + 0:4, value = c(NA, 1, 1, 1, 1))
  expect_equal(compute_average(df_moving_range, date, value, moving_ranges = TRUE), mean(c(1, 1, 1, 1), na.rm = TRUE))
})

# Test handling fewer than four rows
test_that("function warns with fewer than four rows", {
  df_few_rows <- tibble(date = as.Date('2020-01-01') + 0:2, value = c(1, 2, 3))
  expect_warning(compute_average(df_few_rows, date, value, moving_ranges = FALSE))
})

# Test NA handling
test_that("NA values are correctly handled", {
  df_with_na <- tibble(date = as.Date('2020-01-01') + 0:4, value = c(1, NA, 3, NA, 5))
  expect_equal(compute_average(df_with_na, date, value, moving_ranges = FALSE), mean(c(1, 3, 5), na.rm = TRUE))
})

# Test filtering works correctly
test_that("filtering limits the range of values as expected", {
  df <- tibble(date = as.Date('2020-01-01') + 0:4, value = c(1, 2, 3, 4, 5))
  expect_equal(compute_average(df, date, value, moving_ranges = FALSE, date < as.Date('2020-01-05')), mean(c(1, 2, 3, 4)))
})


