
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

test_that("average moving range is correctly calculated with enough rows", {
  df <- tibble::tibble(
    date = as.Date(c("2021-01-01", "2021-01-02", "2021-01-03", "2021-01-04")),
    moving_range = c(NA, 5, 5, 6)  # First value is NA due to the slice operation, assume these moving ranges are correct
  )
  # Expected average moving range calculation would only consider (5, 5, 6), excluding the first NA
  expect_equal(average_moving_range(df, date, moving_range), mean(c(5, 5, 6)))
})

test_that("function warns with fewer than four rows", {
  df_few_rows <- tibble::tibble(
    date = as.Date(c("2021-01-01", "2021-01-02", "2021-01-03")),
    moving_range = c(NA, 5, 5)  # Assume these moving ranges are correct
  )
  expect_warning(average_moving_range(df_few_rows, date, moving_range))
})

test_that("NA values in moving range column are handled with enough rows", {
  df_with_na <- tibble::tibble(
    date = as.Date(c("2021-01-01", "2021-01-02", "2021-01-03", "2021-01-04")),
    moving_range = c(NA, 5, NA, 6)  # NA values present
  )
  # Expected to calculate average excluding NA values (5 and 6)
  expect_equal(average_moving_range(df_with_na, date, moving_range), mean(c(5, 6)))
})

test_that("correct average is calculated after filtering with enough rows", {
  df <- tibble::tibble(
    date = as.Date(c("2023-12-29", "2023-12-30", "2023-12-31", "2024-01-01")),
    moving_range = c(3, 4, 5, 6)  # Assume these moving ranges are correct
  )
  # Filtering to exclude the last date, effectively checking the average of (3, 4, 5)
  expect_equal(average_moving_range(df, date, moving_range, date < as.Date("2024-01-01")), mean(c(4, 5)))
})

test_that("function handles dataset correctly after slice and filter with enough rows", {
  df <- tibble::tibble(
    date = as.Date(c("2023-12-28", "2023-12-29", "2023-12-30", "2023-12-31", "2024-01-01")),
    moving_range = c(2, 3, 4, 5, 6)  # Assume these moving ranges are correct
  )
  # After slicing off the first row and filtering, we're checking the average of (3, 4, 5), excluding the last date
  expect_equal(average_moving_range(df, date, moving_range, date < as.Date("2024-01-01")), mean(c(3, 4, 5)))
})

