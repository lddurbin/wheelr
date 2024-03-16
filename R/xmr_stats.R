#' Compute the moving ranges
#'
#' @description Find the difference between successive values, known as the
#'   moving ranges.
#'
#' @param data A dataframe with at least two columns.
#' @param date_col The unquoted name of the column which contains the date for
#'   each value.
#' @param value_col The unquoted name of the column which contains the values
#'   for which the moving ranges will be computed.
#'
#' @return A dataframe with one column greater than the number in `data`.
#' @export
#'
#' @examples
#' moving_ranges(pounds_scrapped, date, value)
moving_ranges <- function(data, date_col, value_col) {
  data_with_mr <- data |>
    arrange({{ date_col }}) |>
    mutate(
      moving_range = abs(
        {{ value_col }} -
          dplyr::lag({{ value_col }}, default = dplyr::first({{ value_col }}))
      )
    )

  return(data_with_mr)
}


#' Compute the average moving range
#'
#' @description Calculate the average (mean) of the moving ranges.
#'
#' @param data A dataframe with at least two columns.
#' @param date_col The unquoted name of the column which contains the date for
#'   each value.
#' @param moving_range_col The unquoted name of the column which contains the
#'   difference between successive values, known as the moving ranges.
#' @param ... \code{\link[rlang:args_data_masking]{<data-masking>}} Optionally,
#'   pass expressions to filter the data, which limits the range of values over
#'   which the mean will be computed.
#'
#' @return A numeric value.
#' @export
#'
#' @examples
#' # Compute moving ranges.
#' data_with_MRs <- moving_ranges(pounds_scrapped, date, value)
#'
#'# Compute average moving range for values that occur prior to January 2024.
#' average_moving_range(data_with_MRs, date, moving_range, date < as.Date("2024-01-01"))
average_moving_range <- function(data, date_col, moving_range_col, ...) {
  moving_range_avg <- NULL

  if (nrow(data) < 4) {
    warning("Dataset must contain at least four rows for moving range calculation.")
    return(NA_real_)  # Return NA to indicate the calculation can't be performed
  }

  mr_avg <- data |>
    arrange({{ date_col }}) |>
    dplyr::slice(-1) |>
    filter(...) |>
    summarise(moving_range_avg = mean({{ moving_range_col }}, na.rm = TRUE)) |>
    dplyr::pull(moving_range_avg)

  return(mr_avg)
}

