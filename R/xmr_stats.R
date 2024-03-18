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


#' Compute either the average value or the average moving range
#'
#' @param data A dataframe with at least two columns.
#' @param date_col The unquoted name of the column which contains the date for
#'   either each value or each moving range.
#' @param value_col The unquoted name of the column which contains either the
#'   values or the difference between successive values (known as the moving
#'   ranges).
#' @param moving_ranges Compute the average of the values (`FALSE`) or the
#'   average of the moving ranges (`TRUE`)? By default, the average values will
#'   be returned.
#' @param ... \code{\link[rlang:args_data_masking]{<data-masking>}} Optionally,
#'   pass expressions to filter the data, which limits the range of values or
#'   moving ranges over which the mean will be computed.
#'
#' @return A numeric value.
#' @export
#'
#' @examples
#' # Compute moving ranges.
#' data_with_MRs <- moving_ranges(pounds_scrapped, date, value)
#'
#'# Compute average moving range for values that occur prior to January 2024.
#' compute_average(
#'   data_with_MRs,
#'   date,
#'   moving_range,
#'   moving_ranges = TRUE,
#'   date < as.Date("2024-01-01")
#' )
compute_average <- function(data, date_col, value_col, moving_ranges = FALSE, ...) {
  if (nrow(data) < 4) {
    warning("Dataset must contain at least four rows for calculation.")
    return(NA_real_)
  }

  data <- arrange(data, {{ date_col }})

  avg_name <- "value_avg"

  if(moving_ranges == TRUE) {
    data <- dplyr::slice(data, -1)
    avg_name <- "moving_range_avg"
  }

  data_with_average <- data |>
    filter(...) |>
    summarise({{avg_name}} := round(mean({{ value_col }}, na.rm = TRUE), 1)) |>
    dplyr::pull({{avg_name}})

  return(data_with_average)
}


#' Compute the Upper Range Limit
#'
#' @param avg_moving_range Numeric value, expressing the average of the
#'   moving ranges.
#'
#' @return Numeric value.
#'
#' @export
upper_range_limit <- function(avg_moving_range) {
  url <- round((3.27 * avg_moving_range), 1)

  return(url)
}


#' Compute the Upper Natural Process Limit
#'
#' @param avg_moving_range Numeric value, expressing the average of the moving
#'   ranges.
#' @param avg_value Numeric value, expressing the average of the values.
#'
#' @return Numeric value.
#'
#' @export
upper_natural_process_limit <- function(avg_moving_range, avg_value) {
  unpl <- round(avg_value + (2.66 * avg_moving_range), 1)

  return(unpl)
}


#' Compute the Lower Natural Process Limit
#'
#' @param avg_moving_range Numeric value, expressing the average of the
#'   moving ranges.
#' @param avg_value Numeric value, expressing the average of the values.
#'
#' @return Numeric value.
#'
#' @export
lower_natural_process_limit <- function(avg_moving_range, avg_value) {
  lnpl <- round(avg_value - (2.66 * avg_moving_range), 1)

  return(lnpl)
}
