get_moving_range <- function(data, date_col, value_col) {
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
