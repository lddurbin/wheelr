pounds_scrapped <- tibble::tibble(
  date = seq(as.Date("2022-01-01"), as.Date("2024-12-01"), by = "month"),
  value = c(133, 128, 128, 123, 124, 124, 126, 124, 126, 124, 129, 135, 142,
            140, 131, 126, 124, 126, 124, 126, 123, 125, 133, 134, 134, 139,
            143, 131, 126, 125, 124,  NA,  NA,  NA,  NA,  NA)
)


usethis::use_data(pounds_scrapped, overwrite = TRUE)
