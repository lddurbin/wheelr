pounds_scrapped <- tibble::tribble(
  ~month,	~one,	~two,	~three,
  "Jan",	133,	142,	134,
  "Feb",	128,	140,	139,
  "Mar",	128,	131,	143,
  "Apr",	123,	126,	131,
  "May",	124,	124,	126,
  "Jun",	124,	126,	125,
  "Jul",	126,	124,	124,
  "Aug",	124,	126,	NA,
  "Sep",	126,	123,	NA,
  "Oct",	124,	125,	NA,
  "Nov",	129,	133,	NA,
  "Dec",	135,	134,	NA
)


usethis::use_data(pounds_scrapped, overwrite = TRUE)
