library(tidyverse)
library(usethis)
f <- "https://raw.githubusercontent.com/difiore/ada-datasets/main/IMDB-movies.csv"
d <- read_csv(f, show_col_types = FALSE)
d <- filter(d, startYear >= 1920) |>
  filter(startYear <= 1979) |>
  filter(runtimeMinutes >= 60) |>
  filter(runtimeMinutes <= 180)
