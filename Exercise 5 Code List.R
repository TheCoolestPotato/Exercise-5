library(tidyverse)
library(usethis)
library(radiant)
library(mosaic)
f <- "https://raw.githubusercontent.com/difiore/ada-datasets/main/IMDB-movies.csv"
d <- read_csv(f, show_col_types = FALSE)
d <- filter(d, startYear >= 1920) |>
  filter(startYear <= 1979) |>
  filter(runtimeMinutes >= 60) |>
  filter(runtimeMinutes <= 180)
d[c('millenia', 'century', 'decade', 'year')] <- str_split_fixed(d$startYear,'',4)
(p <- ggplot(data=d, aes(x=runtimeMinutes)) +
    geom_histogram(stat = "bin",bins=20,colour="black",fill="purple")) +
  facet_wrap(~decade)
results <- d |> group_by(decade) |>
  dplyr::summarise(pop_n = n(),
                   pop_mean = mean(runtimeMinutes, na.rm = TRUE),
                   pop_sd =sdpop(runtimeMinutes, na.rm = TRUE))
set.seed(1)
s <- d |> group_by(decade) |>
  sample_n(100, replace=FALSE) |>
  dplyr::summarise(samp_size = n(),
                   samp_1_mean=mean(runtimeMinutes, na.rm = TRUE),
                   samp_1_sd=sd(runtimeMinutes, na.rm = TRUE),
                   samp_1_se=sd(runtimeMinutes, na.rm = TRUE)/sqrt(samp_size))
results <- inner_join(s, results, by = "decade") |>
  mutate(pop_se = pop_sd/sqrt(100))
reps <- 1000
s <- {do(reps) * sample_n(group_by(d, decade), 100, replace=FALSE)} |>
  group_by(decade, .index) |>
  summarise(avg_runtimeMinutes = mean(runtimeMinutes, na.rm = TRUE), sd_runtimeMinutes = sd(runtimeMinutes, na.rm = TRUE))
(p <- ggplot(data=s, aes(avg_runtimeMinutes)) +
    geom_histogram(stat = "bin",
                   bins=20,
                   colour="black",
                   fill="purple") +
    facet_wrap(~ decade, scales = "free_x"))
samp_distri <- s |>
  group_by(decade) |>
  summarize(samp_dist_mean=mean(avg_runtimeMinutes),
            samp_dist_stdev=sd(avg_runtimeMinutes))

comparison <- inner_join(results, samp_distri, by = "decade") |>
  dplyr::select(decade, pop_se, samp_1_se, samp_dist_stdev)
