library(tidyverse)
library(blueycolors)

socks_pal <- bluey_palette("socks")

df_raw <- read_csv("data/vdoe_spsf_sy25.csv", na = c("-", "^"))

names_crosswalk <- read_csv("data/name_crosswalk.csv")

names(df_raw) <- names_crosswalk$new_name

# some light cleaning
df <- df_raw |>
    mutate(
        perf_cat = fct_relevel(perf_cat, c("Distinguished", "Meets Expectations", "Approaching Expectations", "Needs Intensive Support", "Too Small")),
    )

# let's count how many schools are in each performance category
df |>
    count(perf_cat) |>
    ggplot(aes(x = n, y = perf_cat)) +
    geom_col(fill = socks_pal[1]) +
    theme_minimal()
# ok, so most schools are distinguished
# but what if we look at by school level

sch_types <- c("ES", "MS", "HS")

df |>
    filter(sch_type %in% sch_types) |>
    count(perf_cat, sch_type) |>
    ggplot(aes(x = n, y = perf_cat)) +
    geom_col(fill = socks_pal[1]) +
    facet_wrap(vars(sch_type)) +
    theme_minimal()

# resume here
