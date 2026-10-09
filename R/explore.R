library(tidyverse)
library(blueycolors)
library(janitor)

socks_pal <- bluey_palette("socks")

# reading in disadvantaged data and cleaning up
df_disadv_raw <- read_csv("data/sy25_sch_disadv.csv") |>
    clean_names()

df_disadv <- df_disadv_raw |>
    select(-c(pt_count, total_count)) |>
    mutate(ft_count = as.numeric(str_remove_all(ft_count, ","))) |>
    pivot_wider(
        names_from = disadvantaged,
        values_from = ft_count,
        values_fill = 0
    ) |>
    mutate(
        pop = N + Y,
        pct_disadv = Y / pop
    ) |>
    filter(!is.na(pct_disadv)) |>
    select(division_number, school_number, pct_disadv)

# reading in spsf data, renaming, and performing some light cleaning
df_spsf_raw <- read_csv("data/vdoe_spsf_sy25.csv", na = c("-", "^"))

names_crosswalk <- read_csv("data/name_crosswalk.csv")

names(df_spsf_raw) <- names_crosswalk$new_name

df_spsf <- df_spsf_raw |>
    mutate(
        perf_cat = fct_relevel(perf_cat, c("Distinguished", "Meets Expectations", "Approaching Expectations", "Needs Intensive Support", "Too Small")),
    )
# joining disadvantaged data with spsf data
df_joined <- df_spsf |>
    left_join(df_disadv, by = c("div_num" = "division_number", "sch_num" = "school_number")) |>
    filter(sch_type %in% c("ES", "MS", "HS")) |>
    mutate(sch_type = fct_relevel(sch_type, c("ES", "MS", "HS")))

df_joined |>
    ggplot(aes(x = pct_disadv, y = overall_score)) +
    geom_point() +
    geom_smooth(method = "lm") +
    facet_wrap(vars(sch_type)) +
    theme_minimal()
# resume here
