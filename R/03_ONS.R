##################################################
## Project: ONS data collection
## Date:  "Tue May 17 12:05:47 2022"
## Author: JM
## Notes: wow @ those column names
##################################################
library(tidyverse)

clean_dates <- function(data){
  # reformats dates from ONS column names
  data |>
    mutate(year = str_extract(date, '[0-2]{4}'),
           month = str_extract(date, '[^_]+_[0-9]{4}$') |>
             str_remove('_[0-2]{4}') |>
             str_to_title(),
           date =  str_glue('{year}-{month}-15'),
           date = lubridate::as_date(date, format = "%Y-%B-%d")
    ) |>
    select(-year, -month)
}

# https://www.arcgis.com/home/item.html?id=7cf545f26fb14b3f972116241e073ada

this <- '_excluding_(cases|those)_linked_to_outbreaks_in_ltch_rh|_estimate_excluding_ltch_residents|_name'

raw <-
  read_csv("https://www.arcgis.com/sharing/rest/content/items/7cf545f26fb14b3f972116241e073ada/data")

renamed <- raw |>
  janitor::clean_names() |>
  rename_with(~.x |>
                str_remove(this) |>
                str_remove('ons_')) |>
  relocate(starts_with('pop')) |>
  glimpse()

neighbourhoods <-
  renamed |>
  select(id, neighbourhood, population) |>
  distinct() |>
  arrange(neighbourhood)

rates <- renamed |>
  select(id, neighbourhood, contains('pop')) |>
  pivot_longer(-c(id, neighbourhood, population),
               names_to = 'date', values_to = 'rate') |>
  clean_dates() |>
  mutate(
    suppressed_rate = rate == 'Suppressed',
    rate = if_else(suppressed_rate, '0', rate) |> as.numeric()
  ) |>
  glimpse()

cases <- renamed |>
  select(id, neighbourhood, contains('cases'), -contains('cum')) |>
  pivot_longer(-c(id, neighbourhood),
               names_to = 'date', values_to = 'cases') |>
  clean_dates() |>
  mutate(
    suppressed_case = str_detect(cases, '<'),
    cases = if_else(suppressed_case, '2.5', cases) |> as.numeric()
  ) |>
  glimpse()


ons <-
  full_join(rates, cases, by = c("id", "neighbourhood", "date")) |>
  select(-starts_with('sup')) |>
  glimpse()

ts_plot_ons <- function(data, response){
  data |>
    mutate(resp = {{response}}) |>
    ggplot(aes(date, resp, group = neighbourhood)) +
    geom_line(alpha = 0.15, size = 0.5)
}

ons |>
  filter(date > lubridate::as_date('2021-10-01')) |>
  ts_plot_ons(rate) +
  labs()

ons |>
  ggplot(aes(date, cases, group = neighbourhood)) +
  geom_line(alpha = 0.15, size = 0.5)

ons |>
  ggplot(aes(date, rate, group = neighbourhood)) +
  geom_line(alpha = 0.15, size = 0.5)


