##################################################
## Project: Learn to code
## Script: Dplyr and ggplot2
##################################################

library(tidyverse)

# did I explain the pipe yet?

# dpylr intro

# lets use this starwars data
dplyr::starwars
# use
starwars |> glimpse()

starwars |>
  ggplot(aes(height, mass, color = sex)) +
  geom_point(alpha = 0.85, shape = 1) +
  scale_y_log10() +
  theme_classic()

# select, filter, mutate, summarise, group_by
# select gets columns by name (position)
starwars |> select(name, height, mass, sex)

# with tidy select
starwars |> select(name, where(~is.numeric(.x)))

# filter gets the rows that pass some condition
starwars |> filter(species == 'Droid')

# mutate changes a column or creates a new one
starwars |>
  mutate(
    # we can use if_else to do something based on a condition
    hair_color = if_else(
      condition = is.na(hair_color),
      true = 'no hair',
      false = hair_color
      )
  )

# summarise is similar to mutate but reduces all values to a single result
starwars |>
  summarise(
    height_mean = mean(height, na.rm = T),
    height_sd = sd(height, na.rm = T),
    )

# group by allows us to split data for groupwise-computation
starwars |>
  group_by(species) |>
  summarise(
    n = n(),
    height_mean = mean(height, na.rm = T),
    height_sd = sd(height, na.rm = T),
  ) |>
  filter(n>1) |>
  arrange(desc(height_mean))



starwars |>
  group_by(species) |>

  summarise(across(
    .cols = where(is.numeric),
    .fns = list('mean' = mean, 'median' = median),
    .names = "{.col}_{.fn}",
    na.rm = T
    )) |>
  pivot_longer(-species) |>
  mutate(
    attribute = str_extract(name, '^[^_]+'),
    stat = str_extract(name, '[^_]+$')
    )







# using a prefix, pivoting, facetted plot from pair of variables
starwars |>
  select(name, where(is.numeric)) |>
  pivot_longer(-name, names_to = 'attribute') |>
  ggplot(aes(x = value)) +
  geom_histogram() +
  facet_wrap(facets = vars(attribute), scales = 'free_x', ncol = 1)
