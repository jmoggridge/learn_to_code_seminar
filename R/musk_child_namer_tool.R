library(tidyverse)

weights <-  c(10, 3, 8, 5, 15,
              4, 5, 6, 9, 7,
              4, 8, 6, 8, 11,
              9, 1, 7, 8, 9,
              11, 6, 3, 1, 3, 1)

my_alpha <-
  map2(letters, weights, ~rep(.x, times = .y)) |>
  flatten() |>
  as.character()


austen <-
  janeaustenr::austen_books() |>
  filter(nchar(text) > 0) |>
  transmute(text = text |> str_to_lower() |>
              str_split('')) |>
  unnest(text) |>
  filter(text %in% letters) |>
  count(text) |>
  mutate(n = ceiling(n / 1000))

austen_alpha <- austen |>
  transmute(x = map2(text, n, ~rep(.x, times = .y))) |>
  unnest(cols = c(x)) |>
  pull(x)




random_name <- function(alpha) {
  name <- alpha |>
    sample(size = rpois(1, lambda = 5), replace = T) |>
    str_c(collapse = '') |>
    str_to_title()
  # check vowel content > 25%
  min_vowels <- floor(nchar(name) / 3)
  vowels_regex <- str_glue('[aeiou]{{{min_vowels}}}')
  min_cons <- floor(nchar(name) / 4)
  cons_regex <- str_glue('[bcdfghjklmnpqrstvwxyz]{{{min_cons}}}')

  # return or retry
  if (
    str_detect(name, vowels_regex) &
    str_detect(name, cons_regex) &
      nchar(name) >= 3 &
      nchar(name) <= 9)
    {
    return(name)
  }  else { # recursion - try again
    random_name()
    }
}

random_name(alpha = austen_alpha)

