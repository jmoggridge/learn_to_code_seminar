##################################################
##' Project: Autoparent
##' Inspiration:
##' This revolutionary AI will replace human parents in 2-3 years, tops.
##' - Elon Musk (2021)
##'
##' Technical specs:
##' - provides a random name for the child (can be terrible)
##################################################


library(tidyverse)

jane_austen_words <-
  janeaustenr::austen_books() |>
  filter(nchar(text) > 0) |>
  transmute(words = str_to_lower(text) |> str_split(' ')) |>
    unnest(words) |>
    filter(!str_detect(words, '[:punct:]|[:digit:]')) |>
    distinct()



jane_austen_profile <- function() {
  austen_profile <-
    janeaustenr::austen_books() |>
    filter(nchar(text) > 0) |>
    transmute(text = str_to_lower(text) |> str_split('')) |>
    unnest(text) |>
    filter(text %in% letters) |>
    count(text) |>
    mutate(n = ceiling(n / 1000),
           alphabet = map2(text, n, ~ rep(.x, times = .y))) |>
    pull(alphabet) |>
    flatten() |>
    unlist()
}

austen <- jane_austen_profile()


musk_profile <- function(){
  tibble(
    weight = c(10, 3, 8, 5, 15,
               4, 5, 6, 9, 7,
               4, 8, 6, 8, 11,
               9, 11, 7, 8, 9,
               11, 6, 3, 11, 3,
               11, 5, 5, 5),
    character = c(letters, '🍆', '420', '69')
  ) |>
    mutate(alphabet = map2(character, weight, ~rep(.x, times = .y))) |>
    pull(alphabet) |>
    flatten() |>
    unlist()
}

# name generate
new_name <- function(alphabet, lambda){
  alphabet |>
    sample(size = rpois(1, lambda = lambda), replace = T) |>
    str_c(collapse = '')
}


# name test
quality_name <- function(name){
   all(
     length = nchar(name) |> between(3,9),
     bad_queue_not_ewe = !str_detect(name, 'q[^u]'),
     not_triple_vowel = !str_detect(name, '[aeiou]{3}'),
     not_triple_cons = !str_detect(name, '[^aeiou]{3}'),
     enough_vowel =
       str_extract_all(name, '[aeiou]') |>
       pluck(1) |>
       length()  |>
       between(floor(0.35 * nchar(name)), floor(0.7*nchar(name)))
   )
}


# name wrapper
autoparent <- function(alphabet = letters, ...) {
  name <- new_name(alphabet, ...)
  # return or retry
  if (quality_name(name)) return(str_to_title(name))
  else autoparent(alphabet = alphabet, ...)
}


austen <- jane_austen_profile()
musk <- musk_profile()
austen_musk <- c(austen, musk)

map_chr(seq(1000), ~autoparent(alphabet = austen, lambda = 10)) |> sort()
map_chr(seq(1000), ~autoparent(alphabet = musk, lambda = 10)) |> sort()
map_chr(seq(1000), ~autoparent(alphabet = austen_musk, lambda = 10)) |> sort()

#
pharma_namer <- function(alphabet, ...) {
  name <- new_name(alphabet, ...)
  if (quality_name(name)) name
  else pharma_namer(alphabet, ...)
}

monoclonal <- function(){
  pharma_namer(austen, lambda = 10) |> paste0('imab', collapse = '')
}
sglt2 <- function(){
  pharma_namer(austen, lambda = 6) |> paste0('iflozin', collapse = '')
}

monoclonal()
sglt2()


##----------------------------------------------##
## Section:  rough work                       ----
##----------------------------------------------##


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
