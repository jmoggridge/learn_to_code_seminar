##################################################
## Project: Scromp matchmaker
## Notes: Gotta match em all
##################################################
#'
#' Scromps are a newly discovered species of shrimp observed on deep-sea
#' hydrothermal vents. They are sexually dimorphic, the two groups (left and right) are differentiated by the direction of a pattern on the shells. Each scromp also has some accessory traits (scromp moves) that define their abilities and role in the colony.
#' Newborns are paired at birth in arranged marriages of left and right based on a specific strategy.
#'
#' To start a new scromp colony in the laboratory, you'll need to play matchmaker
#' Each pair must have a left and a right scromp.
#' All pairs must share a scromp move for the pair to work.

library(tidyverse)

scromp_moves <-
  c('foobar', 'zazz', 'chunga', 'waldo', 'blept')

left_scromps <-
  tibble(name = state.name) |>
  filter(!str_detect(name, 'Island')) |>
  slice_sample(n = 10, replace = F)

random_name <- function() {
  name_attempt <- base::letters |>
    sample(size = rpois(1, lambda = 5)) |>
    str_c(collapse = '') |>
    str_to_title()
  # check vowel content > 25%
  min_vowels <- floor(nchar(name_attempt) / 3)
  the_regex <- str_glue('[aeiou]{{min_vowels}}')
  # return or retry
  if (str_detect(name_attempt, pattern = the_regex)) return(name)
  else create_name()
}


right_scromps <- tibble(name  = map_chr(seq(10), random_name))
create_name()
