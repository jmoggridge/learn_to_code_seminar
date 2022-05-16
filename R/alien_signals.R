##################################################
## Project: Alien Signals
## Date:
## Author:
##################################################

#' Hand & Brain
#' You'll be the 'brain' -> the brain tells the 'hand' what to do
#' The hand will code out your commands and explain what they did

#' **Prompt**
#' Your friend who works at NASA has revealed to you that they have
#' collected a series of unusual transmissions over the past few weeks.
#' They send you an email with this dataset (your problem data) attached.
#' The observations are pairs of frequency (x) and amplitude (y) from a special
#' range of the EM spectrum. Your friend isn't sure what to make of the data.

#' Can you find the pattern in the signals?

# install.packages('tidyverse')
library(tidyverse)
signals <-
  read_csv('data/alien_signals.csv') |>
  transmute(frequency = x, amplitude = y)

signals |> glimpse()












































##------ Sun May 15 19:40:21 2022 ------##