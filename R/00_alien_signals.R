##################################################
## Project 1: Alien Signals
##################################################
#'
#' **Prompt**
#'
#' NASA has emailed you a dataset of unusual emissions they've recorded
#' over the past few weeks. They're not sure how to interpret this data and
#' are desperate for your help.
#'
#' The metadata included with their dataset reads:
#'
#' - A constant signal has been captured from a point in space
#' - There are 25,800 pairs of frequency (x) and amplitude (y) values.
#' - We have de-noised the data to remove background and artefacts.
#' - Can you find a pattern in the signal?

# install.packages('tidyverse')
library(tidyverse)

# signals data from...
signals <-
  # reading in the csv file
  read_csv('data/alien_signals.csv') |>
  rename(frequency = x, amplitude = y)

signals |> glimpse()


# where do we start?


# publish your findings....












































##------ Sun May 15 19:40:21 2022 ------##

#' Hand & Brain
#' You'll be the 'brain' -> the brain tells the 'hand' what to do
#' The hand will code out your commands and explain what they did
