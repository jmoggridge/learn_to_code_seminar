#' ---
#' title: "Simulate data with R's built in random variable generators"
#' author: "Jason Moggridge"
#' date: "2022-05-21"
#' output:
#'   html_document:
#'     theme: flatly
#' ---
#'
#' Create random variables using the `r*()` function corresponding to the distribution that you want.

#+ echo=FALSE
# render this script to an html document with:
# rmarkdown::render(here::here('R/simulate_distributions.R'))

#' Setup:
#+ fig.width=5, fig.height=5, echo=T
library(tidyverse)
library(patchwork) # for combining ggplots
ggplot2::theme_set(theme_bw())

# convenient histogram wrapper; need {{col}} bc. of data-masking
my_hist <- function(data, col, title = NULL) {
  data |>
    ggplot(aes({{col}})) +
    geom_histogram(bins = 30,
                   fill = 'cornflowerblue',
                   alpha = 0.7) +
    labs(subtitle = title)
}

#' ### Our first simulation
#'
#' We'll compare normal and uniform random variables with different sample sizes, to simulate what real datasets might look like.
#'
#+ echo=T
# simulates and plots random samples from norm and unif distributions
compare_uniform_and_normal <- function(n = 1000) {
  # uniform histogram
  uniform <-
    tibble(unif = runif(n, min = -1.5, max = 1.5)) |>
    my_hist(unif, title = str_glue('Uniform n={n}'))

  # normal histogram
  normal <-
    tibble(norm = rnorm(n, mean = 0, sd =  1)) |>
    my_hist(norm, title = str_glue('Normal n={n}'))

  # combine plots
  uniform + normal
}


#'Compare uniform and random distributions
#+ fig.width=6, fig.height=3, echo=T, fig.align='center'

# synchronize our random numbers for reproducibility.
set.seed(101)

# simulate sample size 30
compare_uniform_and_normal(n = 30)

# simulate sample size 300
compare_uniform_and_normal(n = 300)

#' ### Simulation 2: Alter parameters of distributions at constant sample size
#'
#'
#'
#' The Chi-sq. is a continuous distribution has a degrees of freedom parameter
#+ echo = T
map(
  c(1, 2, 4, 6, 9, 1000),
  ~ rchisq(n = 1000, df = .x) |>
    as_tibble() |>
    ggplot(aes(value)) +
    geom_histogram(bins = 30, fill = 'cornflowerblue', alpha = 0.7) +
    geom_vline(aes(xintercept = mean(value)), color = 'magenta') +
    labs(subtitle = str_glue('df = {.x}'), x = NULL)
) |>
  patchwork::wrap_plots(ncol = 2) &
  patchwork::plot_annotation(title = 'Chi Squared n=1000')
#'
#' The poisson distribution is a discrete and non-negative; it approximates count data well in many cases.
#'
#+ echo=T
map(
  c(0.1, 0.3, 0.5, 1, 2, 5, 10, 75, 500),
  ~ rpois(n = 10 ** 5, lambda = .x) |>
    as_tibble() |>
    count(value) |>
    ggplot(aes(value, n)) +
    geom_col(fill = 'cornflowerblue', alpha = 0.7) +
    geom_vline(aes(xintercept = sum(value * n) / sum(n)), color = 'magenta') +
    scale_x_continuous(breaks = scales::pretty_breaks()) +
    labs(subtitle = str_glue('lambda = {.x}'))
) |>
  patchwork::wrap_plots(ncol = 3) &
  patchwork::plot_annotation(title = 'Poisson')
#'
#' The negative binomial distribution has a size (n trials) and prob (of success for each trial) parameter
#'
#+ echo=T
map(
  c(1, 2, 4, 6, 9),
  ~ rnbinom(n = 10000, size = .x, prob = 0.15) |>
    as_tibble() |>
    ggplot(aes(value)) +
    geom_histogram(
      fill = 'cornflowerblue',
      color = 'white',
      alpha = 0.7,
      binwidth = 1
    ) +
    geom_vline(aes(xintercept = mean(value)), color = 'magenta') +
    scale_x_continuous(breaks = scales::pretty_breaks()) +
    labs(subtitle = str_glue('lambda = {.x}'), x = NULL)
) |>
  patchwork::wrap_plots(ncol = 2) &
  patchwork::plot_annotation(title = 'Negative Binomial')
