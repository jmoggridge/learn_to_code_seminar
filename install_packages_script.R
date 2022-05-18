# Install packages

packages <- c("bayesplot", "brms", "coda", "cowplot", "cubelyr", "devtools", "fishualize", "GGally", "ggdist", "ggExtra", "ggforce", "ggmcmc", "ggridges", "ggthemes", "janitor", "lisa", "loo", "palettetown", "patchwork", "psych", "remotes", "rstan", "santoku", "scico", "tidybayes", "tidyverse")


packages <- c("tidyverse", "rcartocolor", "leaflet", "patchwork", "tidymodels", "devtools", "GGally", "ggdist", "ggExtra", "ggforce", "ggmcmc", "ggridges", "ggthemes", "Hmisc", "janitor", "lisa", "loo", "palettetown", "patchwork", "psych", "remotes", "rstan", "santoku", "scico", "tidybayes")


install.packages(packages, dependencies = T)


tidyr::crossing(x = letters, y = letters)
?crossing
?expand_grid
