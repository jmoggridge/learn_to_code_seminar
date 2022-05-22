##################################################
## Project: Diabetes classification
##################################################

# install.packages(c('tidymodels', 'glmnet', 'ranger'))
library(tidyverse)
library(tidymodels)
library(glmnet)
# https://archive.ics.uci.edu/ml/datasets/breast+cancer+wisconsin+(diagnostic)

ordinal_vars <-
  c("clump_thickness", "cell_size_uniformity", "cell_shape_uniformity", "marginal_adhesion", "single_epith_cell_size")

factor_vars <-
  c("bare_nuclei", "bland_chromatin", "normal_nucleoli", "mitoses")


# data from UCI repository
tumours <-
  read_csv(
    'https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/breast-cancer-wisconsin.data',
    col_names = c("id", ordinal_vars, factor_vars, 'diagnosis'),
    show_col_types = F
    )  |>
  mutate(diagnosis = if_else(diagnosis == 2, 'benign', 'malignant'),
         bare_nuclei = as.numeric(bare_nuclei),
         across(.cols = all_of(ordinal_vars),
                ~factor(.x, levels = 1:10, ordered = T)),
         across(.cols = c(all_of(factor_vars)),
                ~factor(.x)),
         diagnosis = factor(diagnosis, levels = c('malignant', 'benign'))
  )

datasplit <- initial_split(tumours)

# set model formula and variable transformation steps
rec <-
  training(datasplit) |>
  recipe(diagnosis ~ .) |>
  update_role(id, new_role = 'id') |>
  step_impute_knn(bare_nuclei) |>
  step_ordinalscore(all_of(!!ordinal_vars)) |>
  step_dummy(all_of(!!factor_vars))


lr_spec <-
  logistic_reg(
    mode = 'classification',
    engine = 'glmnet',
    penalty = tune(),
    mixture = tune()
  )

# workflow
wkflw <-
  workflow() |>
  add_recipe(rec) |>
  add_model(lr_spec)

# set the regularization penalty and flavor
hyperparams <- grid_regular(
  x = list(penalty = penalty(), mixture = mixture()),
  levels = 25
  )

# resampling: a 10 fold-cv
resamples <- vfold_cv(tumours, v = 10)

wkflw       # how to process and model data
hyperparams # model attributes that can't be estimated directly from data
resamples   # data to fit and eval models


library(doFuture)
registerDoFuture()
plan(multisession)

# perform the model fitting and evaluation to find best tuning
rs <- tune_grid(
  object = wkflw,
  resamples = resamples,
  grid = hyperparams,
  control = control_grid(verbose = T, parallel_over = 'resamples'),
  metrics = metric_set(sens, spec, npv, ppv, roc_auc, f_meas)
)

rs |> autoplot()

heatmap <- function(data, metric){
# data must be classification tuning results from tune_*
  data |>
    collect_metrics(summarize = T) |>
    filter(.metric == metric) |>
    filter(mean > 0.85) |>
    ggplot(aes(mixture, penalty, fill = mean)) +
    geom_tile(color = 'white') +
    scale_y_log10() +
    scale_fill_viridis_c()
}

c('sens', 'spec', 'npv', 'ppv', 'roc_auc', 'f_meas') |>
  map(~rs |> heatmap(metric = .x) +
        labs(subtitle = .x)) |>
  patchwork::wrap_plots(guides = 'collect')


rs |> show_best(metric = 'f_meas', n = 10)
rs |> show_best(metric = 'roc_auc', n = 10)
best <- rs |> show_best(metric = 'roc_auc', n = 10) |>
  filter(mixture == 0) |>
  arrange(desc(mean), desc(penalty)) |>
  slice(1)

final_model <- wkflw |>
  tune::finalize_workflow(parameters = best) |>
  fit(data = training(datasplit))

test_rs <-
  testing(datasplit) |>
  select(diagnosis) |>
  bind_cols(
    pred = predict(final_model, new_data = testing(datasplit), type = 'prob'),
    pred_class = predict(final_model, new_data = testing(datasplit), type = 'class'),
  )

test_rs |> conf_mat(truth = diagnosis, estimate = .pred_class)
test_rs |> sens(truth = diagnosis, estimate = .pred_class)
test_rs |> roc_auc(truth = diagnosis, estimate = .pred_malignant)



