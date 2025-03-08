library(tidyverse)
library(tidymodels)
library(finetune)
library(future)
library(purrr)
library(furrr)
library(textrecipes)
library(themis)
library(corrr)

library(bonsai)
library(lightgbm)
library(xgboost)
library(ranger)

library(readr)
library(janitor)
library(lubridate)
library(ISLR2)
library(ISLR)

lr_spec <- logistic_reg( )|>
  set_engine('glm')|>
  set_mode('classification')


data_path <- '../input/playground-series-s5e3/'
train<-
  readr::read_csv(file.path(data_path, 'train.csv'),
                  show_col_types = FALSE)|>
  mutate(rainfall=as.factor(rainfall))
test <-
  readr::read_csv(file.path(data_path, 'test.csv'),
                  show_col_types = FALSE)
submission <-  readr::read_csv(file.path(data_path, 'sample_submission.csv'),show_col_types = FALSE)
set.seed(1142)
df_split <- initial_split(train, prop = 0.8, strata = rainfall)
train_set <- training(df_split)
test_set <- testing(df_split)
cv_folds <- vfold_cv(train_set,
                     v = 5,
                     repeats = 1,
                     strata = rainfall)

lr_rcp_bs <-
  recipe(rainfall ~ ., data = train_set)
v1_rcp<- lr_rcp_bs |>
  step_rm(id,day)|>
  step_log(all_numeric_predictors(),offset=1,skip=FALSE)|>
  step_normalize(all_numeric_predictors())
  step_zv(all_numeric_predictors())|>
  step_corr(all_predictors())
tmp_rcps <- list(bs=lr_rcp_bs,
  v1=v1_rcp)


lr_w_fit <-
  workflow_set(preproc = tmp_rcps,
               models=list(glm=lr_spec))|>
  workflow_map(fn='fit_resamples',
               metrics=metric_set(roc_auc),
               resamples=cv_folds
               #,verbose=TRUE
  )


lr_w_fit |> collect_metrics()|>select(wflow_id,n, .metric, mean,std_err)
