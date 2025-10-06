---
  title: "Untitled"
format: html
editor: visual
---
  
  ## Quarto
  
  Quarto enables you to weave together content and executable code into a finished document. To learn more about Quarto see <https://quarto.org>.

## Running Code

When you click the **Render** button a document will be generated that includes both content and the output of embedded code. You can embed code like this:
  
  ```{r}
~.-datetime
```

You can add options to executable code like this

# linear regression
```{r}

train_dt <- subset(train, select = -c(casual, registered))

my_linear_model <- linear_reg() %>% 
  set_engine("lm") %>%
  set_mode("regression") %>%
  fit(formula=Response~X1+X2+...., data=train_dt)
bike_predictions <- predict(my_linear_model, new_data= test)
bike_predictions
```

```{r}
# ============================================
# BikeShareAnalysis.R
# Linear Regression Model
# ============================================

# Load libraries
library(tidymodels)  # for modeling
library(vroom)       # for writing csv files
library(dplyr)       # for data wrangling

# ------------------------------------------------
# 1. Fit Linear Regression Model
# ------------------------------------------------
# NOTE: We model log(count) instead of count
# Make sure not to include variables like casual or registered as predictors,
# since they are components of count (the target variable).
# Example predictors: season, weather, holiday, temp, humidity, windspeed, etc.

linear_model <- linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression") %>%
  fit(log(count) ~ season + holiday + workingday + weather +
        temp + atemp + humidity + windspeed,
      data = train)

# ------------------------------------------------
# 2. Make Predictions on Test Set
# ------------------------------------------------
# Model was trained on log(count), so we must exponentiate predictions
predictions <- predict(linear_model, new_data = test) %>%
  mutate(count = exp(.pred)) %>%  # back-transform to original scale
  select(count)

# ------------------------------------------------
# 3. Export Predictions for Kaggle
# ------------------------------------------------
# Kaggle requires a CSV with the same structure as sample_submission.csv
# Usually: id + count
submission <- test %>%
  mutate(datetime = format(datetime, "%Y-%m-%d %H:%M:%S")) %>% 
  select(datetime) %>%
  bind_cols(predictions)

# Write CSV for Kaggle submission
vroom_write(submission, "/Users/dylan/Downloads/my_submission.csv",delim = ",")

# ------------------------------------------------
# 4. Next Steps
# ------------------------------------------------
# - Upload "my_submission.csv" to Kaggle
# - Record your score on Learning Suite
# - Git add/commit/push this script to GitHub

```

The `echo: false` option disables the printing of code (only output is displayed).
```{r}
# ================================================================
# 0. Load Libraries
# ================================================================
library(tidymodels)
library(lubridate)
library(vroom)
library(dplyr)

# ================================================================
# 1. Data Cleaning
# ================================================================
train <- train %>%  # remove casual/registered
  mutate(count = log(count))        # log-transform count

# Test data should remain unchanged except for datetime features
# ================================================================
# 2. Feature Engineering Recipe
# ================================================================
bike_recipe <- recipe(count ~ season + holiday + workingday + weather +
                        temp + atemp + humidity + windspeed +
                        datetime,
                      data = train) %>%
  
  # 1. Recode weather "4" -> "3" and make factor
  step_mutate(weather = ifelse(weather == 4, 3, weather)) %>%
  step_mutate(weather = as.factor(weather)) %>%
  
  # 2. Extract hour from datetime
  step_mutate(hour = lubridate::hour(datetime)) %>%
  step_mutate(hour = as.factor(hour)) %>%
  
  # 3. Make season a factor
  step_mutate(season = as.factor(season)) %>%
  
  # 4. Normalize numeric predictors
  step_normalize(all_numeric_predictors()) %>%
  
  # 5. Convert categorical vars into dummies
  step_dummy(all_nominal_predictors()) %>%
  
  # 6. Drop datetime
  step_rm(datetime)


# ================================================================
# 3. Model Specification
# ================================================================
linear_model <- linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression")

# ================================================================
# 4. Workflow
# ================================================================
bike_workflow <- workflow() %>%
  add_model(linear_model) %>%
  add_recipe(bike_recipe)

# Fit workflow on training data
bike_fit <- fit(bike_workflow, data = train)

# ================================================================
# 5. Predictions on Test Set
# ================================================================
predictions <- predict(bike_fit, new_data = test) %>%
  mutate(count = exp(.pred)) %>%  # back-transform log(count)
  select(count)

# ================================================================
# 6. Create Kaggle Submission
# ================================================================
submission <- test %>%
  mutate(datetime = format(datetime, "%Y-%m-%d %H:%M:%S")) %>%
  select(datetime) %>%
  bind_cols(predictions)

# Write CSV
vroom::vroom_write(submission,
                   "/Users/dylan/Downloads/my_submission.csv",
                   delim = ",")


```
```{r}
# Prep and bake the recipe
prepped_recipe <- prep(bike_recipe, training = train)
baked_train <- bake(prepped_recipe, new_data = train)

# Show the first five rows
head(baked_train, 5)

```

```{r}
preg_model <- linear_reg(penalty=1, mixture=.5) %>% 
  #Set model and tuning10
  set_engine("glmnet") # Function to fit in R11
preg_wf <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(linear_model) %>%
  fit(data=train)
vs <- predict(preg_wf, new_data=test)
submission1 <- test %>%
  mutate(datetime = format(datetime, "%Y-%m-%d %H:%M:%S")) %>% 
  select(datetime) %>%
  bind_cols(vs)
names(submission1)[2] <- "count"
vroom::vroom_write(submission1,
                   "/Users/dylan/Downloads/my_submission1.csv",
                   delim = ",")
```

```{r}
names(train)
```


```{r}
library(tidymodels)
library(lubridate)
library(dplyr)

library(tidymodels)
library(lubridate)
library(dplyr)

bike_recipe <- recipe(count ~ season + holiday + workingday + weather +
                        temp + atemp + humidity + windspeed +
                        datetime,
                      data = train) %>%
  
  
  step_mutate(weather = if_else(weather == 4, 3L, weather, missing = weather)) %>%
  
  
  step_mutate(
    season  = factor(season),
    weather = factor(weather)
  ) %>%
  
  
  
  
  step_mutate(hour = lubridate::hour(datetime)) %>%
  step_mutate(hour = factor(hour)) %>%
  
  step_date(datetime, features="dow") %>%
  step_interact( ~ hour:workingday + temp:humidity ) %>%
  step_mutate(
    hour_sin = sin(2 * pi * hour(datetime) / 24),
    hour_cos = cos(2 * pi * hour(datetime) / 24)
  )  %>%
  
  step_rm(datetime) %>%
  step_interact( ~ temp:holiday) %>%
  
  
  
  
  
  
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
  
  
  step_normalize(all_numeric_predictors())


prepped_recipe <- prep(bike_recipe)
train_processed <- bake(prepped_recipe, new_data = test)


names(train_processed)


vroom::vroom_write(
  train_processed,
  "/Users/dylan/Downloads/train_processed.csv",
  delim = ","
)








```

# tuning
```{r}
preg_model <- linear_reg(penalty=tune(),
                         mixture=tune()) %>% 
  set_engine("glmnet") 
preg_wf <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(preg_model) 

grid_of_tuning_params <- grid_regular(penalty(), 
                                      mixture(),
                                      levels= 5)
folds <- vfold_cv(train, v= 5, repeats=1)

```

```{r}
CV_results <- preg_wf %>% 
  tune_grid(resamples=folds,
            grid=grid_of_tuning_params,
            metric=metric_set(rmse, mae))
collect_metrics(CV_results) %>%
  filter(.metric=="rmse") %>%
  ggplot(data=., aes(x=penalty, y=mean, color=factor(mixture))) +
  geom_line()
bestTune <- CV_results %>%
  select_best(metric="rmse")

final_wf <- 
  preg_wf %>%
  finalize_workflow(bestTune) %>%
  fit(data=train)
final_wf %>%
  predict(new_data=test)

```

```{r}
show_notes(CV_results, n = Inf)
```
```{r}
library(tidymodels)
my_mod <- decision_tree(tree_depth = tune(),
                        cost_complexity = tune(),
                        min_n=tune()) %>%
  set_engine("rpart") %>%
  set_mode("regression")

preg_wf <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(my_mod) 

grid_of_tuning_params <- grid_regular(penalty(), 
                                      mixture(),
                                      levels= 5)
folds <- vfold_cv(train, v= 5, repeats=1)
```

```{r}
# Install engine if needed
# install.packages("rpart")

library(tidymodels)

# 1. Specify model
my_mod <- decision_tree(
  tree_depth = tune(),
  cost_complexity = tune(),
  min_n = tune()
) %>%
  set_engine("rpart") %>%
  set_mode("regression")

# 2. Workflow (model + recipe)
preg_wf <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(my_mod)

# 3. Grid of tuning values
grid_of_tuning_params <- grid_regular(
  tree_depth(),
  cost_complexity(),
  min_n(),
  levels = 5
)

# 4. K-fold cross-validation
set.seed(123) # reproducibility
folds <- vfold_cv(train, v = 5)

# 5. Tune the model
tuned_res <- tune_grid(
  preg_wf,
  resamples = folds,
  grid = grid_of_tuning_params,
  metrics = metric_set(rmse, rsq)  # common regression metrics
)

# 6. Select best tuning parameters
best_params <- select_best(tuned_res, metric="rmse")

# 7. Finalize workflow with best params
final_wf <- finalize_workflow(preg_wf, best_params)

# 8. Fit final model on full training data
final_fit <- fit(final_wf, data = train)

# 9. Predict on new/test data
preds <- predict(final_fit, new_data = test)

# 10. Evaluate (if you have true test values)
metrics <- bind_cols(test, preds) %>%
  metrics(truth = count, estimate = .pred)

metrics

```
```{r}
best_params <- select_best(tuned_res, metric="rmse")

# 7. Finalize workflow with best params
final_wf <- finalize_workflow(preg_wf, best_params)

# 8. Fit final model on full training data
final_fit <- fit(final_wf, data = train)

# 9. Predict on new/test data
preds <- predict(final_fit, new_data = test)

# 10. Evaluate (if you have true test values)
metrics <- bind_cols(test, preds) %>%
  metrics(truth = count, estimate = .pred)

metrics

```
```{r}
library(tidymodels)

# --- MODEL ---
my_mod <- decision_tree(
  tree_depth = tune(),
  cost_complexity = tune(),
  min_n = tune()
) %>%
  set_engine("rpart") %>%
  set_mode("regression")

# --- WORKFLOW ---
preg_wf <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(my_mod)

# --- GRID ---
grid_of_tuning_params <- grid_regular(
  tree_depth(),
  cost_complexity(),
  min_n(),
  levels = 5
)

# --- CV ---
folds <- vfold_cv(train, v = 5)

# --- TUNE ---
tuned_res <- tune_grid(
  preg_wf,
  resamples = folds,
  grid = grid_of_tuning_params,
  metrics = metric_set(rmse, rsq)
)

# --- SELECT BEST ---
best_params <- select_best(tuned_res, metric = "rmse")

# Check what we got
print(best_params)

# --- FINALIZE ---
final_wf <- finalize_workflow(preg_wf, best_params)

# --- FIT ---
final_fit <- fit(final_wf, data = train)

```

```{r}
train_preds <- predict(final_fit, train) %>%
  bind_cols(train %>% select(count))

head(train_preds)
```

```{r}
library(dplyr)
library(vroom)
library(tidymodels)

# 1. Predict on test set
test_preds <- predict(final_fit, new_data = test)
test_preds <- exp(test_preds)
# 2. Combine predictions with original datetime
submission7 <- test %>%
  select(datetime) %>%           # keep original datetime
  bind_cols(test_preds) %>%      # add predicted counts
  rename(count = .pred) %>%     # rename for submission
  mutate(datetime = format(datetime, "%Y-%m-%d %H:%M:%S"))  # format datetime

# 3. Write CSV
vroom::vroom_write(
  submission7,
  "/Users/dylan/Downloads/my_submission7.csv",
  delim = ","
)

```

```{r}
library(tidymodels)
library(ranger)
my_mod <- rand_forest(mtry = tune(), 
                      min_n=tune(),
                      trees= 1000) %>%
  set_engine("ranger") %>%
  set_mode("regression")
preg_wf <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(my_mod)
mygrid <- grid_regular(mtry(range=c(1,12)),
                       min_n(),
                       levels = 5)
folds <- vfold_cv(train, v = 5)

# --- TUNE ---
tuned_res <- tune_grid(
  preg_wf,
  resamples = folds,
  grid = mygrid,
  metrics = metric_set(rmse, rsq)
)

# --- SELECT BEST ---
best_params <- select_best(tuned_res, metric = "rmse")

# Check what we got
print(best_params)

# --- FINALIZE ---
final_wf <- finalize_workflow(preg_wf, best_params)

# --- FIT ---
final_fit <- fit(final_wf, data = train)


```
```{r}
test_preds <- predict(final_fit, new_data = test)
test_preds <- exp(test_preds)
# 2. Combine predictions with original datetime
submission8 <- test %>%
  select(datetime) %>%           # keep original datetime
  bind_cols(test_preds) %>%      # add predicted counts
  rename(count = .pred) %>%     # rename for submission
  mutate(datetime = format(datetime, "%Y-%m-%d %H:%M:%S"))  # format datetime

# 3. Write CSV
vroom::vroom_write(
  submission8,
  "/Users/dylan/Downloads/my_submission8.csv",
  delim = ","
)
```

```{r}
library(bonsai)
library(lightgbm)
library(dbarts)
lgbm_mod <- boost_tree(
  trees = tune(),
  tree_depth = tune(),
  learn_rate = tune()
) %>%
  set_engine("lightgbm") %>%
  set_mode("regression")


```
```{r}

library(tidymodels)
library(bonsai)

# LightGBM model spec
lgbm_mod <- boost_tree(
  trees = tune(),
  learn_rate = tune(),
  tree_depth = tune()
) %>%
  set_engine("lightgbm") %>%
  set_mode("regression")

# Workflow
reg_wf <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(lgbm_mod)

# Regular tuning grid
mygrid <- grid_regular(
  trees(range = c(100, 1000)),
  learn_rate(range = c(-3, -0.1)),  # log10 scale, so 0.001–0.8
  tree_depth(range = c(3, 12)),
  levels = 5
)

# Cross-validation folds
folds <- vfold_cv(train, v = 5)

# --- TUNE ---
tuned_res <- tune_grid(
  reg_wf,
  resamples = folds,
  grid = mygrid,
  metrics = metric_set(rmse, rsq)
)

# --- SELECT BEST ---
best_params <- select_best(tuned_res, metric = "rmse")
print(best_params)

# --- FINALIZE ---
final_wf <- finalize_workflow(reg_wf, best_params)

# --- FIT ---
final_fit <- fit(final_wf, data = train)


```
```{r}
test_preds <- predict(final_fit, new_data = test)
test_preds <- exp(test_preds)
# 2. Combine predictions with original datetime
submission9 <- test %>%
  select(datetime) %>%           # keep original datetime
  bind_cols(test_preds) %>%      # add predicted counts
  rename(count = .pred) %>%     # rename for submission
  mutate(datetime = format(datetime, "%Y-%m-%d %H:%M:%S"))  # format datetime

# 3. Write CSV
vroom::vroom_write(
  submission9,
  "/Users/dylan/Downloads/my_submission9.csv",
  delim = ","
)
```

```{r}
library("agua")
h2o::h2o.init()

auto_model <- auto_ml() %>%
  set_engine("h2o", max_runtime_secs=600, max_models=10 ) %>%
  set_mode("regression")

automl_wf <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(auto_model) %>%
  fit(data=train)

preds <- predict(automl_wf, new_data = test)
```
```{r}
test_preds <- exp(preds)
# 2. Combine predictions with original datetime
submission10 <- test %>%
  select(datetime) %>%           # keep original datetime
  bind_cols(test_preds) %>%      # add predicted counts
  rename(count = .pred) %>%     # rename for submission
  mutate(datetime = format(datetime, "%Y-%m-%d %H:%M:%S"))  # format datetime

# 3. Write CSV
vroom::vroom_write(
  submission10,
  "/Users/dylan/Downloads/my_submission10.csv",
  delim = ","
)
```

```{r}
bart_model <- bart(trees=tune()) %>% # BART figures out depth and learn_rate9
  set_engine("dbarts") %>% # might need to install10
  set_mode("regression")
reg_wf <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(bart_model)

# Regular tuning grid
mygrid <- grid_regular(
  trees(range = c(100, 1000)),
  learn_rate(range = c(-3, -0.1)),  # log10 scale, so 0.001–0.8
  tree_depth(range = c(3, 12)),
  levels = 5
)

# Cross-validation folds
folds <- vfold_cv(train, v = 5)

# --- TUNE ---
tuned_res <- tune_grid(
  reg_wf,
  resamples = folds,
  grid = mygrid,
  metrics = metric_set(rmse, rsq)
)

# --- SELECT BEST ---
best_params <- select_best(tuned_res, metric = "rmse")
print(best_params)

# --- FINALIZE ---
final_wf <- finalize_workflow(reg_wf, best_params)

# --- FIT ---
final_fit <- fit(final_wf, data = train)

```