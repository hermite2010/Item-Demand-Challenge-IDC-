## Libraries I am going to need
library(tidyverse)
library(tidymodels)
library(vroom)      # FOR WRITING CSV
library(poissonreg) # FOR POISSON REGRESSION
library(rpart)      # FOR DECISION TREE
library(ranger)     # FOR RANDOM FOREST
library(stacks)     # FOR STACKING
library(xgboost)    # FOR BOOSTED TREES
library(parsnip)    # FOR BART
library(dbarts)
library(timetk)     # FOR EDA
library(modeltime)
library(forecast)

## Read in the data
itemTrain <- vroom("Item-Demand-Challenge-IDC-/train.csv")
itemTest <- vroom("Item-Demand-Challenge-IDC-/test.csv")

subtrain_1 <- itemTrain %>% 
  filter(store ==9, item ==36)
subtrain_2 <- itemTrain %>% 
  filter(store ==5, item ==50)

# Pull out Holidays? Days of the Week? Month?
ide_recipe_sub_1 <- recipe(sales ~., data = subtrain_1) %>% 
  step_date(date, features = c('dow','month','year','doy')) %>% 
  step_range(date_doy, min=0,max=pi) %>% 
  step_mutate(sinDOY=sin(date_doy), cosDOY=cos(date_doy)) %>% 
  #step_lag(sales, lag = 7)
  step_holiday(holidays = c("ChristmasDay"))

prepped_recipe <- prep(ide_recipe)
bake(prepped_recipe, new_data = itemTrain) #Make sure recipe work on train
bake(prepped_recipe, new_data = itemTest) #Make sure recipe works on test

# Random Forest -----------------------------------------------------------
tree_mod <- rand_forest(mtry = tune(),
                        min_n = tune(),
                        trees = 500) %>% 
  set_engine("ranger") %>% 
  set_mode("regression")

## Workflow and model and recipe
tree_wf <- workflow() %>%
  add_recipe(ide_recipe) %>%
  add_model(tree_mod)

## set up grid of tuning values
tuning_grid <- grid_regular(mtry(range = c(1,(ncol(storeItem)))),
                            min_n(),
                            levels = 5)

## set up k-fold CV
folds <- vfold_cv(storeItem, v = 5, repeats=1)

## Run the CV
CV_results <- tree_wf %>%
  tune_grid(resamples=folds,
            grid=tuning_grid,
            metrics=metric_set(smape)) #Or leave metrics NULL

## find best tuning parameters
bestTune <- CV_results %>%
  select_best("smape")

collect_metrics(CV_results) %>% 
  filter(mtry == as.numeric(bestTune[1]), 
         min_n ==as.numeric(bestTune[2])) %>% 
  pull('mean')




# ARIMA -------------------------------------------------------------------
subtrain_1 <- itemTrain %>% 
  filter(store ==9, item ==36) %>% 
  select(-store,-item)

subtrain1_test <- itemTest %>%
  filter(store==9, item==36) %>%
  select(-store,-item)

subtrain_2 <- itemTrain %>% 
  filter(store ==5, item ==50) %>% 
  select(-store,-item)

subtrain2_test <- itemTest %>%
  filter(store==5, item==50) %>%
  select(-store,-item)

# Pull out Holidays? Days of the Week? Month?
ide_recipe_sub_1 <- recipe(sales ~., data = subtrain_1) %>% 
  step_date(date, features = c('dow','month','year','doy')) %>% 
  step_range(date_doy, min=0,max=pi) %>% 
  step_mutate(sinDOY=sin(date_doy), cosDOY=cos(date_doy)) %>% 
  #step_lag(sales, lag = 7)
  step_holiday(holidays = c("ChristmasDay"))

# Pull out Holidays? Days of the Week? Month?
ide_recipe_sub_2 <- recipe(sales ~., data = subtrain_2) %>% 
  step_date(date, features = c('dow','month','year','doy')) %>% 
  step_range(date_doy, min=0,max=pi) %>% 
  step_mutate(sinDOY=sin(date_doy), cosDOY=cos(date_doy)) %>% 
  #step_lag(sales, lag = 7)
  step_holiday(holidays = c("ChristmasDay"))

cv_split_1 <- time_series_split(subtrain_1, assess="3 months", cumulative = TRUE)
cv_split_1 %>%
  tk_time_series_cv_plan() %>% #Put into a data frame
  plot_time_series_cv_plan(date, sales, .interactive=FALSE)

cv_split_2 <- time_series_split(subtrain_2, assess="3 months", cumulative = TRUE)
cv_split_2 %>%
  tk_time_series_cv_plan() %>% #Put into a data frame
  plot_time_series_cv_plan(date, sales, .interactive=FALSE)


arima_recipe_1 <- ide_recipe_sub_1 # For the linear model part
arima_recipe_2 <- ide_recipe_sub_2 # For the linear model part
arima_model <- arima_reg(seasonal_period=365, # How long is the searson? 
                         non_seasonal_ar=5, # default max p to tune
                         non_seasonal_ma=5, # default max q to tune
                         seasonal_ar=2, # default max P to tune
                         seasonal_ma=2, #default max Q to tune
                         non_seasonal_differences=2, # default max d to tune
                         seasonal_differences=2 #default max D to tune
                         ) %>%
  set_engine("auto_arima")

arima_wf_1 <- workflow() %>%
  add_recipe(arima_recipe_1) %>%
  add_model(arima_model) %>%
  fit(data=training(cv_split_1))

arima_wf_2 <- workflow() %>%
  add_recipe(arima_recipe_2) %>%
  add_model(arima_model) %>%
  fit(data=training(cv_split_2))

# Cross-validate to tune model
cv_results_1 <- modeltime_calibrate(arima_wf_1,
                                  new_data = testing(cv_split_1))

cv_results_2 <- modeltime_calibrate(arima_wf_2,
                                    new_data = testing(cv_split_2))

## Visualize CV results
image1 <- cv_results_1 %>%
  modeltime_forecast(new_data = testing(cv_split_1),
                   actual_data = subtrain_1) %>%
  plot_modeltime_forecast(.interactive=TRUE)

image2 <- cv_results_2 %>%
  modeltime_forecast(new_data = testing(cv_split_2),
                     actual_data = subtrain_2) %>%
  plot_modeltime_forecast(.interactive=TRUE)

## Evaluate the accuracy
cv_results_1 %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(.interactive = FALSE)

cv_results_2 %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(.interactive = FALSE)

# Refit to all data then forecast
arima_fullfit_1 <- cv_results_1 %>%
  modeltime_refit(data = subtrain_1)

arima_fullfit_2 <- cv_results_2 %>%
  modeltime_refit(data = subtrain_2)

arima_preds_1 <- arima_fullfit_1 %>%
  modeltime_forecast(new_data = subtrain1_test) %>%
  rename(date=.index, sales=.value) %>%
  select(date, sales) %>%
  full_join(., y=subtrain1_test, by="date") %>%
  select(id, sales)

arima_preds_2 <- arima_fullfit_2 %>%
  modeltime_forecast(new_data = subtrain2_test) %>%
  rename(date=.index, sales=.value) %>%
  select(date, sales) %>%
  full_join(., y=subtrain2_test, by="date") %>%
  select(id, sales)

image3 <- arima_fullfit_1 %>%
  modeltime_forecast(new_data = subtrain1_test, actual_data = subtrain_1) %>%
  plot_modeltime_forecast(.interactive=FALSE)

image4 <- arima_fullfit_2 %>%
  modeltime_forecast(new_data = subtrain2_test, actual_data = subtrain_2) %>%
  plot_modeltime_forecast(.interactive=FALSE)

plotly::subplot(image1,image2,image3,image4, nrows = 2)

# Double For Loop ---------------------------------------------------------


nStores <- max(itemTrain$store)
nItems <- max(itemTrain$item)

for(s in 1:nStores){
  for(i in 1:nItems){
    storeItemTrain <- train %>%
      filter(store==s, item==i)
    storeItemTest <- test %>%
      filter(store==s, item==i)
    
    ## Fit storeItem models here
    
    ## Predict storeItem sales
    
    ## Save storeItem predictions
    if(s==1 & i==1){
      all_preds <- preds
    } else {
      all_preds <- bind_rows(all_preds, preds)
    }
    
  }
}
