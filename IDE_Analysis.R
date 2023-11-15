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

## Read in the data
itemTrain <- vroom("Item-Demand-Challenge-IDC-/train.csv")
itemTest <- vroom("Item-Demand-Challenge-IDC-/test.csv")

storeItem <- itemTrain %>% 
  filter(store ==9, item ==36)

# Pull out Holidays? Days of the Week? Month?
ide_recipe <- recipe(sales ~., data = storeItem) %>% 
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
