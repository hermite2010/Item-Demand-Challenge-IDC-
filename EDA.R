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
library(gridExtra)  

## Read in the data
itemTrain <- vroom("Item-Demand-Challenge-IDC-/train.csv")
itemTest <- vroom("Item-Demand-Challenge-IDC-/test.csv")

head(itemTrain)

itemTrain %>%
  plot_time_series(date, sales, .interactive=FALSE)

plot1 <- itemTrain %>%
  filter(item == 2, store == 10) %>% 
  pull(sales) %>% 
  forecast::ggAcf(.)+
  ggtitle("Item 2 and Store 10")
plot2 <- itemTrain %>%
  filter(item == 5, store == 5) %>% 
  pull(sales) %>% 
  forecast::ggAcf(.)+
  ggtitle("Item 5 and Store 5")
plot3 <- itemTrain %>%
  filter(item == 8, store == 9) %>% 
  pull(sales) %>% 
  forecast::ggAcf(.)+
  ggtitle("Item 8 and Store 9")
plot4 <- itemTrain %>%
  filter(item == 3, store == 1) %>% 
  pull(sales) %>% 
  forecast::ggAcf(.)+
  ggtitle("Item 3 and Store 1")
grid.arrange(plot1,plot2,plot3,plot4)

