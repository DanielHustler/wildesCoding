
library(mlr3verse)
library(shiny)
library(R6)
library(lattice)
library(GGally)
library(Hmisc)
library(assertr)
library(stringr)
library(data.table)
library(lubridate)
library(rpart)
library(rpart.plot)
library(kknn)
library(ranger)
library(xgboost)
library(DALEX)
library(DALEXtra)
library(quantreg)
library(tidyverse)
library(ggplot2)
library(gridExtra)


source("./functions.R")
#### Beispiel für Pipeline-Operationen #####
# 
# x <- -10:10
# x %>% abs %>% sqrt %>% `[`(. != 0) %>% logb(base = 5) %>% round(digits = 2) %>%
#   abs %>% c(1:3, ., 4:6) %>% cumsum %>% sum

############################################

data <- fread("./Data/house-prices-advanced-regression-techniques/test.csv")

visualiseMissingData(data)

data <- cleanData(data)



head(data)
