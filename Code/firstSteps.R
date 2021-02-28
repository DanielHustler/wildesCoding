
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
library(naniar)
library(UpSetR)

source("./functions.R")

data <- fread("./Data/house-prices-advanced-regression-techniques/train.csv")
data_test <- fread("./Data/house-prices-advanced-regression-techniques/test.csv")


############################################################################
#################### VISUALISATION OF DATA #################################
############################################################################

visualizeMissingData(data)

## get columns of type character with more than 10% missing data. 
## In those cases create new category "ValueMissing"

missing.values.percentage <- as.data.table(missinValuesPercentage(data))

col_Missing_ge_10_pct <- missing.values.percentage[which(missing.values.percentage$pct >= 10 & missing.values.percentage$isna == T),]$key

replaceNA(data)

## get columns with missing values of type character and integer separately to prepare imputation

data_classes <- sapply(data,class)

col_replaced <- col_Missing_ge_10_pct[which(data_classes[col_Missing_ge_10_pct] == "character")]
col_to_be_replaced <- setdiff(missing.values.percentage$key[which(missing.values.percentage$isna == TRUE)], col_replaced)

col_Missing_integer <- unique(col_to_be_replaced[which(col_to_be_replaced %in% names(data)[sapply(data,is.integer)])])
col_Missing_character <- unique(col_to_be_replaced[which(col_to_be_replaced %in% names(data)[sapply(data,is.character)])])


## visualize patterns in missing data 

data[,..col_Missing_integer] %>% as_shadow_upset() %>% upset(,nset = length(names(data[,..col_Missing_integer])))
data[,..col_Missing_character] %>% as_shadow_upset() %>% upset(,nset = length(names(data[,..col_Missing_character])))




############################################################################
#################### VISUALISATION OF DATA #################################
############################################################################

############################################################################
#################### Imputation OF DATA ####################################
############################################################################

############################################################################
#################### Analysis of main components (clustering)###############
############################################################################


# DBScan, Markov Chain Monte Carlo Clustering

############################################################################
#################### Choice of Methodes ####################################
############################################################################

############################################################################
#################### Training of models ####################################
############################################################################

############################################################################
#################### Comparison of results #################################
############################################################################
data <- cleanData(data)



head(data)
