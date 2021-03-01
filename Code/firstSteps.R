
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
library(corrplot)

source("./functions.R")

data <- fread("./Data/house-prices-advanced-regression-techniques/train.csv")
# data_test <- fread("./Data/house-prices-advanced-regression-techniques/test.csv")


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

col_int <- unique(names(data)[which(names(data) %in% names(data)[sapply(data,is.integer)])])
col_char <- unique(names(data)[which(names(data) %in% names(data)[sapply(data,is.character)])])


## visualize patterns in missing data 

data[,..col_to_be_replaced] %>% as_shadow_upset() %>% upset(,nset = length(names(data[,..col_to_be_replaced])))

## After taking a look at the missing values it was decided to change all NA in coulmns concerning
## garages, basements and Masonary Veneeers to "No garage", "No basement" and "No masonary veneer". 
## All values of type integer will be set to 0, icluding "LotFrontage".

col_garage <- c("GarageCond","GarageFinish","GarageQual","GarageType","GarageYrBlt")
col_bsmt<- c("BsmtCond","BsmtFinType1","BsmtQual","BsmtExposure","BsmtFinType2")
col_masVnr <- c("MasVnrArea","MasVnrType")
col_electrical <- c("Electrical")
col_LotFrontage <- c("LotFrontage")

replaceNAinColumns(data,col_garage,"garage")
replaceNAinColumns(data,col_bsmt,"basement")
replaceNAinColumns(data,col_masVnr,"masonaryVeneer")
replaceNAinColumns(data,col_electrical,"electrical")
replaceNAinColumns(data,col_LotFrontage,"frontage")

data_int <- data[,..col_int]
data_char <- data[,..col_char]

correlations_int <- cor(data_int)
p_values_int <- cor.mtest(data_int)

# visualize correlation matrix of integer values. Only highlight correlations with
# p-value above 0.01. only usable after imputation of missing values

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(correlations_int, method="color", col=col(200),  
         type="upper", order="hclust", 
         
         tl.col="black", tl.srt=45, #Text label color and rotation
         # Combine with significance
         p.mat = p_values_int, sig.level = 0.01, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag=FALSE 
)
############################################################################
#################### VISUALISATION OF DATA #################################
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
