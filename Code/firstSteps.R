library(caret)
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
source("./methodsForPredictions.R")

data_train <- fread("./Data/house-prices-advanced-regression-techniques/train.csv")
data_test <- fread("./Data/house-prices-advanced-regression-techniques/test.csv")
sampleSubmission <- fread("./Data/house-prices-advanced-regression-techniques/sample_submission.csv")

data_test$SalePrice <- sampleSubmission$SalePrice

testRows <- nrow(data_train)

data <- rbind(data_train,data_test)

numberOfBuckets <- 1000

############################################################################
#################### Preprocess DATA #######################################
############################################################################

visualizeMissingData(data)

data <- preProcess(data)

# str(data)


data_train <- data[1:testRows,]
data_test <- data[-(1:testRows),]

data_train$salePriceIndex <- as.numeric(cut2(data_train$SalePrice, g=numberOfBuckets))
bucket = unique(data_train$salePriceIndex)
buckets <- data.table(bucket = bucket, 
                      minPrice = sapply(bucket,function(i) min(data_train[salePriceIndex == i,SalePrice])), 
                      maxPrice = sapply(bucket,function(i) max(data_train[salePriceIndex == i,SalePrice]))
)
buckets <- buckets[order(bucket),]

for (i in seq_len(length(buckets$bucket))){
  if (i != 1){
    buckets[bucket == i, minPrice := buckets[bucket == i-1, maxPrice]+1]
  }
}


data_test[, salePriceIndex := 
            sapply(data_test$SalePrice, function(i) 
              
              ifelse(min(buckets[,minPrice]) > i, 1, ifelse(max(buckets[,maxPrice]) < i, 100, buckets[minPrice<=i & maxPrice>=i, bucket]))
            )
]
salePrices_train <- data_train[,.(SalePrice, Id, salePriceIndex )]
salePrices_test <- data_test[,.(SalePrice, Id, salePriceIndex)]

setnames(salePrices_train,c("SalePrice","salePriceIndex"), c("SalePrice_train","SalePriceIndex_train"))
setnames(salePrices_test,c("SalePrice","salePriceIndex"), c("SalePrice_test","SalePriceIndex_test"))


data_train_for_correlations <- data_train

data_train[,SalePrice := NULL][,Id := NULL][, salePriceIndex := NULL]
data_test[,SalePrice := NULL][,Id := NULL][, salePriceIndex := NULL]

############################################################################
#################### Preprocess DATA #######################################
############################################################################


############################################################################
#################### Analysis of main components (clustering)###############
############################################################################

# in this section we explore multiple ways of analysing the principal components of the data.
# During a PCA a linear transformation is performed. The data wil be transformed
# to a new base. The base is the set of Eigenvectors. The corresponding Eigenvalues 
# show the influence on the variance of the main component. The higher the eigenvalue, 
# the more variance is explained by the principle component.

# Verfahren: DBScan, Markov Chain Monte Carlo Clustering

############# correlation analysis #########################################

# correlations <- cor(data_train_for_correlations)
# p_values <- cor.mtest(data_train_for_correlations)



# visualize correlation matrix of integer values. Only highlight correlations with
# p-value above 0.01. only usable after imputation of missing values. After One Hot Encdoing the visual is useless.

# col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
# corrplot(correlations, method="color", col=col(200),  
#          type="upper", order="hclust", 
#          
#          tl.col="black", tl.srt=45, #Text label color and rotation
#          # Combine with significance
#          p.mat = p_values, sig.level = 0.01, insig = "blank", 
#          # hide correlation coefficient on the principal diagonal
#          diag=FALSE 
# )



############# correlation analysis #########################################

############################################################################
################## PCA #####################################################
############################################################################


data_train.pca <- prcomp(data_train, center = TRUE, scale. = TRUE)

summary(data_train.pca)

plot(data_train.pca, type = "l")
##at PC155 we explain more than 90% of variance

################ PCA with decision tree ####################################################

finalPC_DecisionTree <- 7

decisionTreePCA <- pcaDecisionTree(SalePrice = salePrices_train, 
                                   data_train.pca = data_train.pca, 
                                   finalPC = finalPC_DecisionTree, 
                                   data_test = data_test,
                                   salePrices_test = salePrices_test,
                                   method = "anova",
                                   depth = 0.005
                                     )

decisionTreePCA_Prediction <- decisionTreePCA$prediction
decisionTreePCA_mse <- decisionTreePCA$mse
decisionTreePCA_mse

################ PCA with decision tree ####################################################

############# PCA with random forest ########################################################

finalPC_RandomForest <- 7

randomForestPCA <- pcaRandomForest(SalePrice = salePrices_train, 
                                   data_train.pca = data_train.pca, 
                                   finalPC = finalPC_RandomForest, 
                                   data_test = data_test,
                                   salePrices_test = salePrices_test,
                                   method = "anova",
                                   depth = 0.005,
                                   numberOfTress = 100,
                                   numberOfFeaturesUsedInTreeConstruction = 5,
                                   importanceOfVariablesCalculated = TRUE
                                     )




randomForestPCA_Prediction <- randomForestPCA$prediction
randomForestPCA_mse <- randomForestPCA$mse
randomForestPCA_mse


############# PCA with random forest ########################################################
# # library(devtools)
# # install_github("vqv/ggbiplot")
# 
# library(ggbiplot)
# ggbiplot(data_train.pca)
################ PCA with decision tree ####################################################


# Radom Forest, Gradient Boosting with decision tree (xgBoost often used for trees)  
# 


############################################################################
#################### Choice of Methodes ####################################
############################################################################

############################################################################
#################### Training of models ####################################
############################################################################

############################################################################
#################### Comparison of results #################################
############################################################################





