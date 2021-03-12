pcaDecisionTree <- function(SalePrice, data_train.pca, finalPC, data_test, salePrices_test, method, depth){
 
  #add a training set with principal components
  train.data <- data.frame(SalePrice = SalePrice$SalePriceIndex_train, data_train.pca$x)
  
  #we are interested in first 155 PCAs
  train.data <- train.data[,1:(finalPC+1)]
  
  #run a decision tree
  library(rpart)
  library(rpart.plot)
  rpart.model <- rpart(SalePrice ~ .,data = train.data, method = method, control = rpart.control(cp = depth))
  rpart.plot(rpart.model,box.palette="blue")
  
  #transform test into PCA
  test.data <- predict(data_train.pca, newdata = as.data.table(data_test))
  test.data <- as.data.frame(test.data)
  
  #select the first 155 components
  test.data <- test.data[,1:155]
  
  #make prediction on test data
  rpart.prediction <-round( predict(rpart.model, test.data, type = ), digits = 0)
  
  test_pred <- cbind(data.table(rpart.prediction), salePrices_test)
  
  salePricePredictionSpace <-unique(salePrices_train[, SalePrice_Prediction := mean(SalePrice_train), by = "SalePriceIndex_train"][,.(SalePriceIndex_train, SalePrice_Prediction)])
  
  test_pred <- merge(test_pred, salePricePredictionSpace, by.x = "rpart.prediction", by.y = "SalePriceIndex_train", all.x = TRUE)
  test_pred<-test_pred[order(Id),]
  test_pred$sq_error = (test_pred$SalePrice_test - test_pred$SalePrice_Prediction)^2
  mse <- sum(test_pred[which(is.na(SalePrice_Prediction)!=TRUE), sq_error])
  result <- list(prediction = test_pred, mse = mse)
  return(result)
}