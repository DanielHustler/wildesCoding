  
visualizeMissingData <- function(data){
  
  missing.values <- missingValues(data)
  
  simplePlot <- missing.values %>%
    ggplot() +
    geom_bar(aes(x=key, y=num.missing), stat = 'identity') +
    labs(x='variable', y="number of missing values", 
         title='Number of missing values') +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  
  
  
  missing.values.percentage <- missinValuesPercentage(data)
  
  
  
  levels <- (missing.values.percentage  %>% filter(isna == T) %>%     
               arrange(desc(pct)))$key
  
  percentage.plot <- missing.values.percentage %>%
    ggplot() +
      geom_bar(aes(x = reorder(key, desc(pct)), 
                 y = pct, fill=isna), 
             stat = 'identity', alpha=0.8) +
    scale_x_discrete(limits = levels) +
    scale_fill_manual(name = "", 
                      values = c('steelblue', 'tomato3'), 
                      labels = c("Present", "Missing")) +
    coord_flip() +
    labs(title = "Percentage of missing values", 
         x = 'Variable', y = "% of missing values")
  
 
 
 
  
 grid.arrange(percentage.plot, simplePlot, ncol = 2)

}
  
  
  
cleanData = function(data){

 data_char <- sapply(data, function(y) is.character(y))
  
  col <- names(data_char[which(data_char==TRUE)])
  
  data[,..col] <- factor(data[,..col])
  
  na_count <-sapply(data, function(y) sum(length(which(is.na(y)))))
  
  unique_count <- sapply(data, function(y) sum(length(unique(y))))
  
  col_na <- names(na_count[which(na_count >0)])
  
  
  temp <- col_na[1]
  for(i in seq_len(length(col_na)-1)){
    temp <- paste(temp, col_na[i], sep = " + ")
  }
  
  data_imp <- aregImpute(~ c(col_na), data = data, n.impute = 5)
  
  
  return(data)
}

missinValuesPercentage <- function(data){
  
 missing.values.percentage <- data %>%
    gather(key = "key", value = "val") %>%
    mutate(isna = is.na(val)) %>%
    group_by(key) %>%
    mutate(total = n()) %>%
    group_by(key, total, isna) %>%
    summarise(num.isna = n()) %>%
    mutate(pct = num.isna / total * 100)
 
 return(missing.values.percentage)
 
}

missingValues <- function(data){
  
  missing.values <- data %>%
    gather(key = "key", value = "val") %>%
    mutate(is.missing = is.na(val)) %>%
    group_by(key, is.missing) %>%
    summarise(num.missing = n()) %>%
    filter(is.missing==T) %>%
    select(-is.missing) %>%
    arrange(desc(num.missing))
  
  return(missing.values)
}

replaceNA = function(data) {
  
}

imputation = function(data){
    
  return(data)
  
}


# mat : is a matrix of data
# ... : further arguments to pass to the native R cor.test function
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}

replaceNAinColumns = function(data,columns,nameOfColumns) {
  
  for (i in columns){
    
    if (sapply(data,class)[[i]] == "character"){
      
      data[is.na(get(i)) , (i):=paste0("No ",nameOfColumns)]
      
    }else{
      
      data[is.na(get(i)) , (i):= 0]
      
    }
  }
}

preProcess <- function(data){
  
  ## get columns of type character with more than 10% missing data. 
  ## In those cases create new category "ValueMissing"
  
  missing.values.percentage <- as.data.table(missinValuesPercentage(data))
  
  col_Missing_ge_10_pct <- missing.values.percentage[which(missing.values.percentage$pct >= 10 & missing.values.percentage$isna == T),]$key
  
  for (i in col_Missing_ge_10_pct){
    
    if (sapply(data,class)[[i]] == "character"){
      
      data[is.na(get(i)) , (i):=paste0(i,"_missingValue")]
      
    }
  }
  
  ## get columns with missing values of type character and integer separately to prepare imputation
  
  data_classes <- sapply(data,class)
  
  col_replaced <- col_Missing_ge_10_pct[which(data_classes[col_Missing_ge_10_pct] == "character")]
  col_to_be_replaced <- setdiff(missing.values.percentage$key[which(missing.values.percentage$isna == TRUE)], col_replaced)
  
  col_char <- unique(names(data)[which(names(data) %in% names(data)[sapply(data,is.character)])])
  col_int <- setdiff(names(data), col_char)
  
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
  
  data <- data[complete.cases(data),]
  
  data_char <- data[,..col_char]
  data_int <- data[,..col_int]
  
  
  #one-hot encode all categorial variables
  
  dmy <- dummyVars(" ~ .", data = data_char)
  trsf <- data.frame(predict(dmy, newdata = data_char))
  data <- cbind(data_int, trsf)
  
  return(data)
}

includeSaleBuckets <- function(data, numberOfClasses){
  
  width_of_buckets <- (max(data[, SalePrice]) - min(data[, SalePrice])) / nrow(data)
  data <- data[, SalePriceBucket := width_of_buckets*round(SalePrice / width_of_buckets, digits = 0 )]
  
}