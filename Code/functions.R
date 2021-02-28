  
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
  
  for (i in col_Missing_ge_10_pct){
    
    if (sapply(data,class)[[i]] == "character"){
      
      data[is.na(get(i)) , (i):=paste0(i,"_missingValue")]
      
    }
  }
}

imputation = function(data){
    
  return(data)
  
}
