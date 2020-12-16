function(d,varName,categories){
  d[,varName] <- as.factor(sample(categories,nrow(d),replace=TRUE))
  return(d)
}
