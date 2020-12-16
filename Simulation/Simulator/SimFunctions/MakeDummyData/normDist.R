function(d,varName){
  d[,varName] <- rnorm(nrow(d))
  return(d)
}