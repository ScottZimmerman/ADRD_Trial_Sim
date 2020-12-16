function(d,varName){
  d[,varName] <- rbinom(nrow(d),1,0.5)
  return(d)
}