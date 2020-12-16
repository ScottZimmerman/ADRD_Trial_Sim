function(d,varName,resid_sd){
  resid <- rnorm(nrow(d),mean=0, sd=resid_sd)
  d[,varName] <- d[,varName] + resid
  return(d)
}