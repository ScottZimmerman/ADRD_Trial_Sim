function(dirs,
         p,
         varName,
         coefficients,
         formula,
         uncertainty){

  #Main functions returned for use
  fit <- function(d){
    print("fit not meaningful for combineCohorts.R")
    return(NA)
  }

  combine <- function(d){
    d_new <- NA
    for(i in 1:nrow(coefficients)){
      var <- coefficients[i,"var"]
      cohort <- coefficients[i,"cohort"]
      d_temp <- d[d$cohort==cohort,]
      d_temp[varName] <- d_temp[var]
      if(is.na(d_new)){
        d_new <- d_temp
      }else{
        d_new <- rbind(d_new,d_temp)
      }
    }
    d_new <- arrange(d_new,ID)
    return(d_new)
  }
  setup <- combine

  simulate <- combine

  return(list(fit=fit,
              setup=setup,
              simulate=simulate))
}
