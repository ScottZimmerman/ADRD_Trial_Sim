function(dirs,
         p,
         varName,
         coefficients,
         formula,
         uncertainty){
  #Data
  model <- NA

  createAdditionalVariables <- function(d){
    hba1c.c <- d[,"hba1c"]-6.5
    d[,"hba1c_U65"] <- (hba1c.c<0)*(hba1c.c)
    d[,"hba1c_O65"] <- (hba1c.c>=0)*(hba1c.c)
    return(d)
  }

  #Main functions returned for use
  fit <- function(d){
    return(NA)
  }

  setup <- function(d){
    d[,varName] <- runif(nrow(d),4.5,9)
    d <- createAdditionalVariables(d)
    return(d)
  }

  simulate <- function(d){
    intensiveTarget <- coefficients[coefficients$arm=="intensive","target"]
    standardTarget <- coefficients[coefficients$arm=="standard","target"]

    d[,varName] <- (d$arm == "intensive")*intensiveTarget+
                   (d$arm == "standard")*standardTarget
    d <- createAdditionalVariables(d)
    return(d)
  }

  return(list(fit=fit,
              setup=setup,
              simulate=simulate))
}
