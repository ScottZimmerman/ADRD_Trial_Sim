function(dirs,
         p,
         varName,
         coefficients,
         formula,
         uncertainty){
  #Data
  model.env <- environment()
  model <- NA

  #Helper functions: used by main functions but not returned
  makeDummyData <- dget(file.path(dirs$makeDummyData,"normDist.R"))

  replaceCoefficients <- function(model){
    for(coefName in names(model$coefficients)){
      if(!(coefName %in% rownames(coefficients))){
        stop(paste0("Error ",varName," missing coefficient for ",coefName))
      }
      model$coefficients[[coefName]] <- coefficients[coefName,"Estimate"]
    }
    return(model)
  }

  addResiduals <- dget(file.path(dirs$addUncertainty,"normDist.R"))

  #Main functions returned for use
  fit <- function(d){
    model <- lm(formula, data=d)
    return(model)
  }

  setup <- function(d){
    d <- makeDummyData(d,varName)
    model <- fit(d)
    model <- replaceCoefficients(model)
    assign("model",model, envir=model.env)
    return(d)
  }

  simulate <- function(d){
    d[,varName] <- predict(model,newdata=d)
    d <- addResiduals(d,varName,uncertainty)
    return(d)
  }

  return(list(fit=fit,
              setup=setup,
              simulate=simulate))
}
