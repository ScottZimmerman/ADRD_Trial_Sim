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
  makeDummyData <- dget(file.path(dirs$makeDummyData,"binomialDist.R"))

  replaceCoefficients <- function(model){
    for(coefName in rownames(coefficients)){
      model$coefficients[[coefName]] <- coefficients[coefName,"Estimate"]
    }
    return(model)
  }

  #Main functions returned for use
  fit <- function(d){
    model <- glm(formula,
                 family = binomial(link="logit"),
                 data=d)
    return(model)
  }

  setup <- function(d){
    d <- makeDummyData(d,varName)
    model <- fit(d)
    model <- replaceCoefficients(model)
    assign("model", model, envir=model.env)
    return(d)
  }

  simulate <- function(d){
    #Predict outcome probs
    outcomeProb <- predict(model,newdata=d,type="response")

    #Assign outcome randomly with these probs of success
    U <- runif(nrow(d))
    d[,varName] <- as.numeric(U<outcomeProb)

    return(d)
  }

  return(list(fit=fit,
              setup=setup,
              simulate=simulate))
}
