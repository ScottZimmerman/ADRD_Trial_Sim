library("stringr")
function(dirs,
         p,
         varName,
         coefficients,
         formula,
         uncertainty,
         family){
  #Data
  model.env <- environment()
  diabetic.model <- NA
  nondiabetic.model <- NA

  diabetic.coefficients <- coefficients[startsWith(rownames(coefficients),"diabetic"),]
  rownames(diabetic.coefficients) <- str_replace(rownames(diabetic.coefficients),"diabetic.","")

  nondiabetic.coefficients <- coefficients[startsWith(rownames(coefficients),"nondiabetic"),]
  rownames(nondiabetic.coefficients) <- str_replace(rownames(nondiabetic.coefficients),"nondiabetic.","")

  #Helper functions: used by main functions but not returned
  makeDummyData <- function(d,varName){
    d[,varName] <- runif(nrow(d),min=4,max=12)
    return(d)
  }

  replaceCoefficients <- function(model, coefficients){
    for(coefName in names(model$coefficients)){
      if(!(coefName %in% rownames(coefficients))){
        stop(paste0("Error ",varName," missing coefficient for ",coefName))
      }
      model$coefficients[[coefName]] <- coefficients[coefName,"Estimate"]
    }
    return(model)
  }

  addResiduals <- function(d,
                           varName,
                           uncertainty=NULL, #Uses gammaDispersion from coefficients
                           coefficients
                           ){
    #We sample from a gamma distribution with shape and rate parameters
    #The predicted values are the means, and mean = shape/rate
    #shape = 1/dispersion
    #We then solve for (shape,rate) = (1/dispersion, 1/(dispersion*mean))
    dispersion <- coefficients["gammaDispersion","Estimate"]
    shape <- 1/dispersion
    rate <- 1/(dispersion*exp(d[,varName]))
    d[,varName] <- rgamma(nrow(d), shape=shape, rate=rate)

    return(d)
  }

  createAdditionalVariables <- function(d){
    hba1c.c <- d[,"hba1c"]-6.5
    d[,"hba1c_U65"] <- (hba1c.c<0)*(hba1c.c)
    d[,"hba1c_O65"] <- (hba1c.c>=0)*(hba1c.c)
    return(d)
  }

  #Main functions returned for use
  fit <- function(d){
    model <- glm(formula,
                          family = Gamma(link="log"),
                          data=d)
    return(model)
  }

  setup <- function(d){
    d <- makeDummyData(d,varName)
    d <- createAdditionalVariables(d)
    model <- fit(d)
    diabetic.model <- model
    nondiabetic.model <- model

    diabetic.model <- replaceCoefficients(model,
                                          diabetic.coefficients)
    nondiabetic.model <- replaceCoefficients(model,
                                             nondiabetic.coefficients)

    assign("diabetic.model",
           diabetic.model,
           model.env)
    assign("nondiabetic.model",
           nondiabetic.model,
           model.env)
    return(d)
  }

  simulate_subdist <- function(model,d,coefficients){
    d[,varName] <- predict(model,
                           newdata=d,
                           dispersion=coefficients["gammaDispersion","Estimate"])
    d <- addResiduals(d,varName,uncertainty, coefficients)
    return(d)
  }

  simulate <- function(d){
    #Predict separately for diabetics and nondiabetics
    d_diab_counterfactual <- simulate_subdist(diabetic.model,
                                              d,
                                              diabetic.coefficients)
    d_nondiab_counterfactual <-simulate_subdist(nondiabetic.model,
                                                d,
                                                nondiabetic.coefficients)

    #Combine accordingly
    d$hba1c <- (d$diabetes==1)*d_diab_counterfactual$hba1c + (d$diabetes==0)*d_nondiab_counterfactual$hba1c
    d <- createAdditionalVariables(d)

    return(d)
  }

  return(list(fit=fit,
              setup=setup,
              simulate=simulate))
}
