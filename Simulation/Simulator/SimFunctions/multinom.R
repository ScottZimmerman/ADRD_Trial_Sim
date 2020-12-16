library("nnet")
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
  makeDummyData <- dget(file.path(dirs$makeDummyData,"simpleSample.R"))

  replaceCoefficients <- function(model){
    for(coefName in names(model$coefficients)){
      model$coefficients[[coefName]] <- coefficients[coefName,"Estimate"]
    }

    #Multinom source code indicates where coefs are stored: https://rdrr.io/cran/nnet/src/R/multinom.R
    r <- length(model$vcoefnames)

    #This is what's used to make coef mx when you call coef(model): coef <- matrix(model$wts, nrow = model$n[3L], byrow=TRUE)[, 1L+(1L:r), drop=FALSE]
    # So the cofs are the final entries of the vector model$wts
    # Assign coefs by converting into the matrix presented by coef(model), changing the values, and converting back to the vector form
    temp <- matrix(model$wts, nrow = model$n[3L], byrow=TRUE)[, 1L+(1L:r), drop=FALSE]
    rownames(temp) <- model$lab
    colnames(temp) <- model$coefnames

    for(rowIndex in 1:nrow(coefficients)){
      varCategory <- coefficients[rowIndex,varName]
      if(varCategory %in% p$categoryValues[[varName]]){
        coefName <- coefficients[rowIndex,"X"]
        temp[varCategory,coefName] <- coefficients[rowIndex,"Estimate"]
      }else{
        print("Possible error: multinom.R")
      }
    }
    model$wts <- as.vector(t(temp))
    return(model)
  }

  addUncertainty <- dget(file.path(dirs$addUncertainty,"categoricalAccuracy.R"))

  #Main functions returned for use
  fit <- function(d){
    model <- nnet::multinom(formula, data=d)
    return(model)
  }

  setup <- function(d){
    d <- makeDummyData(d,
                       varName,
                       p$categoryValues[[varName]])
    model <- fit(d[complete.cases(d),])
    model <- replaceCoefficients(model)
    assign("model",model, envir=model.env)
    return(d)
  }

  simulate <- function(d){
    d[,varName] <- predict(model,newdata=d)
    d <- addUncertainty(d,
                        varName,
                        uncertainty) #uncertainty is a joint distributiton of true vs predicted
    return(d)
  }

  get <- function(){
    return(model)
  }

  return(list(get=get,
              fit=fit,
              setup=setup,
              simulate=simulate))
}
