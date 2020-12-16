function(dirs,
         p,
         varName,
         coefficients,
         formula,
         uncertainty){
  #Data
  model.env <- environment()
  model <- NA

  possibleValues <- list(
    arm=c("standard","intensive")
  )

  #Helper functions used by main functions
  doSampling <- function(d){
    #Make demographic variables weights table
    arms <- expand.grid(possibleValues, stringsAsFactors = FALSE)

    d_weights <- left_join(arms, coefficients, by = c("arm"))
    d_weights[is.na(d_weights$prop),"prop"] <- 0
    weights <- d_weights$prop

    #Sample arms
    d_arms <- sample_n(arms,p$n,replace=TRUE,weight=weights)

    #Make additional variables used in formulae
    ID <- 1:nrow(d_arms)
    d_arms <- cbind(ID,d_arms)
    d <- left_join(d,d_arms,by="ID")
    return(d)
  }

  #Main functions returned for use
  fit <- function(d){
    print("fit not defined for assignTxArm")
    return(NULL)
  }

  setup <- doSampling
  simulate <- doSampling

  return(list(fit=fit,
              setup=setup,
              simulate=simulate))
}