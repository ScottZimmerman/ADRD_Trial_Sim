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
    cohort=c(0,1)
  )

  #Helper functions used by main functions
  doSampling <- function(d){
    #Make demographic variables weights table
    cohort <- expand.grid(possibleValues, stringsAsFactors = TRUE)

    d_weights <- left_join(cohort, coefficients, by = c("cohort"))
    d_weights[is.na(d_weights$prop),"prop"] <- 0
    weights <- d_weights$prop

    #Sample cohorts data
    d_cohort <- sample_n(cohort,p$n,replace=TRUE,weight=weights)

    #Make additional variables used in formulae
    ID <- 1:nrow(d_cohort)
    d_cohort <- cbind(ID,d_cohort)
    d <- left_join(d,d_cohort,by="ID")
    return(d)
  }

  #Main functions returned for use
  fit <- function(d){
    print("fit not defined for simCohort")
    return(NULL)
  }

  setup <- doSampling
  simulate <- doSampling

  return(list(fit=fit,
              setup=setup,
              simulate=simulate))
}