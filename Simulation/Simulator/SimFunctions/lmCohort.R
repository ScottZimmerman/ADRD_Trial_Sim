function(dirs,
         p,
         varName,
         coefficients,
         formula,
         uncertainty){

  lms.env <- environment()
  lms <- list()

  #coefficients structure will have columns
  validCohortNumbers <- unique(coefficients$cohort)

  fit <- function(d){
    #model <- lm(formula, data=d)
    print("Fit not meaningful for lmCohort.R")
    return(NA)
  }

  setup <- function(d){
    lms <- list()
    d_new <- NA
    for(cohortNumber in c(0,1)){
      coefficientsCohort <- coefficients[coefficients$cohort==cohortNumber,]
      lms[[as.character(cohortNumber)]] <- dget(file.path(dirs$simFunctions,"lm.R"))(dirs,
                                                                          p,
                                                                          varName,
                                                                          coefficientsCohort,
                                                                          formula,
                                                                          uncertainty)
      d_cohort <- d[d$cohort==cohortNumber,]
      if(cohortNumber %in% validCohortNumbers){
        d_cohort <- lms[[as.character(cohortNumber)]]$setup(d_cohort)
      }else{
        d_cohort[,varName] <- rep(NA,nrow(d_cohort))
      }
      if(is.na(d_new)){
        d_new <- d_cohort
      }else{
        d_new <- rbind(d_new,d_cohort)
      }
    }
    assign("lms", lms, envir=lms.env)
    d_new <- d_new[order(d_new$ID),]
    return(d_new)
  }

  simulate <- function(d){
    d_new <- NA
    for(cohortNumber in c(0,1)){
      d_cohort <- d[d$cohort==cohortNumber,]

      if(cohortNumber %in% validCohortNumbers){
        d_cohort <- lms[[as.character(cohortNumber)]]$simulate(d_cohort)
      }else{
        d_cohort[,varName] <- rep(NA,nrow(d_cohort))
      }
      if(is.na(d_new)){
        d_new <- d_cohort
      }else{
        d_new <- rbind(d_new,d_cohort)
      }
    }
    d_new <- arrange(d_new,ID)

    return(d_new)
  }
  return(list(fit=fit,
              setup=setup,
              simulate=simulate))
}
