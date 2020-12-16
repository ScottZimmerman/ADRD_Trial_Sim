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
    agecat = paste0("Agecat",0:6),
    sex = c("Female","Male"),
    race = c("White","Black")
  )

  #Helper functions used by main functions
  doSampling <- function(d){
    #Make demographic variables weights table
    demog <- expand.grid(possibleValues, stringsAsFactors = TRUE)
    for(colName in colnames(demog)){
      demog[,colName] <- as.character(demog[,colName])
    }

    d_weights <- left_join(demog,coefficients, by = c("agecat", "sex", "race"))
    d_weights[is.na(d_weights$prop),"prop"] <- 0
    weights <- d_weights$prop

    #Sample demographic data
    d <- sample_n(demog,p$n,replace=TRUE,weight=weights)

    #Make additional variables used in formulae
    d <- d %>% mutate(
      age60 =
        case_when(
          agecat == "Agecat0" ~ 50,
          agecat == "Agecat1" ~ 55,
          agecat == "Agecat2" ~ 60,
          agecat == "Agecat3" ~ 65,
          agecat == "Agecat4" ~ 70,
          agecat == "Agecat5" ~ 75,
          agecat == "Agecat6" ~ 80
        )
    )
    d$age60 <- (d$age60+runif(nrow(d),0,5) - 60)/10
    d$base.age60 <- d$age60
    d$sexMale <- as.numeric(d$sex == "Male")
    d$raceBlack <- as.numeric(d$race=="Black")
    ID <- 1:nrow(d)
    d <- cbind(ID,d)
    return(d)
  }

  #Main functions returned for use
  fit <- function(d){
    print("fit not defined for simDemog")
    #Could create a frequency table based on d
    return(NULL)
  }

  setup <- doSampling
  simulate <- doSampling

  return(list(fit=fit,
              setup=setup,
              simulate=simulate))
}