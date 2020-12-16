library("lme4")
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
  makeLongitudinal <- function(d,varName){
    d2 <- d
    d2[,"age60"] <- d[,"age60"]+runif(nrow(d),min=0.45,max=0.55) #add 4.5-5.5 years
    d3 <- d2
    d3[,"age60"] <- d2[,"age60"]+runif(nrow(d2),min=0.45,max=0.55) #add 4.5-5.5 years
    d$obsID <- 1
    d2$obsID <- 2
    d3$obsID <- 3
    d <- rbind(d,d2,d3)
    return(d)
  }

  makeDummyData <- function(d,varName){
    #Creates longitudinal data
    d[,varName] <- rnorm(nrow(d),sd=0.5) #random effect
    d <- makeLongitudinal(d,varName)
    d[,varName] <- d[,varName]+rnorm(nrow(d))
    return(d)
  }

  replaceCoefficients <- function(model){
    betaNames <- colnames(coef(model)$ID)
    betas <- rep(NA,length(betaNames))

    #Assign fixed effects
    for(betaIndex in 1:length(betas)){
      coefName <- betaNames[betaIndex]
      betas[betaIndex] <- coefficients[coefName,"Estimate"]
      if(is.na(coefficients[coefName,"Estimate"]) | is.null(coefficients[coefName,"Estimate"])){
        stop(paste0("Error: ",varName," model missing coefficient input for ",coefName))
      }
    }
    slot(model,"beta") <- betas

    #Theta is a vector of parameters used by lmer to predict
    #This section is a conversion in order to get theta from a var-corr mx
    #Assign random effects: mkVarCorr function from https://github.com/lme4/lme4/blob/master/R/lmer.R explains the link between theta and var-corr mx
    IntSD <- coefficients["RE_ID_Intercept","SE"]
    AgeSD <- coefficients["RE_ID_age60","SE"]
    IntAgeCor <- coefficients["Cor_RE_ID","Estimate"]
    IntAgeCov <- IntAgeCor/(IntSD*AgeSD)
    V <- matrix(c(IntSD^2, IntAgeCov, IntAgeCov, AgeSD^2),nrow=2)

    ResidualSE <- coefficients["Residual","SE"]
    VdivSig <- V*(1/ResidualSE^2)
    VdivSig <- nearPD(VdivSig)
    Mtheta <- t(chol(VdivSig$mat))
    Vtheta <- Mtheta[lower.tri(Mtheta,diag=TRUE)]
    slot(model,"theta") <- Vtheta
    return(model)
  }

  addResiduals <- dget(file.path(dirs$addUncertainty,"normDist.R"))

  #Main functions returned for use
  fit <- function(d){
    model <- lmer(as.formula(formula),
                  data = d,
                  na.action = na.omit)
    return(model)
  }

  setup <- function(d){
    d <- makeDummyData(d,varName)
    d[is.na(d$cses),"cses"] <- 0
    d[is.na(d$diet_p),"diet_p"] <- 0
    d[is.na(d$diet_w),"diet_w"] <- 0

    model <- fit(d)
    model <- replaceCoefficients(model)
    assign("model",model, envir=model.env)
    return(d)
  }

  simulate <- function(d){
    d <- makeLongitudinal(d,varName)

    d[,"cses_temp"] <- d[,"cses"]
    d[,"diet_p_temp"] <- d[,"diet_p"]
    d[,"diet_w_temp"] <- d[,"diet_w"]

    d[is.na(d$cses),"cses"] <- 0
    d[is.na(d$diet_p),"diet_p"] <- 0
    d[is.na(d$diet_w),"diet_w"] <- 0
    d[,varName] <- predict(model,newdata=d)
    d <- addResiduals(d,varName,uncertainty)

    d[,"cses"] <- d[,"cses_temp"]
    d[,"diet_p"] <- d[,"diet_p_temp"]
    d[,"diet_w"] <- d[,"diet_w_temp"]

    d[,"cses_temp"] <- NULL
    d[,"diet_p_temp"] <- NULL
    d[,"diet_w_temp"] <- NULL

    return(d)
  }

  getModel <- function(){
    return(model)
  }

  return(list(fit=fit,
              setup=setup,
              simulate=simulate,
              getModel=getModel))
}
