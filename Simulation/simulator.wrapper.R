
# Takes in a list of parameters:
#   n, age (list), black, male, intensity (list of treatment intensities by arm)
#
# then creates scenario and batch instruction files
# then runs scenario(s)
# finally returns simulated data frame

function(info,
         dirsConstructor){

  library("R.utils")
  scenarioName <- "temp" #Eventually this needs to make a unique name
  dirs <- dirsConstructor(scenarioName)

  #---------------------------------------
  #Create inputs for this scenario
  #---------------------------------------
  createScenarioInputs <- function(scenarioName){
    #Set up new scenario
    dir.create(dirs$scenarioOut)
    dir.create(dirs$scenarioIn)

    copyDirectory(dirs$scenarioTemplate,
              dirs$scenarioIn,
              recursive=TRUE)

    #Sample size: Modify parameters.R file
    paramsPath <- file.path(dirs$scenarioIn,"parameters.R")
    params <- dget(paramsPath)
    params$n <- info$n
    dput(params,file=paramsPath)

    #Composition:Modify Demographics input csv
    demog <- expand.grid(agecat=paste0("Agecat",0:5),
                sex=c("Male","Female"),
                race=c("Black","White"),
                prop=1)

    demog[demog$sex=="Male","prop"]   <- demog[demog$sex=="Male","prop"]*info$male
    demog[demog$sex=="Female","prop"] <- demog[demog$sex=="Female","prop"]*(1-info$male)

    demog[demog$race=="Black","prop"] <- demog[demog$race=="Black","prop"]*info$black
    demog[demog$race=="White","prop"] <- demog[demog$race=="White","prop"]*(1-info$black)

    demog[demog$agecat=="Agecat0","prop"] <- demog[demog$agecat=="Agecat0","prop"]*info$age[["50_54"]]
    demog[demog$agecat=="Agecat1","prop"] <- demog[demog$agecat=="Agecat1","prop"]*info$age[["55_59"]]
    demog[demog$agecat=="Agecat2","prop"] <- demog[demog$agecat=="Agecat2","prop"]*info$age[["60_64"]]
    demog[demog$agecat=="Agecat3","prop"] <- demog[demog$agecat=="Agecat3","prop"]*info$age[["65_69"]]
    demog[demog$agecat=="Agecat4","prop"] <- demog[demog$agecat=="Agecat4","prop"]*info$age[["70_74"]]
    demog[demog$agecat=="Agecat5","prop"] <- demog[demog$agecat=="Agecat5","prop"]*info$age[["75_79"]]

    write.csv(demog,
              file=file.path(dirs$scenarioIn,"Coefficients","demographics.csv"),
              row.names=FALSE)

    #hba1c input csv
    hba1cCoefficients <- data.frame(arm=c("intensive","standard"),
                                    target=c(info$intensity$intensive,
                                             info$intensity$standard))
    write.csv(hba1cCoefficients,
              file=file.path(dirs$scenarioIn,"Coefficients","hba1c.csv"),
              row.names=FALSE)

    #Formulae: change type of formula to "setValue" for hba1c
    formulaePath <- file.path(dirs$scenarioIn,"formulae.R")
    formulae <- dget(formulaePath)
    formulae$hba1c <- "setValue"
    dput(formulae,file=formulaePath)
    print("Done setting up scenario")
  }
  createScenarioInputs(scenarioName)


  #Run simulation
  print("Loading simulator")
  baseDir <- dirs$simulator
  simulator <- dget(file.path(baseDir,"main.R"))
  print("Simulator loaded")
  instructions <- list(
    batchName=scenarioName,
    scenarioNames = c(scenarioName)
  )
  simOutputs <- simulator(baseDir,instructions)
  scenariosData  <- simOutputs$scenariosData
  print("simulator.wrapper.R finished")
  df <- scenariosData[[scenarioName]]
  model <- simOutputs$memoryModels[[scenarioName]]
  df$mem.s_pred <- predict(model)
  #Make z score
  df$mem.s_pred <- df$mem.s_pred/sd(df[df$obsID==1,"mem.s"])
  df_intensive <- df[df$arm=="intensive",]
  df_standard <- df[df$arm=="standard",]
  df_intensive1 <- df_intensive[df_intensive$obsID==1,]
  df_intensive2 <- df_intensive[df_intensive$obsID==2,]
  df_standard1 <- df_standard[df_standard$obsID==1,]
  df_standard2 <- df_standard[df_standard$obsID==2,]

  slopeIntensiveArm <- mean(df_intensive2$mem.s_pred - df_intensive1$mem.s_pred)
  slopeStandardArm <- mean(df_standard2$mem.s_pred - df_standard1$mem.s_pred)
  sdStandardArm <- sd(df_standard2$mem.s_pred - df_standard1$mem.s_pred)
  sdIntensiveArm <- sd(df_intensive2$mem.s_pred - df_intensive1$mem.s_pred)
  print(list(slopeStandardArm=slopeStandardArm,
             slopeIntensiveArm=slopeIntensiveArm,
             sdStandardArm=sdStandardArm,
             sdIntensiveArm=sdIntensiveArm))
  #-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  #PREDICTED TRAJECTORIES
  #TEMPORARY: REPLACE WITH ACTUAL CALCULATION
  interval <- 40
  nTimePoints <- 4 # Zero included
  nArms <- 2
  # slopeStandardArm <- -0.1
  # slopeIntensiveArm <- -0.12
  # sdStandardArm <- 0.19
  # sdIntensiveArm <- 0.19
  timePointIntegers  <- (0:(nTimePoints-1))
  df_pred <- data.frame("Study Arm"=c(rep("Standard",nTimePoints),
                                      rep("Intensive",nTimePoints)),
                        months=rep(
                          timePointIntegers*interval,
                          nArms
                        ),
                        mean_pred_mem=c(
                          timePointIntegers*slopeStandardArm,
                          timePointIntegers*slopeIntensiveArm
                        ),
                        sdev = c(rep(sdStandardArm,nTimePoints),
                                 rep(sdIntensiveArm,nTimePoints)))
  df_pred$mem_change <- df_pred$mean_pred_mem
  #-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

  #-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  #EFFECT SIZE
  #TEMPORARY: REPLACE WITH ACTUAL CALCULATION
  months <- timePointIntegers*interval
  #Use above slopes and sds, t tests
  effect_size <- df_pred[df_pred[,'Study.Arm']=="Intensive","mean_pred_mem"]-
    df_pred[df_pred[,'Study.Arm']=="Standard","mean_pred_mem"]

  #Sum the variances to get the variances of the difference of 2 indep random variables
  sds <- sqrt((df_pred[df_pred[,'Study.Arm']=="Intensive","sdev"])^2+
          (df_pred[df_pred[,'Study.Arm']=="Standard","sdev"])^2)
  ci_low <- effect_size - 1.96*sds
  ci_high <- effect_size + 1.96*sds

  effects=list("months" = months,
           "effect_size" = effect_size,
           "ci_low" = ci_low,
           "ci_high" = ci_high)
  #-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

  #Return output
  simOutput <- list(d=df,
                    predictions=list(df_pred=df_pred),
                    effects=effects)
  return (simOutput)
}