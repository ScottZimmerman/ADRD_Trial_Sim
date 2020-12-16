function(baseDir,
         instructions){
  #========================================================
  # Version of simulation code for back-end of UI
  #
  # Overview: This code runs a set of scenarios with specified data-generating
  #   functions and parameters, and outputs summary plots and the generated data set.
  #
  # Example inputs:
  # instructions <- list(
  #   batchName="temp",
  #   scenarioNames = c("temp")
  # )
  # baseDir <- file.path("Simulation","Simulator")
  #
  # Each scenario defines a set of data-generating instructions and has the following inputs:
  #   -parameters.R: Overall simulation parameters such as sample size and variable simulation order
  #   -categoryValues.R: Category names for each categorical variable
  #   -modelTypes.R: For each variable, the name of the file in the SimFunctions
  #                   directory that handles simulation of the variable
  #   -formulae.R: Functions used by model, to be handled by the appropriate
  #                         function in SimFunctions as specified in modelTypes.R
  #                         Joint distributions handled with "JD"
  #   -Coefficients folder: csv files for each, to be handled by the appropriate
  #                         function in SimFunctions as specified in modelTypes.R.
  #                         Joint distributions contain columns for each variable and a
  #                         "prop" column that sums to 1.
  #   -Uncertainty folder:
  #           -residuals.csv: residuals standard deviation of continuous variables
  #           -accuracy.csv: square matrix of accuracy for all categorical variables
  #                         (with NAs if categories do not belong to the same variable)
  #                         handled by SimFunctions/AddUncertainty/categoricalAccuracy.R
  #                         Rows are predicted, columns are truth, cells are P(truth|predicted)
  #
  # Scenario outputs consist of data sets data.csv in subfolders of Scenarios
  #
  # The scenarios to run and the batch name are specified in "instructions" below.
  #
  # "Batches" are sets of scenarios that are run together A subfolder of the Batches directory
  #     will be created with the name instructions$batchName, and all plots, tables, and other
  #     outputs for comparing the scenarios. The output instructions.RDS also includes a record
  #     of the simulation instructions and copies of the simulation inputs
  #
  #
  # Written by Scott Zimmerman (scott.zimmerman@ucsf.edu)
  #========================================================

  #========================================================
  #Libraries
  #========================================================
  library(tidyr)
  library(nnet)
  library(dplyr)
  library(Matrix)
  library(ggplot2)
  library(gridExtra)
  library(ggpubr)
  library(lme4)

  #========================================================
  # Directories
  #========================================================
  dirs <- list(
    input = file.path(baseDir,"Input"),
    output = file.path(baseDir,"Output"),
    validation = file.path(baseDir,"Validation"),
    simFunctions = file.path(baseDir,"SimFunctions")
  )

  dirs$inputData <- file.path(dirs$input,"Data")
  dirs$scenarios <- file.path(dirs$input,"Scenarios")
  dirs$batchesOut <- file.path(dirs$output,"Batches")
  dirs$scenariosOut <- file.path(dirs$output,"Scenarios")
  dirs$addUncertainty <- file.path(dirs$simFunctions,"AddUncertainty")
  dirs$makeDummyData <- file.path(dirs$simFunctions,"MakeDummyData")

  #========================================================
  # Batch instructions
  #========================================================
  dirs$batchOut <- file.path(dirs$batchesOut,instructions$batchName)

  #========================================================
  # Output directories setup
  #========================================================
  dir.create(dirs$batchOut, showWarnings = FALSE)
  for(scenarioName in instructions$scenarioNames){
    dir.create(file.path(dirs$scenariosOut,scenarioName), showWarnings = FALSE)
  }

  #========================================================
  # Load scenario input data from batch instructions
  #========================================================
  scenarios <- list()
  for(scenarioName in instructions$scenarioNames){
    scenarioDir <- file.path(dirs$scenarios,scenarioName)
    scenario <- list(
      parameters = dget(file.path(scenarioDir,"parameters.R")),
      formulae = dget(file.path(scenarioDir,"formulae.R")),
      modelTypes = dget(file.path(scenarioDir,"modelTypes.R")),
      coefficients = list(),
      uncertainty = list()
    )

    #Category names for categorical variables
    categoryValues <- dget(file.path(scenarioDir,"categoryValues.R"))
    scenario$parameters$categoryValues <- categoryValues

    #Each variable either contains an R formula or "JD" and has a corresponding coefficients csv
    for(varName in names(scenario$formulae)){
      coefsCSVfilename <- paste0(varName, ".csv")
      coefsCSVpath <- file.path(scenarioDir,"Coefficients",coefsCSVfilename)
      print(paste0("Loading ",coefsCSVpath))
      if(varName %in% names(categoryValues) ||
         scenario$formula[[varName]] %in% c("JD","setValue")){
        #JDs and categorical variables don't have row names
        scenario$coefficients[[varName]] = read.csv(coefsCSVpath,stringsAsFactors = FALSE)
      }else{
        scenario$coefficients[[varName]] = read.csv(coefsCSVpath,stringsAsFactors = FALSE,row.names="X")
      }
    }

    #Uncertainty is two files: residuals and prediction accuracy,
    residualsCSVpath <- file.path(scenarioDir,"Uncertainty","residuals.csv")
    accuracyCSVpath <- file.path(scenarioDir,"Uncertainty","accuracy.csv")

    residuals <- read.csv(residualsCSVpath, row.names='X')
    accuracy <- read.csv(accuracyCSVpath, row.names='X')

    #Both uncertainty files need preprocessing
    ##Residuals is just a single value (the residual's SD) for each formula
    scenario$uncertainty <- residuals$Residual
    names(scenario$uncertainty) <- rownames(residuals)
    scenario$uncertainty <- as.list(scenario$uncertainty)

    #We pull out the submatrices for each variable in the accuracy matrix.
    for(varName in names(categoryValues)){
      varCategories <- categoryValues[[varName]]
      scenario$uncertainty[[varName]] <- accuracy[varCategories,varCategories]
    }

    #Load model functions
    scenario$models <- list()
    for(varName in scenario$parameters$varOrder){
      modelType <- scenario$modelTypes[varName]
      modelFilename <- paste0(modelType,".R")
      modelFilePath <- file.path(dirs$simFunctions,modelFilename)
      cat(paste0("Loading ",modelFilePath,"\n"))
      scenario$models[[varName]] <- dget(modelFilePath)(dirs,
                                                        scenario$parameters,
                                                        varName,
                                                        scenario$coefficients[[varName]],
                                                        scenario$formula[[varName]],
                                                        scenario$uncertainty[[varName]])
    }

    #Set data
    scenarios[[scenarioName]] <- scenario
  }
  instructions$scenarios <- scenarios

  #Save batch instructions
  saveRDS(instructions,file.path(dirs$batchOut,"instructions.RDS"))

  #========================================================
  # Model function set-up
  #========================================================
  for(scenarioName in names(instructions$scenarios)){
    cat(paste0("\nSetting up scenario: ",scenarioName,"\n\n"))
    d_setup <- NA
    scenario <- instructions$scenarios[[scenarioName]]
    p <- scenario$parameters
    for(varName in p$varOrder){
      cat(paste0("Setting up model for ",varName,"\n"))

      #Each model function has a setup function that assigns the
      # coefficients we specified in the instructions
      # to the models of the specified functional form
      d_setup <- scenario$models[[varName]]$setup(d_setup)
    }
    d_setup <- NULL
  }

  #========================================================
  # Main loop: Simulate data for each scenario
  #========================================================
  scenariosData <- list()
  memoryModels  <- list()
  for(scenarioName in names(instructions$scenarios)){
    cat(paste0("\nRunning simulation scenario: ",scenarioName,"\n\n"))
    scenario <- instructions$scenarios[[scenarioName]]
    p <- scenario$parameters

    #Set up placeholder for data (will be overwritten by first function)
    d <- NA

    #Simulate variables in order
    for(varName in p$varOrder){
      cat(paste0(" Simulating variable: ",varName,"\n"))
      d <- scenario$models[[varName]]$simulate(d)
    }

    #Keep data in memory for batch plotting
    scenariosData[[scenarioName]] <- d
    memoryModels[[scenarioName]] <- scenario$models$mem.s$getModel()

    #Save output
    write.csv(d, file.path(dirs$scenariosOut,scenarioName,"data.csv"),
              row.names=FALSE)
  }

  return(list(scenariosData=scenariosData,
              memoryModels=memoryModels))
}