function(scenarioName){
  dirs <- list(
    simulator = file.path("Simulation","Simulator")
  )
  dirs$templates <- file.path(dirs$simulator,"Templates")
  dirs$scenarioTemplate <- file.path(dirs$templates,"Scenario")
  dirs$simulationIn <- file.path(dirs$simulator,"Input")
  dirs$simulationOut <- file.path(dirs$simulator,"Output")
  dirs$scenarioIn <- file.path(dirs$simulationIn,"Scenarios",scenarioName)
  dirs$scenarioOut <- file.path(dirs$simulationOut,"Scenarios",scenarioName)

  return(dirs)
}