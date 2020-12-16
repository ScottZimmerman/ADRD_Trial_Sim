library(shiny)
library(R.utils)

#reading testDat
dat <- read.csv("../Data/testData.csv")
dirsConstructor <- dget("dirs.R")

server <- function(input, output){

  # builds a reactive expression that creates a list of parameters
  # when button is pressed
  parameters.text <- eventReactive(input$submit.button, {
    HTML(paste(tags$h3("Inputs"),
               # "<b>Sample Size:</b> ", input$pick.n,
               # "<b>Mean:</b> ", input$pick.mean,
               # "<b>SD:</b> ", input$pick.sd,
               # "<b>Effect size:</b> ", input$pick.a,
               tags$h3("Generated data from simulation")
               )
         )
  })

  # makes simulation outputs (after submit button is pressed)
  # using function from test_function R script
  simOut <- eventReactive(input$submit.button, {
    sim.fun <- dget(file.path("Simulation","simulator.wrapper.R"))

    parameters <- list(n = input$pick.n,
                       age = list(
                         "50_54"=input$pick.age.50_54,
                         "55_59"=input$pick.age.55_59,
                         "60_64"=input$pick.age.60_64,
                         "65_69"=input$pick.age.65_69,
                         "70_74"=input$pick.age.70_74
                       ),
                       black = input$pick.race.black,
                       male = input$pick.gender.male,
                       intensity = list(
                         intensive = input$pick.intensity.intensive.arm,
                         standard = input$pick.intensity.standard.arm
                       ))

    #Create last age category
    parameters$age[["75_79"]] <- 1 - sum(unlist(parameters$age))
    parameters$age[["75_79"]] <- max(parameters$age[["75_79"]],0)

    sim.fun(parameters,
            dirsConstructor)
  })

  #Outputs simulated data (simOut()$d) from above as a table
  output$table.out <- DT::renderDataTable(DT::datatable({
    simOut()$d
  }))

  #Make plots:
  output$plot1 <- renderPlot({
    if(input$radio == 1){
      print("chose input$radio == 1")
      trajectory.fun <- dget(file.path("Plotting","trajectory.single.R"))

      parameters <- simOut()$predictions
      trajectory.fun(parameters)
    }else{
      effect.plot <- dget(file.path("Plotting","effect.plot.R"))

      parameters <- simOut()$effects
      effect.plot(parameters)
    }
  })
}
