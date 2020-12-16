library(shiny)
library(DT)
library(shinythemes)
library(ggplot2)
dget("server.R")

ui <- fluidPage(theme = shinytheme("lumen"),
  navbarPage("A Simulator to Inform Alzheimer’s Disease and Dementia Trial Design (beta)",
     #creates tab 1
     tabPanel("Welcome",
      div(style="display: inline-block;vertical-align:top; width: 800px;",
          p("Welcome to the simulator! To design better RCTs for Alzheimer’s disease and dementia (ADRD), it is important to systematically use observational evidence. Simulation studies provide a mechanism to evaluate proposed trials. We present a simulator to improve ADRD trial design by simulating trials with varying trial characteristics, based on available observational evidence.
"),
          p("This simulator is here to help."),
          p("This is a work in progress for presentation at the Society for Epidemiologic Research virtual conference 2020. We're still working on developing the best possible simulation model, and presenting useful information to help guide trial design. We welcome your feedback: Please send us a message through the github repo (github.com/ScottZimmerman/ADRD_Trial_Sim))!"),
          p("Funding for this research was provided by The United States’ National Institute on Aging grant 5R01AG057869-03, and is the work of Scott C. Zimmerman (1), Kaitlin Swinnerton (1),  Camilla Calmasini (1), Melinda C. Power (2), Teresa Filshtein (3), Sarah F. Ackley (1), Megha L. Mehrotra (1), Jingkai Wei (2),  Kan Z Gianattasio (2), Audra L. Gold (1), and M. Maria Glymour (1). 1: UCSF Dept of Epidemiology and Biostatistics, 2: George Washington University Dept of Epidemiology, 3: 23andMe
"),
          p("To begin, click on the \"Simulate data\" tab above."))
     ),
     tabPanel("Simulate data",
          #makes the sidebar input widgets
          sidebarPanel(
            h4("Sample Composition"),
            helpText("The baseline distribution of variables can affect cognitive outcomes. Select demographic characteristics:"),
            h5("Age"),
            div(style="display: inline-block;vertical-align:top; width: 200px;",
                numericInput("pick.age.50_54",
                             label = "Proportion in age group 50-54", min = 0, max = 1, value = 1/6),
                div(style="display: inline-block;vertical-align:top; width: 30px;", HTML("<br>")),
                numericInput("pick.age.55_59",
                             label = "Proportion in age group 55-59", min = 0, max = 1, value = 1/6),
                div(style="display: inline-block;vertical-align:top; width: 30px;", HTML("<br>")),
                numericInput("pick.age.60_64",
                             label = "Proportion in age group 60-64", min = 0, max = 1, value = 1/6),
                div(style="display: inline-block;vertical-align:top; width: 30px;", HTML("<br>")),
                numericInput("pick.age.65_69",
                             label = "Proportion in age group 65-69", min = 0, max = 1, value = 1/6),
                numericInput("pick.age.70_74",
                             label = "Proportion in age group 70-74", min = 0, max = 1, value = 1/6)
                #PUT SOMETHING HERE SAYING THAT A PROPORTION OF X WILL BE IN THE FINAL 75-79 AGE GROUP
                ),
            HTML('<br/>'),

            h5("Gender"),
            div(style="display: inline-block;vertical-align:top; width: 200px;",
                numericInput("pick.gender.male",
                             label = "Proportion Male", min = 0, max = 1, value = 0.385)),
            HTML('<br/>'),

            h5("Race"),
            div(style="display: inline-block;vertical-align:top; width: 200px;",
                numericInput("pick.race.black",
                             label = "Proportion Black", min = 0, max = 1, value = 0.645)),

            HTML('<br/>'),
            HTML('<br/>'),
            h4("Sample Size"),
            helpText("Select a sample size. Participants will be randomly assigned to treatment arms with probability 0.5."),
            numericInput("pick.n",
                         label = "Input sample size",
                         min = 10,
                         value = 10251),

            HTML('<br/>'),
            h4("Intervention Intensity"),
            helpText("What is the HbA1c level achieved in each arm of the study?"),
            div(style="display: inline-block;vertical-align:top; width: 200px;",
                sliderInput("pick.intensity.intensive.arm",
                            label = "Intensive Arm", min=4.5, max=9, step = 0.1, value = 6.5)),
            div(style="display: inline-block;vertical-align:top; width: 25px;", HTML("<br>")),
            div(style="display: inline-block;vertical-align:top; width: 200px;",
                sliderInput("pick.intensity.standard.arm",
                            label = "Standard Arm", min=4.5, max=9, step = 0.1, value = 7.5)),
            actionButton("submit.button",
                         label = "Submit",
                         class="btn btn-primary",
                         icon = icon("play"),
                         width = '100%')
          ),

          #makes main panel with the text output and the dataframe output
          mainPanel(htmlOutput("inputs.text"),
                    br(),
                    dataTableOutput("table.out"))
     ),

     #creates tab 2
     tabPanel("Plot simulation results",
              sidebarPanel(
                helpText("Plot results of simulation"),
                radioButtons("radio", label = "Plot options",
                             choices = list("Trajectories" = 1,
                                            "Estimated effects" = 2))),
              mainPanel(plotOutput("plot1"))
     )
  )
)
ui
