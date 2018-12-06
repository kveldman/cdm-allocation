library(DT)
library(shiny)
library(shinyBS)

navbarPage("Booz | Allen | Hamilton",
 tabPanel("Allocation Model",
          fluidPage(
            fluidRow(
              column(1,
                     selectInput(inputId = "team", label = "Team:", choices = c('All','Bravo','Delta'))
              ),
              column(1,
                     selectizeInput(
                       'tasks', 'Tasks:', choices = NULL, multiple = TRUE
                     )
              ),
              column(2,
                     selectInput(inputId = "Function", label = "Functional Role:", choices = NULL)
              ),
              column(2,
                     selectInput(inputId = "lcat", label = "Labor Category:", choices = NULL)
              ),
              column(1,
                     selectInput(inputId = "filledStatus", label = "Position Status:", choices = c('All','Filled','Unfilled'))
              )
              ,
              column(1,
                     selectInput(inputId = "monthOfInterest", label = "Month:", choices = NULL)
              ),
              column(2, 
                     selectInput(inputId = "allocationFlag", label = "Allocation:",choices = NULL)
              )
            ),
            fluidRow(
              column(1,
                     actionButton("resetAll", "Reset")
              )
            ),
            fluidRow(
              dataTableOutput('summaryTable')
            ),
            # fluidRow(
            #   downloadButton("exportData", "Export")
            # ),
            fluidRow(
              column(1),
              column(10, dataTableOutput('filteredTable')),
              column(1)
            )
            # fluidRow(
            #   column(1,
            #          actionButton("modalTest", "Modal Test"),
            #          bsModal("modalExample", "Data Table", "modalTest", size = "large",
            #                  dataTableOutput('filteredTable'))
            #   )
            # )
          )

 ),
 tabPanel("Aggregate View"
 ),
 tabPanel("Hire Forecast"
 ),
 tabPanel("Scenario Planning"
 )
)