library(DT)
library(shiny)

navbarPage("Booz | Allen | Hamilton",
 tabPanel("Allocation Model",
          fluidPage(
            fluidRow(
              column(1, 
                     selectInput(inputId = "allocationFlag", label = "Allocation:",choices = NULL)
              ),
              column(1,
                     selectInput(inputId = "Function", label = "Function:", choices = NULL)
              ),
              column(2,
                     selectInput(inputId = "lcat", label = "Labor Category:", choices = NULL)
              ),
              column(2,
                     selectInput(inputId = "filledStatus", label = "Position Status:", choices = c('All','Filled','Unfilled'))
              ),
              column(1,
                     selectInput(inputId = "team", label = "Team:", choices = c('All','Bravo','Delta'))
              ),
              column(2,
                     selectizeInput(
                       'tasks', 'Tasks:', choices = NULL, multiple = TRUE
                     )
              ),
              column(2,
                     selectInput(inputId = "futureAvailability", label = "Future Availability:", choices = NULL)
              )
            ),
            
            fluidRow(
              dataTableOutput('flaggedTable')
            ),
            fluidRow(
              column(1),
              column(10, dataTableOutput('filteredTable')),
              column(1)
            )
          )

 ),
 tabPanel("Aggregate View"
 ),
 tabPanel("Hire Forecast"
 ),
 tabPanel("Scenario Planning"
 )
)