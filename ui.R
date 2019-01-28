library(DT)
library(shiny)
library(shinyBS)

navbarPage("Booz | Allen | Hamilton",
 tabPanel("Allocation Model",
          fluidPage(
            fluidRow(
              column(width = 2,offset = 10, 
                     h4('Data as of: 01/25/2019', align = 'right')
              )
            ),
            fluidRow(
              column(1,
                     selectInput(inputId = "team", label = "Task Order:", choices = c('All','Bravo','Delta'))
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
              ),
              column(2,
                     actionButton("openModal", "Expand Selected Cell"),
                     bsModal("modalExample", "Individual Breakdown", "openModal", size = "large",
                             dataTableOutput('filteredTable'))
              ),
              column(2,
                     actionButton("openTaskModal", "Task/RFS Breakdown"),
                     bsModal("taskModal", "Task/RFS Information", "openTaskModal", size = "large",
                             dataTableOutput('taskTable'))
              )
            ),
            fluidRow(
              br(),
              dataTableOutput('allocationTable')
            )
          )

 ),
 tabPanel("Forecasting View"
 ),
 tabPanel("Hire Forecast"
 ),
 tabPanel("Scenario Planning"
 )
)