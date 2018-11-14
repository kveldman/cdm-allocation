library(DT)
library(shiny)

navbarPage("Booz | Allen | Hamilton",
 tabPanel("Allocation Model",
          fluidPage(
            fluidRow(
              column(1, 
                     selectInput(inputId = "allocationFlag", label = "Allocation:",choices = NULL)
              ),
              column(2,
                     selectInput(inputId = "Function", label = "Function:", choices = NULL)
              ),
              column(2,
                     selectInput(inputId = "lcat", label = "Labor Category:", choices = NULL)
              ),
              column(2,
                     selectInput(inputId = "filledStatus", label = "Position Status:", choices = c('All','Filled','Unfilled'))
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
    # sidebarLayout(
    #   sidebarPanel(
    #     width = 1,
    #     height = 5
    #     ),
    #   mainPanel(
    #     fluidRow(
    #       column(2, 
    #         selectInput(inputId = "allocationFlag", label = "Allocation:",choices = NULL)
    #       ),
    #       column(3,
    #         selectInput(inputId = "Function", label = "Function:", choices = NULL)
    #       ),
    #       column(3,
    #         selectInput(inputId = "role", label = "Role:", choices = NULL)
    #       )
    #     ),
    #       
    #     fluidRow(
    #       dataTableOutput('flaggedTable')
    #     ),
    #     fluidRow(
    #       dataTableOutput('filteredTable')
    #     )
    #   )
    # )
 ),
 tabPanel("Aggregate View"
 ),
 tabPanel("Hire Forecast"
 ),
 tabPanel("Scenario Planning"
 )
)