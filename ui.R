library(DT)
library(shiny)
library(shinyBS)
library(plyr)
library(reshape2)
library(ggplot2)
library(plotly)
library(data.table)
library(dplyr)

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
 tabPanel("Forecasting View",
          fluidPage(
            fluidRow(
              column(2,
                     selectInput(inputId = "forecastingTaskOrder", label = "Task Order:", choices = c('All','Bravo','Delta'))
              ),
              column(2,
                     selectizeInput(
                       'forecastTasks', 'Tasks:', choices = NULL, multiple = TRUE
                     )
              ),
              column(2,
                     selectInput(inputId = "forecastWorkstream", label = "Workstream:", choices = NULL)
              ),
              column(2,
                     selectInput(inputId = "forecastOrg", label = "Org:", choices = NULL)
              ),
              column(2,
                     selectInput(inputId = "forecastTeam", label = "Team:", choices = NULL)
              ),
              column(2,
                     selectInput(inputId = "forecastFunction", label = "Functional Role:", choices = NULL)
              )
            ),
            fluidRow(
              column(2,
                     selectInput(inputId = "forecastBravoLcat", label = "Bravo LCAT:", choices = NULL)
              ),
              column(2,
                     selectInput(inputId = "forecastDeltaLcat", label = "Delta LCAT:", choices = NULL)
              ),
              column(2,
                     selectInput(inputId = "forecastFilledStatus", label = "Position Status:", choices = c('All','Filled','Unfilled'))
              ),
              column(2,
                     selectInput(inputId = "forecastScenario", label = "Scenario:", choices = c('Best','P Go%','P Win%','Worst'))
              )
              # ,
              # column(1,
              #        selectizeInput(
              #          'taskTest', 'Tasks:', choices = c('All','Task1','Task2','RFS1','RFS2','RFS3','RFS4','etc.'), multiple = TRUE, selected = 'All'
              #        )
              # ),
              # column(1,
              #        selectizeInput(
              #          'otisTest', 'Otis Status:', choices = c('All','Idea','Lead','Qualified Lead'), multiple = TRUE, selected = 'All'
              #        )
              # )
              # ,
              # column(1,
              #        selectInput(inputId = "scenarioTest", label = "Scenario:", choices = c('Scenario 1','All Won','All Lost'))
              # )
              # ,
              # column(1,
              #        selectInput(inputId = "scenarioTest", label = "Forecast:", choices = c('3 Month','6 Month','12 Month'), selected = '12 Month')
              # )
            ),
            fluidRow(
              column(5),
              column(2,
                     selectInput(inputId = "selectedUnit", label = "Select Unit:", choices = c('Hours','FTE'), selected = 'Hours')
              )
            ),
            fluidRow(
              br(),
              plotlyOutput("plot")
            )
          )
 ),
 tabPanel("Hire Forecast"
 ),
 tabPanel("Scenario Planning"
 )
)