shinyServer(function(input, output,session) {
  # Change the below commented code if the working directory is a different folder from the current folder
  # setwd("C:/Users/591548/OneDrive - BOOZ ALLEN HAMILTON/CDM DEFEND/CDM Staffing/CDM DEFEND Resource Allocation/cdm-allocation")
  
  ############################Centralized file import and minor data transformation############################
  
  #Historical spend data from PBS
  rawHistoricalData <- read.table("manuallyCombinedPBS.txt", sep = "\t", header = TRUE, fill = TRUE, stringsAsFactors=FALSE)  
  
  #Job number decode for PBS data
  jobNumberDecode <- read.table("jobNumberDecode.txt", sep = "\t", header = TRUE, fill = TRUE, stringsAsFactors=FALSE)        
  
  #Forecasted spend data from the Combined Spend Plan
  rawDataSet <- read.table("0201spendplan.txt", sep = "\t", header = TRUE, fill = TRUE, stringsAsFactors=FALSE)               
  
  #Clean the data set of empty columns to improve processing time. A lot of the spend plan is "noise."
  rawDataSet <- purgeEmptyColumns(rawDataSet)
  
  #Get polished tasks for summarized spend plan
  spendPlanTaskDecode <- read.table("taskNameDecodeCSP.txt", sep = "\t", header = TRUE, fill = TRUE, stringsAsFactors=FALSE)
  
  #Import more accurate Demographic Information from the Staffing Matrix subset
  DemographicMapping <- read.table("CDM Staffing Matrix for ITSM.txt", header = TRUE, fill = TRUE, stringsAsFactors=FALSE, sep="\t")
  names(DemographicMapping) <- c("Company","ID","Last_Name","First_Name","CDM_Start_Date","End_Date","Task_Order",
                                 "Workstream","Org","Team","Functional_Role","Bravo_LCAT","Delta_LCAT")  
  
  #Import and Transform Bravo RFS Tracker
  rawBravoRFSTracker <- read.table("bravoTestRFSTracker.txt", sep = "\t", header = TRUE, fill = TRUE, stringsAsFactors=FALSE)
  parsedBravoRFSTracker <- rawBravoRFSTracker[,c('Task.Order','RFS.Number','Specific.FTE.Count..Not.required.','Expected.Award.Date','PoP','OTIS.Status','p.Go..','p.Win..')]
  names(parsedBravoRFSTracker) <- c('taskOrder','RFSName','totalFTE','startDate','Length','OTIS','pGo','pWin')
  
  #Import and Transform Delta RFS Tracker
  rawDeltaRFSTracker <- read.table("deltaTestRFSTracker.txt", sep = "\t", header = TRUE, fill = TRUE, stringsAsFactors=FALSE)
  parsedDeltaRFSTracker <- rawDeltaRFSTracker[,c('Task.Order','RFS.Number','Specific.FTE.Count..Not.Required.','Expected.Award.Date','PoP','OTIS.Status','p.Go..','p.Win..')]
  names(parsedDeltaRFSTracker) <- c('taskOrder','RFSName','totalFTE','startDate','Length','OTIS','pGo','pWin')
  
  #Import and Transform BOE data
  rawBoeData <- read.table("BOEdata.txt", sep = "\t", header = TRUE, fill = TRUE, stringsAsFactors=FALSE)
  parsedBoeData <- rawBoeData[,c('Task.Order','RFS','Hours','Expected.Award.Date','PoP','Functional_Role','Bravo_LCAT','Delta_LCAT')]
  names(parsedBoeData) <- c('taskOrder','RFSName','Hours','startDate','Length','Functional_Role','Bravo_LCAT','Delta_LCAT')
  
  ########################Transform different sources into desired format for merging######################################################
  polishedHistoricalData <- transformHistoricalData(rawHistoricalData,jobNumberDecode)
  spendPlanForecastSummary <- transformSpendPlanData(rawDataSet,spendPlanTaskDecode)
  polishedBravoRFSTracker <- transformRFSTracker(parsedBravoRFSTracker)
  polishedDeltaRFSTracker <- transformRFSTracker(parsedDeltaRFSTracker)
  polishedBOEData <- transformBOEData(parsedBoeData)
  
  
  #Aggregate of the PBS data which reduces it to a third of the size
  historicalAggregate <- ddply(polishedHistoricalData, .(taskOrder, JobNo, EmpNo, TransDate), numcolwise(sum))

  
  # # Import txt file of the most recent spend plan and task breakdown
  # 
  # taskBreakdown <- read.table("task-breakdown.txt", sep = "\t", header = TRUE, fill = TRUE, stringsAsFactors=FALSE)
  # names(taskBreakdown) <- c('Task Order','Task/RFS','Title','Task Lead','PBS Support')
  # taskDT <- data.frame(matrix(taskBreakdown, ncol = 5), stringsAsFactors = FALSE)
  # 
  

  

  
  #Bind all hourly data sources
  testHourData <- rbind(historicalAggregate,spendPlanForecastSummary)
  

  #Table containing position information for display
  masterDataForGraph <- merge(testHourData, DemographicMapping, by.x = 'EmpNo', by.y = 'ID', 
                              all.x = TRUE, all.y = FALSE)
  masterDataForGraph <- bind_rows(masterDataForGraph,polishedBOEData,polishedBravoRFSTracker,polishedDeltaRFSTracker)
  

  for(i in 1:nrow(masterDataForGraph)){
    if(is.na(masterDataForGraph$pGo[i])){
      masterDataForGraph$pGo[i]=1      
    }
    if(is.na(masterDataForGraph$pWin[i])){
      masterDataForGraph$pWin[i]=1      
    }
  }
  masterDataForGraph$pGo = as.numeric(masterDataForGraph$pGo)
  masterDataForGraph$pWin = as.numeric(masterDataForGraph$pWin)
  
  #Filter Capabilities
  observe({
    updateSelectizeInput(session, 'forecastTasks', choices = c('All',unique(sort(summaryForecastTable()$JobNo))), server = TRUE, selected = 'All')
    updateSelectInput(session = session, inputId = "forecastWorkstream", choices = c('All',unique(sort(summaryForecastTable()$Workstream))), selected = 'All')
    updateSelectInput(session = session, inputId = "forecastOrg", choices = c('All',unique(sort(summaryForecastTable()$Org))), selected = 'All')
    updateSelectInput(session = session, inputId = "forecastTeam", choices = c('All',unique(sort(summaryForecastTable()$Team))), selected = 'All')
    updateSelectInput(session = session, inputId = "forecastFunction", choices = c('All',unique(sort(masterDataForGraph$Functional_Role))), selected = 'All')
    updateSelectInput(session = session, inputId = "forecastDeltaLcat", choices = c('All',unique(sort(masterDataForGraph$Delta_LCAT))), selected = 'All')
    updateSelectInput(session = session, inputId = "forecastBravoLcat", choices = c('All',unique(sort(masterDataForGraph$Bravo_LCAT))), selected = 'All')
    updateSelectInput(session = session, inputId = "forecastCompany", choices = c('All',unique(sort(masterDataForGraph$Company))), selected = 'All')
    })
  
  summaryForecastTable <- reactive({
    finalForecastTable <- masterDataForGraph
    
    if (input$forecastingTaskOrder != "All") {
      if(input$forecastingTaskOrder == 'Bravo'){
        finalForecastTable <- finalForecastTable[finalForecastTable$taskOrder == 'Bravo',]
      }else{
        finalForecastTable <- finalForecastTable[finalForecastTable$taskOrder == 'Delta',]
      }
    }
    
    if (input$forecastFunction != "All"){
      finalForecastTable <- finalForecastTable[which(finalForecastTable$Functional_Role == input$forecastFunction), ]
    }
    
    if (input$forecastWorkstream != "All"){
      finalForecastTable <- finalForecastTable[which(finalForecastTable$Workstream == input$forecastWorkstream), ]
    }
    
    if (input$forecastOrg != "All"){
      finalForecastTable <- finalForecastTable[which(finalForecastTable$Org == input$forecastOrg), ]
    }
    
    if (input$forecastTeam != "All"){
      finalForecastTable <- finalForecastTable[which(finalForecastTable$Team == input$forecastTeam), ]
    }
    
    return(finalForecastTable)
  })
  
  output$plot <- renderPlotly({
    
    if (input$forecastingTaskOrder != "All") {
      if(input$forecastingTaskOrder == 'Bravo'){
        masterDataForGraph <- masterDataForGraph[masterDataForGraph$taskOrder == 'Bravo',]
      }else{
        masterDataForGraph <- masterDataForGraph[masterDataForGraph$taskOrder == 'Delta',]
      }
    }
    
    if(is.na(length(input$forecastTasks))==FALSE && !('All' %in% input$forecastTasks)){
      selectedTasks <- c()
      for(n in c(1:length(input$forecastTasks))){
        selectedTasks <- c(selectedTasks, input$forecastTasks[n])
      }
      masterDataForGraph <- masterDataForGraph[masterDataForGraph$JobNo %in% selectedTasks,]
    }
    
    if (input$forecastFunction != "All"){
      masterDataForGraph <- masterDataForGraph[which(masterDataForGraph$Functional_Role == input$forecastFunction), ]
    }
    
    if (input$forecastWorkstream != "All"){
      masterDataForGraph <- masterDataForGraph[which(masterDataForGraph$Workstream == input$forecastWorkstream), ]
    }
    
    if (input$forecastOrg != "All"){
      masterDataForGraph <- masterDataForGraph[which(masterDataForGraph$Org == input$forecastOrg), ]
    }

    if (input$forecastTeam != "All"){
      masterDataForGraph <- masterDataForGraph[which(masterDataForGraph$Team == input$forecastTeam), ]
    }
    
    if (input$forecastDeltaLcat != "All"){
      masterDataForGraph <- masterDataForGraph[which(masterDataForGraph$Delta_LCAT == input$forecastDeltaLcat), ]
    }
    
    if (input$forecastBravoLcat != "All"){
      masterDataForGraph <- masterDataForGraph[which(masterDataForGraph$Bravo_LCAT == input$forecastBravoLcat), ]
    }
    
    if (input$forecastFilledStatus != "All") {
      if (input$forecastFilledStatus == "Filled") {
        masterDataForGraph <- masterDataForGraph[is.na(as.integer(masterDataForGraph$EmpNo)) == FALSE,]
      }else if (input$forecastFilledStatus == "Unfilled") {
        masterDataForGraph <- masterDataForGraph[is.na(as.integer(masterDataForGraph$EmpNo)) == TRUE,]
      }
    }
    
    #scenario planning (not working)
    if(input$forecastScenario != 'Best'){
      if(input$forecastScenario == 'P Go%'){
        masterDataForGraph$Hours = masterDataForGraph$Hours*masterDataForGraph$pGo
      }
      if(input$forecastScenario == 'P Win%'){
        masterDataForGraph$Hours = masterDataForGraph$Hours*masterDataForGraph$pWin
      }
      if(input$forecastScenario == 'Worst'){
        for(i in 1:nrow(masterDataForGraph)){
          if(masterDataForGraph$pGo[i] != 1){
            masterDataForGraph$Hours[i] = 0
          }
        }
      }
    }

    #Plot
    summarizedMasterData <- ddply(masterDataForGraph, .(taskOrder, JobNo, TransDate), numcolwise(sum))
    
    chartTitle <- ''
    axisTitle <- ''
    if(input$selectedUnit == 'Hours'){
      chartTitle <- 'Total Hours Per Month'
      axisTitle <- 'Hours'
    }else{
      summarizedMasterData$Hours <- summarizedMasterData$Hours/2080
      chartTitle <- 'Total FTE Per Month'
      axisTitle <- 'FTE'
    }
    

    dataToGraph <- dcast(summarizedMasterData, TransDate ~ JobNo, value.var = 'Hours')  
    data.table::melt(dataToGraph, id.vars='TransDate') %>%
    plot_ly(x = ~TransDate, y = ~value, type = 'bar',
              name = ~variable, color = ~variable) %>%
      layout(title=chartTitle, yaxis = list(title = axisTitle), xaxis=list(title="Month"),barmode = 'stack')
  })
  
  
  
  # 
  # #The headers of the Month Total columns contain the numerical version of the month and the allocation for that month
  # #This parses for that information
  # currentHeaderNames <- names(monthValues <- rawDataSet[ , names(rawDataSet) == "ID" | grepl( "Mo.Hours" , names(rawDataSet) ) ])
  # newHeaderValues <- c('ID')
  # currentMonthValues <- c()
  # monthlyAllocations <- c()
  # for (x in currentHeaderNames){
  #   if(x!='ID'){
  #     newHeaderValues <- c(newHeaderValues,format((as.Date(as.numeric(substring(x,10,14)), origin="1899-12-30")), "%Y-%m"))
  #     currentMonthValues <- c(currentMonthValues,substring(x,10,14))
  #     monthlyAllocations <- c(monthlyAllocations, as.numeric(substring(x,nchar(x)-2,nchar(x))))
  #   }
  # }
  # 
  # observe({
  #   updateSelectizeInput(session, 'tasks', choices = c('All',getTaskNames(rawDataSet)), server = TRUE, selected = 'All')
  #   updateSelectInput(session = session, inputId = "Function", choices = getFunctionalRoles(functionalRoleMapping), selected = 'All')
  #   updateSelectInput(session = session, inputId = "lcat", choices = getLCATs(rawDataSet$LCAT), selected = 'All')
  #   updateSelectInput(session = session, inputId = "monthOfInterest", choices = c('N/A','Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'), selected = 'N/A')
  #   updateSelectInput(session = session, inputId = "allocationFlag", choices = c('All','Over (>105%)','At (80-105%)','Under (1-80%)'), selected = 'All')
  # })
  # 
  # observeEvent(input$resetAll, {
  #   updateSelectInput(session = session, inputId = "team", selected = 'All')
  #   updateSelectInput(session = session, inputId = "tasks", selected = 'All')
  #   updateSelectInput(session = session, inputId = "Function", selected = 'All')
  #   updateSelectInput(session = session, inputId = "lcat", selected = 'All')
  #   updateSelectInput(session = session, inputId = "filledStatus", selected = 'All')
  #   updateSelectInput(session = session, inputId = "monthOfInterest", selected = 'N/A')
  #   updateSelectInput(session = session, inputId = "allocationFlag", selected = 'All')
  # })
  # 
  # summaryTable <- reactive({
  #   #Pull in full data set
  #   filteredDataTable <- rawDataSet
  #   essentialColumns <- names(filteredDataTable)[1:5]
  # 
  #   #Get month values from data set
  #   monthValues <- c()
  #   for (x in names(filteredDataTable[ ,grepl( "Mo.Hours" , names(filteredDataTable) ) ])){
  #     monthValues <- c(monthValues,substring(x,10,14))
  #   }
  # 
  #   #Removes Month Total columns from data set.  Those were only valuable for their headers.
  #   filteredDataTable <- filteredDataTable[ , !grepl( "Mo.Hours" , names(filteredDataTable) ) ]
  # 
  #   #Get month totals for allocation filters based on unaltered totals
  #   unfilteredExportTable <- c()
  #   for(z in c(1:nrow(filteredDataTable))){
  #     unfilteredMonthTotals <- c()
  #     for(u in c(1:length(monthValues))){
  #       currentMonthTotal <- 0
  #       currentLine <- filteredDataTable[z ,grepl( monthValues[u] , names(filteredDataTable) )]
  #       currentMonthTotal <- sum(as.numeric(currentLine[grepl('-',currentLine) == FALSE]))
  #       unfilteredMonthTotals <- c(unfilteredMonthTotals, currentMonthTotal)
  #     }
  #     unfilteredExportTable <- c(unfilteredExportTable, c(filteredDataTable[z,'ID'],round(sum(unfilteredMonthTotals)/2080,digits=2),unfilteredMonthTotals))
  #   }
  # 
  #   #Final table assembly
  #   unfilteredMonthDT <- data.frame(matrix(unfilteredExportTable, ncol = 14, byrow = TRUE), stringsAsFactors = FALSE)
  #   monthLabels <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
  #   for(u in 1:length(monthLabels)){
  #     monthLabels[u] <- paste(monthLabels[u],' (',monthlyAllocations[u],')',sep = '')
  #   }
  #   names(unfilteredMonthDT) <- c('ID','FTE',monthLabels)
  #   unfilteredMonthDT <- unfilteredMonthDT[unfilteredMonthDT$FTE>0,]
  # 
  #   for(x in 2:ncol(unfilteredMonthDT)){
  #     unfilteredMonthDT[,x] = as.numeric(unfilteredMonthDT[,x])
  #   }
  # 
  #   #Ability to filter by Delta and Bravo
  #   if (input$team != "All") {
  #     if(input$team == 'Bravo'){
  #       filteredDataTable <- filteredDataTable[,grepl('DEF.D',names(filteredDataTable)) == FALSE]
  #     }else{
  #       filteredDataTable <- filteredDataTable[,grepl('DEF.B',names(filteredDataTable)) == FALSE]
  #     }
  #   }
  # 
  #   #Ability to filter data by tasks and RFSs of interest
  #   #A lot of columns are removes using numerical calculations.  This preserves the personnel info
  #   columnsToKeep <- c(essentialColumns)
  #   if(is.na(length(input$tasks))==FALSE && !('All' %in% input$tasks)){
  #     for(n in c(1:length(input$tasks))){
  #       if(grepl('RFS',input$tasks[n])){
  #         columnsToKeep <- c(columnsToKeep,names(filteredDataTable)[grepl(input$tasks[n],names(filteredDataTable))])
  #       } else{
  #         tempColumns <- names(filteredDataTable)[grepl(input$tasks[n],names(filteredDataTable))]
  #         columnsToKeep <- c(columnsToKeep, tempColumns[grepl('RFS',tempColumns) == FALSE])
  #       }
  #     }
  #     filteredDataTable <- filteredDataTable[,columnsToKeep]
  #   }
  # 
  #   #Table preserving personnel data
  #   personnelDT <- filteredDataTable[,essentialColumns]
  # 
  #   #Gets month totals for each record
  #   exportTable <- c()
  #   for(z in c(1:nrow(filteredDataTable))){
  #     monthTotals <- c()
  #     for(u in c(1:length(monthValues))){
  #       currentMonthTotal <- 0
  #       currentLine <- filteredDataTable[z ,grepl( monthValues[u] , names(filteredDataTable) )]
  #       currentMonthTotal <- sum(as.numeric(currentLine[grepl('-',currentLine) == FALSE]))
  #       monthTotals <- c(monthTotals, currentMonthTotal)
  #     }
  #     exportTable <- c(exportTable, c(filteredDataTable[z,'ID'],round(sum(monthTotals)/2080,digits=2),monthTotals))
  #   }
  # 
  #   #Final table assembly
  #   monthDT <- data.frame(matrix(exportTable, ncol = 14, byrow = TRUE), stringsAsFactors = FALSE)
  #   monthLabels <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
  #   for(u in 1:length(monthLabels)){
  #     monthLabels[u] <- paste(monthLabels[u],' (',monthlyAllocations[u],')',sep = '')
  #   }
  #   names(monthDT) <- c('ID','FTE',monthLabels)
  # 
  #   #Combines personnel info, corrected functional roles, and month aggregates
  #   finalTable <- merge(merge(personnelDT,monthDT,by="ID"),functionalRoleMapping,all.x = TRUE,all.y = FALSE, by='ID')
  #   finalColumnOrder <- c('ID','Resource','Functional Role','LCAT','FTE',monthLabels)
  #   finalTable <- finalTable[,finalColumnOrder]
  #   #Converts the month totals to numeric for future calculations and sortings
  #   for(c in 5:ncol(finalTable)){
  #     finalTable[,c] = as.numeric(finalTable[,c])
  #   }
  # 
  #   #Ability to filter data by functional role of interest
  #   if ((input$Function != "All") && (length(input$Function))){
  #     if (input$Function != "Other") {
  #       #The use of which() here prevents the table from populating with NAs
  #       finalTable <- finalTable[which(finalTable$"Functional Role" == input$Function), ]
  #     } else{
  #       listOfFunctions <- getFunctionalRoles(functionalRoleMapping)
  #       finalTable <- finalTable[!(finalTable$"Functional Role" %in% listOfFunctions),]
  #     }
  #   }
  # 
  #   #Ability to filter data by LCAT of interest
  #   if (input$lcat != "All") {
  #     if (input$lcat != "Other") {
  #       finalTable <- finalTable[finalTable$LCAT == input$lcat,]
  #     } else{
  #       listOfLcats <- sort(c('Other','Administration/Clerical', 'Applications Developer', 'Applications Systems Analyst', 'Business Process Consultant',
  #                             'Business Systems Analyst', 'Chief Information Security Officer', 'Computer Forensic & Intrusion Analyst',
  #                             'Configuration Management Specialist', 'Data Architect', 'Database Specialist', 'Enterprise Architect',
  #                             'Financial Analyst', 'Hardware Engineer', 'Help Desk Specialist', 'Information Assurance/Security Specialist',
  #                             'Network Specialist', 'Program Manager', 'Project Manager', 'Quality Assurance Specialist', 'Subject Matter Expert',
  #                             'Technical Writer', 'Test Engineer', 'Training Specialist', 'Systems Engineer'))
  #       finalTable <- finalTable[!(finalTable$LCAT %in% listOfLcats),]
  #     }
  #   }
  # 
  #   #Ability to filter data by position status.  Assuming that every filled position (BAH employee or subcontractor)
  #   #has a numeric ID
  #   if (input$filledStatus != "All") {
  #     if (input$filledStatus == "Filled") {
  #       finalTable <- finalTable[is.na(as.integer(finalTable$ID)) == FALSE,]
  #     }else if (input$filledStatus == "Unfilled") {
  #       finalTable <- finalTable[is.na(as.integer(finalTable$ID)) == TRUE,]
  #     }
  #   }
  # 
  #   #Ability to filter by allocation.  Compares selected month total to max allocation collected earlier
  #   if (input$monthOfInterest != 'N/A' && input$allocationFlag != 'All') {
  #     currentColumn <- monthLabels[grepl(input$monthOfInterest,monthLabels)]
  #     currentAllocation <- as.numeric(substring(monthLabels[grepl(input$monthOfInterest,monthLabels)],6,8))
  #     if(input$allocationFlag == 'Over (>105%)'){
  #       unfilteredMonthDT <- unfilteredMonthDT[unfilteredMonthDT[[currentColumn]] >= currentAllocation*1.05,]
  #     }else if(input$allocationFlag == 'At (80-105%)'){
  #       tempTable <- unfilteredMonthDT[unfilteredMonthDT[[currentColumn]] >= currentAllocation*0.8,]
  #       unfilteredMonthDT <- tempTable[tempTable[[currentColumn]] <= currentAllocation*1.05,]
  #     }else if(input$allocationFlag == 'Under (1-80%)'){
  #       tempTable <- unfilteredMonthDT[unfilteredMonthDT[[currentColumn]] > 0,]
  #       unfilteredMonthDT <- tempTable[tempTable[[currentColumn]] <= currentAllocation*0.8,]
  #     }
  #     finalTable <- finalTable[finalTable$ID %in% unfilteredMonthDT$ID,]
  #   }
  # 
  #   finalTable <- finalTable[finalTable$FTE>0,]
  # 
  #   return(finalTable)
  # })
  # 
  # output$allocationTable <- DT::renderDataTable(server = FALSE, datatable(
  #   summaryTable(),
  #   selection = list(mode="single", target="cell"),
  #   extensions = 'Buttons', options = list(
  #     buttons = 'excel',dom = 'Bfrtipl')
  #   # options=list(columnDefs = list(list(visible=FALSE, targets=c(5,7,20:31))))
  #   )
  #          # %>%
  #          # formatStyle(
  #          #    c("Spend (FTE)","Oct","Nov","Dec","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep"),
  #          #    textAlign = 'center'
  #          # ) %>%
  #          # formatStyle(
  #          #   'Oct', 'OctFlag',
  #          #   backgroundColor = styleEqual(c('Over','Under'), c('#ffcccc','#b3c6ff'))
  #          # ) %>%
  #          # formatStyle(
  #          #   'Nov', 'NovFlag',
  #          #   backgroundColor = styleEqual(c('Over','Under'), c('#ffcccc','#b3c6ff'))
  #          # ) %>%
  #          # formatStyle(
  #          #   'Dec', 'DecFlag',
  #          #   backgroundColor = styleEqual(c('Over','Under'), c('#ffcccc','#b3c6ff'))
  #          # ) %>%
  #          # formatStyle(
  #          #   'Jan', 'JanFlag',
  #          #   backgroundColor = styleEqual(c('Over','Under'), c('#ffcccc','#b3c6ff'))
  #          # ) %>%
  #          # formatStyle(
  #          #   'Feb', 'FebFlag',
  #          #   backgroundColor = styleEqual(c('Over','Under'), c('#ffcccc','#b3c6ff'))
  #          # ) %>%
  #          # formatStyle(
  #          #   'Mar', 'MarFlag',
  #          #   backgroundColor = styleEqual(c('Over','Under'), c('#ffcccc','#b3c6ff'))
  #          # ) %>%
  #          # formatStyle(
  #          #   'Apr', 'AprFlag',
  #          #   backgroundColor = styleEqual(c('Over','Under'), c('#ffcccc','#b3c6ff'))
  #          # ) %>%
  #          # formatStyle(
  #          #   'May', 'MayFlag',
  #          #   backgroundColor = styleEqual(c('Over','Under'), c('#ffcccc','#b3c6ff'))
  #          # ) %>%
  #          # formatStyle(
  #          #   'Jun', 'JunFlag',
  #          #   backgroundColor = styleEqual(c('Over','Under'), c('#ffcccc','#b3c6ff'))
  #          # ) %>%
  #          # formatStyle(
  #          #   'Jul', 'JulFlag',
  #          #   backgroundColor = styleEqual(c('Over','Under'), c('#ffcccc','#b3c6ff'))
  #          # ) %>%
  #          # formatStyle(
  #          #   'Aug', 'AugFlag',
  #          #   backgroundColor = styleEqual(c('Over','Under'), c('#ffcccc','#b3c6ff'))
  #          # ) %>%
  #          # formatStyle(
  #          #   'Sep', 'SepFlag',
  #          #   backgroundColor = styleEqual(c('Over','Under'), c('#ffcccc','#b3c6ff'))
  #          # )
  # )
  # 
  # output$taskTable <- DT::renderDataTable(datatable(taskBreakdown, options=list(dom = 't', pageLength = 15), rownames = FALSE))
  # 
  # 
  # output$filteredTable <- DT::renderDataTable(datatable({
  # 
  #   userSelection <- input$allocationTable_cells_selected
  # 
  #   data <- summaryTable()
  # 
  #   if (length(userSelection)) {
  #     data <- data[userSelection[1],]
  #   }
  # 
  #   if(length(userSelection)){
  #     if((userSelection[2] >= 6)&(data[userSelection[2]] != 0)){
  # 
  #       #Assemble secondary data table
  #       userSelectionID <- toString(data[1])
  #       userSelectedPersonnelInfo <- personnelInfo[personnelInfo$ID == userSelectionID,]
  # 
  #       tempDate <- currentMonthValues[userSelection[2]-5]
  #       tempTableFilteredByDate <- rawDataSet[ ,names(rawDataSet) == "ID" | grepl( tempDate , names(rawDataSet))]
  #       tempTableFilteredByID <- tempTableFilteredByDate[tempTableFilteredByDate$ID == userSelectionID,]
  # 
  #       tempColNames <- c()
  #       tempColVals <- c()
  #       for(c in c(1:ncol(tempTableFilteredByID))){
  #         if(any(grep("-",tempTableFilteredByID[1,c]))){
  #         }else{
  #           tempColNames <- c(tempColNames,colnames(tempTableFilteredByID)[c])
  #           tempColVals <- c(tempColVals,tempTableFilteredByID[1,c])
  #         }
  #       }
  # 
  #       userSelectedDataTable <- data.frame(matrix(tempColVals, nrow = 1))
  #       names(userSelectedDataTable) <- tempColNames
  # 
  # 
  #       tempColNames2 <- c(names(userSelectedPersonnelInfo),'Total Month Hours','Total B Hours','Total D Hours','Total 2D Hours')
  #       tempColVals2 <- c(userSelectedPersonnelInfo[1,])
  # 
  #       tempTotal <- userSelectedDataTable[,grepl( "Mo.Hours", names(userSelectedDataTable))][1]
  #       tempBTotal <- 0
  #       tempDTotal <- 0
  #       temp2DTotal <- 0
  #       tempTaskValues <- c()
  # 
  #       for(q in c(1:ncol(userSelectedDataTable))){
  #         if(grepl("DEF.B",names(userSelectedDataTable)[q]) && (grepl('total',tolower(names(userSelectedDataTable)[q]))==FALSE)){
  #           tempBTotal <- tempBTotal + as.numeric(userSelectedDataTable[1,q])
  #           tempColNames2 <- c(tempColNames2,names(userSelectedDataTable)[q])
  #           tempTaskValues <- c(tempTaskValues,as.numeric(userSelectedDataTable[1,q]))
  #         } else if(grepl("DEF.D",names(userSelectedDataTable)[q]) && (grepl('total',tolower(names(userSelectedDataTable)[q]))==FALSE)){
  #           tempDTotal <- tempDTotal + as.numeric(userSelectedDataTable[1,q])
  #           tempColNames2 <- c(tempColNames2,names(userSelectedDataTable)[q])
  #           tempTaskValues <- c(tempTaskValues,as.numeric(userSelectedDataTable[1,q]))
  #         }
  #       }
  # 
  #       tempColVals2 <- c(tempColVals2,tempTotal,tempBTotal,tempDTotal,temp2DTotal,tempTaskValues)
  #       finalFilteredTable <- data.frame(matrix(tempColVals2, nrow = 1))
  #       for(x in c(10:length(tempColNames2))){
  #         tempColNames2[x] = substring(tempColNames2[x],1,nchar(tempColNames2[x])-15)
  #       }
  #       names(finalFilteredTable) <- tempColNames2
  #       colnames(finalFilteredTable) <- gsub("\\.+", " ", colnames(finalFilteredTable))
  # 
  #       return(datatable({finalFilteredTable},
  #                        options=list(dom = 't', columnDefs = list(list(className = 'dt-center', targets = 6:ncol(finalFilteredTable)),
  #                                                                  list(visible=FALSE, targets=c(1:5,9))))))
  #     }else{
  #       errorMessage <- data.frame(matrix(c('Please return to the main screen and select a valid month value.'),nrow = 1))
  #       names(errorMessage)[1] <- 'Note:'
  #       return(datatable(errorMessage, options=list(dom = 't'), rownames = FALSE))
  #     }
  #   }else{
  #     errorMessage <- data.frame(matrix(c('Please return to the main screen and select a valid month value.'),nrow = 1))
  #     names(errorMessage)[1] <- 'Note:'
  #     return(datatable(errorMessage, options=list(dom = 't'), rownames = FALSE))
  #   }
  # 
  #   return()
  # 
  # }
  # )
  # )
  
  
}
)



getTaskNames <- function(sourceTable){
  tempTaskNames <- names(sourceTable[ ,grepl("Task",names(sourceTable)) | grepl("RFS",names(sourceTable)) | grepl("CLIN",names(sourceTable)) ])
  finalRFSNames <- c()
  finalTaskNames <- c()
  
  for(x in c(1:length(tempTaskNames))){
    if(grepl("RFS",tempTaskNames[x])){
      finalRFSNames <- c(finalRFSNames, substr(tempTaskNames[x],1,regexpr('RFS',tempTaskNames[x])+5))
    }else{
      finalTaskNames <- c(finalTaskNames, substr(tempTaskNames[x],1,regexpr('LB',tempTaskNames[x])-2))
    }
  }
  return(sort(unique(c(finalTaskNames,finalRFSNames))))
}

# Function to clean data set.  Reduces number of columns from 600 to 100
purgeEmptyColumns <- function(sourceTable){
  colsToKeep <- c()
  # Any of the Month Total columns (containing "Mo.Hours") need to be kept because they have the allocation totals
  # All other "Total" columns or those with no data are removed.
  for(x in c(1:ncol(sourceTable))){
    if(((all(grepl('-',sourceTable[,x])) == FALSE) | (grepl( "Mo.Hours" , names(sourceTable)[x] ) == TRUE))
       &&(grepl( "Total" , names(sourceTable)[x] ) == FALSE)
       &&(grepl( "TOTAL" , names(sourceTable)[x] ) == FALSE)
       &&(grepl( "X" , names(sourceTable)[x] ) == FALSE)){
      colsToKeep <- c(colsToKeep,x)
    }
  }
  return(sourceTable[,colsToKeep])
}

getFunctionalRoles <- function(staffingMatrixFunctionalRoles){
  defaultFunctionalRoles <- c('Agency Lead','Business Operations Analyst','Business Operations Lead','Capability Engineer','Change and Configuration Management',
                              'Chief Engineer','Cloud and Mobile Capability Lead','Communications Lead','Cyber Architect','Cyber Security Test Analyst',
                              'Cyber Security Test Lead','Data Integrity Support','Data Presentation Capability Lead','Data Protection Capability Lead',
                              'Data Quality Lead','Delivery Manager','Demand Modeling','Deputy Agency Lead','Deputy IAM Lead','Deputy Integration Lead','Deputy PM',
                              'Deputy RFS Execution Lead','Deputy SA Manager','Deputy SIO Manager','Deputy Testing Lead','DI Architect','DI Data Analyst','DI Developer',
                              'DI Development Lead','DI Field Engineer','DI Field Engineering Lead','DI Lead','DI Technical Lead','DI Tester','Digital Engineer',
                              'Endpoint Security Capability Lead','Engineering Capability Lead','Engineering Resource Allocation Manager','Finance Manager',
                              'Financial and Forecast Lead','Governance & Training Lead','Governance Analyst','IA Engineer','IA Lead','IA Security Lead','IAM Capability Lead',
                              'IAM Capability Owner','IAM PM','Information Security Analyst','Integration Lead','IOS Engineer','IOS Manager','ITSM Lead','Lab Architect',
                              'Lab Manager','Lab Support','Lead SIM','NAC Engineer','NAC Lead','PBS Lead','PBS Support','Perimeter Defense Capability Lead','Procurement Lead',
                              'Product Owner','Product Owners Lead','Program Manager','Project Coordination Support','Project Coordinator','Project Coordinator Lead',
                              'Project Expeditor','Quality Lead','Requirements Analyst','Requirements Lead','Resource Lead','Resource Support','RFS Execution Lead',
                              'RFS Execution Oversight','RFS Portfolio Manager','RFS Response Lead','RFS Support','SA Manager','SDD Manager','SELC Lead','Service Desk Support',
                              'ServiceNow Capability Lead','ServiceNow Developer','ServiceNow Engineer','SIO Manager','Solution Architect','Solution Architecture Lead',
                              'Solution Architecture Support','Solutions Architect','Support Desk Lead','Systems Test & Integration Engineer','Talent Acquisition and Sourcing',
                              'Talent Management Lead','Technical Test Lead','Technical Writer','Training Lead','Vulnerability Management Analyst','Vulnerability Management Lead')
  
  # defaultFunctionalRoles <- c('Project Manager', 'Lead Systems Integration Manager', 'Cyber Architect', 'Chief Engineer', 
  # 'RFS Lead (Ongoing Assessment Lead)', 'Deputy Program Manager', 'Executive Assistant', 'Finance Manager', 
  # 'Market Liaison', 'Ongoing Assessment Lead', 'Ongoing Assessment Manager', 'Ongoing Assessment Support', 
  # 'PBS Lead', 'PBS Support', 'Procurement Lead', 'Procurement Analyst', 'Project Coordinator/Scheduler', 
  # 'Project Coordinator/Scheduler Lead', 'Project Expediter', 'Project Operations Lead', 'Quality Lead', 
  # 'Quality Support', 'Resource Lead', 'Resource Support', 'RFS Portfolio Manager', 'RFS Response Lead', 
  # 'RFS Support', 'SubKs', 'Tech Writer', 'Transition-In Manager', 'Transition-In Support', 'Delivery Manager', 
  # 'Agency Lead', 'Data Integration Engineer', 'Data Integration Team Lead', 'Data Integration Tech Lead', 
  # 'Lab Support', 'Products Lead', 'Product Owner', 'Requirements Lead', 'Requirements Analyst', 
  # 'Solution Design and Development Manager', 'Solution Architect Lead', 'Solution Architect', 'Engineer', 
  # 'Cloud and Mobile Capability Lead', 'Data Presentation Capability Engineer', 'Data Presentation Capability Lead', 
  # 'Data Protection Capability Lead', 'Deputy SIO Manager', 'Endpoint Security Capability Lead', 
  # 'Engineering Capability Lead', 'Engineering Resource Lead', 'IDAM and PAM Capability Lead', 
  # 'Incident Lead', 'Integration Lead', 'IOS Engineer (Tool)', 'IOS Lead', 'IOS OPS Liaison', 'IOS Project Coordinator',
  # 'ITSM Lead CSI', 'Lab Architect', 'Lab Manager', 'Lab Support', 'Perimeter Defense Capability Lead', 
  # 'ITIL Process Lead', 'Resource Allocation Lead', 'ServiceNow Capability Lead', 'ServiceNow Developer', 
  # 'ServiceNow Engineer', 'Solution Integration and Operations Manager', 'Agency & Test Integration Engineer', 
  # 'Deputy Solution Assurance Manager', 'Governance/Training Lead', 'Governance Analyst', 
  # 'Information Assurance Engineer', 'Information Assurance Lead', 'Information Assurance SME', 
  # 'Incident Response Lead', 'Quality Assurance/Metrics Lead', 'Quality Assurance Support', 'Solution Assurance Manager', 
  # 'Testing Lead', 'Test Engineer', 'Training Lead', 'Vulnerability Manager', 'Technical Writer', 'Automation Lead', 
  # 'Cyber Operations Manager', 'Cyber Ops Team Lead', 'Cyber Tools Lead', 'Forensics Engineer', 
  # 'Incident Response Team Lead', 'Intrusion Detection Analyst', 'IR Process Manager', 'Penetration Tester', 
  # 'Project Coordinator2', 'SIEM Analyst', 'Threat Indicator/Sharing Lead', 'Vulnerability Manager Surge Cyber')
  
  userInputFunctionalRoles <- staffingMatrixFunctionalRoles[,2]
  userInputFunctionalRoles <- unique(userInputFunctionalRoles)
  relevantFunctionalRoles <- c()
  for (x in 1:length(userInputFunctionalRoles)){
    # print(userInputFunctionalRoles[x])
    if(userInputFunctionalRoles[x] %in% defaultFunctionalRoles){
      relevantFunctionalRoles <- c(relevantFunctionalRoles, userInputFunctionalRoles[x])
    }
  }
  return(c('All','Other',sort(relevantFunctionalRoles)))
  
}

getLCATs <- function(approvedLCATs){
  defaultLCATs <- c('Administration/Clerical', 'Applications Developer', 'Applications Systems Analyst', 'Business Process Consultant', 
                    'Business Systems Analyst', 'Chief Information Security Officer', 'Computer Forensic & Intrusion Analyst',
                    'Configuration Management Specialist', 'Data Architect', 'Database Specialist', 'Enterprise Architect',
                    'Financial Analyst', 'Hardware Engineer', 'Help Desk Specialist', 'Information Assurance/Security Specialist',
                    'Network Specialist', 'Program Manager', 'Project Manager', 'Quality Assurance Specialist', 'Subject Matter Expert',
                    'Technical Writer', 'Test Engineer', 'Training Specialist', 'Systems Engineer')
  userInputLCATs <- unique(approvedLCATs)
  relevantLCATs <- c()
  for (x in 1:length(userInputLCATs)){
    if(userInputLCATs[x] %in% defaultLCATs){
      relevantLCATs <- c(relevantLCATs, userInputLCATs[x])
    }
  }
  return(c('All','Other',sort(relevantLCATs)))
}

transformHistoricalData <- function(pbsDataSet,jobNumberDataSet){
  rawHistoricalData <- pbsDataSet
  
  #Reformat raw data
  names(rawHistoricalData) <- c('taskOrder','JobNo','EmpNo','TransDate','Hours')
  transformedHistoricalData <- rawHistoricalData[,c('taskOrder','EmpNo','JobNo','TransDate','Hours')]
  transformedHistoricalData$TransDate <- as.Date(transformedHistoricalData$TransDate,"%m/%d/%Y")
  transformedHistoricalData$TransDate <- format(as.Date(transformedHistoricalData$TransDate), "%Y-%m")
  transformedHistoricalData$taskOrder <- as.character(transformedHistoricalData$taskOrder)
  transformedHistoricalData$EmpNo <- as.character(transformedHistoricalData$EmpNo)
  transformedHistoricalData$JobNo <- as.character(transformedHistoricalData$JobNo)
  #Remove TIAs
  transformedHistoricalData <- transformedHistoricalData[transformedHistoricalData$Hours>0,]
  
  #Pair with proper task data
  transformedHistoricalData <- merge(transformedHistoricalData, jobNumberDataSet, by.x = c("taskOrder", "JobNo"), by.y = c("Task.Order", "JobNo"), all.x = TRUE, all.y = FALSE)
  transformedHistoricalData$JobNo <- transformedHistoricalData$Decode
  transformedHistoricalData <- transformedHistoricalData[!names(transformedHistoricalData) == 'Decode']
  
  #Return polished dataset
  return(transformedHistoricalData)
}

transformSpendPlanData <- function(dataset,taskDataSet){
  # write.table(dataset, file = "audit.txt", sep = "\t")
  
  #Remove all columns except ID and relevant month data
  dataset <- dataset[ , -which(names(dataset) %in% c('Type','Resource','Function','Role'))]
  
  #Transform data set
  forecastDataSet <- melt(dataset[ , !grepl( "Mo.Hours" , names(dataset) ) ], id.vars=c("ID"))
  forecastDataSet <- forecastDataSet[forecastDataSet$value > 0,]
  names(forecastDataSet) <- c('empID','lineID','hours')
  
  #Parses individual headers for information on Task Order, Task, and Month and creates data table to merge
  parsedColumns <- c()
  currentUniqueValues <- unique(forecastDataSet$lineID)
  for(x in 1:length(currentUniqueValues)){
    #Gets value of interest
    tempValue <- as.character(currentUniqueValues[x])
    
    #Pulls relevant date from the line item and converts it to the same format as the other data sets
    tempDate <- format((as.Date(as.numeric(substring(tempValue,nchar(tempValue)-4,nchar(tempValue))), origin="1899-12-30")), "%Y-%m")
    
    #Determines Task Order for the line item
    tempTaskOrder <- ''
    if(grepl('DEF.B',tempValue)){
      tempTaskOrder <- 'Bravo'
    }else{
      tempTaskOrder <- 'Delta'
    }
    
    #Determines Task Name for the line item (using Combined Spend Plan format)
    tempTaskName <- ''
    if(grepl("RFS",tempValue)){
      tempTaskName <- substr(tempValue,3,regexpr('RFS',tempValue)+5)
    }else{
      tempTaskName <- substr(tempValue,3,regexpr('LB',tempValue)-2)
    }
    
    #Puts all relevant information in one place
    
    parsedColumns <- suppressWarnings(c(parsedColumns,tempValue, tempTaskOrder,tempTaskName, tempDate))
  }
  taskDT <- data.frame(matrix(parsedColumns, ncol = 4, byrow = TRUE), stringsAsFactors = FALSE)
  names(taskDT) <- c('lineID','taskOrder','task','month')
  
  #Combines data tables to include relevant task order, task, and month data
  dtParsedTasks <- merge(forecastDataSet, taskDT, by = 'lineID', all.x = TRUE, all.y = FALSE)
  
  #Format properly
  summarizedSpendPlan <- dtParsedTasks[,c('taskOrder','empID','task','month','hours')]
  names(summarizedSpendPlan) <- c('taskOrder','EmpNo','JobNo','TransDate','Hours')
  summarizedSpendPlan$Hours <- as.numeric(summarizedSpendPlan$Hours)
  
  
  #Substitute user friendly task data
  summarizedSpendPlan <- merge(summarizedSpendPlan, taskDataSet, by.x = c("taskOrder", "JobNo"), by.y = c("taskOrder", "taskName"), all.x = TRUE, all.y = FALSE)
  summarizedSpendPlan$JobNo <- summarizedSpendPlan$Decode
  summarizedSpendPlan <- summarizedSpendPlan[!names(summarizedSpendPlan) == 'Decode']
  
  #Return polished dataset
  return(summarizedSpendPlan)
}

transformRFSTracker <- function(rfsData){
  #Parse and transform for relevant data
  rfsArray <- c()
  for(u in 1:nrow(rfsData)){
    rfsLength <- rfsData$Length[u]  #Duration of current RFS
    currentCount <- 0
    currentDate <- as.Date(rfsData$startDate[u],"%m/%d/%Y") #Get RFS Start Date in usable format
    currentFTE <- rfsData$totalFTE[u] #Get size (FTE) for total RFS
    
    #Loop to add a line item for each month of an RFS
    while(currentCount < rfsLength){
      #Get RFS data in desired order to join with other sources (remember to convert FTE to Hours)
      rfsArray <- c(rfsArray, rfsData$taskOrder[u],rfsData$RFSName[u],'NA',format(as.Date(currentDate), "%Y-%m"),
                    round(currentFTE*2080/rfsLength,2),rfsData$pGo[u],rfsData$pWin[u])
      
      currentDate <- currentDate + 31 #Move to next month to add a line item
      currentCount <- currentCount+1
    }
  }
  
  #Reformat into final table
  bravoTrackerDT <- data.frame(matrix(rfsArray, ncol = 7, byrow = TRUE), stringsAsFactors = FALSE)
  names(bravoTrackerDT) <- c('taskOrder','JobNo','EmpNo','TransDate','Hours','pGo','pWin')
  bravoTrackerDT$Hours <- as.numeric(bravoTrackerDT$Hours)
  bravoTrackerDT <- bravoTrackerDT[bravoTrackerDT$TransDate < '2020-02',]   #Clips for one year in the future
  
  #Return transformed data set
  return(bravoTrackerDT)
  
}

transformBOEData <- function(boeData){
  #Parse and transform for relevant data
  boeArray <- c()
  for(u in 1:nrow(boeData)){
    rfsLength <- boeData$Length[u]  #Duration of current RFS
    currentCount <- 0
    currentDate <- as.Date(boeData$startDate[u],"%m/%d/%Y") #Get RFS Start Date in usable format
    
    #Loop to add a line item for each month of an RFS
    while(currentCount < rfsLength){
      #Get RFS data in desired order to join with other sources (remember to convert FTE to Hours)
      boeArray <- c(boeArray, boeData$taskOrder[u],boeData$RFSName[u],'NA',format(as.Date(currentDate), "%Y-%m"),
                    boeData$Hours[u]/rfsLength,boeData$Functional_Role[u],boeData$Bravo_LCAT[u],boeData$Delta_LCAT[u])
      currentDate <- currentDate + 31   #Move to next month to add a line item
      currentCount <- currentCount+1
    }
  }
  
  #Reformat into final table
  boeDT <- data.frame(matrix(boeArray, ncol = 8, byrow = TRUE), stringsAsFactors = FALSE)
  names(boeDT) <- c('taskOrder','JobNo','EmpNo','TransDate','Hours',"Functional_Role","Bravo_LCAT","Delta_LCAT")
  boeDT$Hours <- as.numeric(boeDT$Hours)
  boeDT <- boeDT[boeDT$TransDate < '2020-02',]   #Clips for one year in the future
  
  #Return transformed data set
  return(boeDT)
}

# Useful code
# write.table(historicalAggregate, file = "audit.txt", sep = "\t")
