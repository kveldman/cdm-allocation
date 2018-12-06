shinyServer(function(input, output,session) {
  # Change the below commented code if the working directory is a different folder from the current folder
  # setwd("C:/Users/Kyle/Documents/CDM/R Testing/Allocation/cdm-allocation")
  
  # Import txt file of the most recent spend plan
  rawDataSet <- read.table("1127spendPlan.txt", sep = "\t", header = TRUE, fill = TRUE, stringsAsFactors=FALSE)
  
  
  # Rename 'Role' column to LCAT as per stakeholder specification
  names(rawDataSet)[names(rawDataSet) == "Role"] <- "LCAT"
  
  # Import more accurate Functional Roles from the Staffing Matrix subset
  functionalRoleMapping <- read.table("cdm-staffing-matrix.txt", sep = "\t", header = TRUE, fill = TRUE, stringsAsFactors=FALSE)
  names(functionalRoleMapping)[names(functionalRoleMapping) == "Functional.Role"] <- "Functional Role"
  names(functionalRoleMapping)[1] <- 'ID'

  # Clean the data set of empty columns to improve processing time.  A lot of the spend plan is "noise"
  rawDataSet <- purgeEmptyColumns(rawDataSet)
  
  #Table containing position information for display
  personnelInfo <- data.frame(rawDataSet[,2:6])
  names(personnelInfo) <- names(rawDataSet)[2:6]
  
  #The headers of the Month Total columns contain the numerical version of the month and the allocation for that month
  #This parses for that information
  currentHeaderNames <- names(monthValues <- rawDataSet[ , names(rawDataSet) == "ID" | grepl( "Mo.Hours" , names(rawDataSet) ) ])
  newHeaderValues <- c('ID')
  currentMonthValues <- c()
  monthlyAllocations <- c()
  for (x in currentHeaderNames){
    if(x!='ID'){
      newHeaderValues <- c(newHeaderValues,format((as.Date(as.numeric(substring(x,10,14)), origin="1899-12-30")), "%Y-%m"))
      currentMonthValues <- c(currentMonthValues,substring(x,10,14))
      monthlyAllocations <- c(monthlyAllocations, as.numeric(substring(x,nchar(x)-2,nchar(x))))
    }
  }
  
  observe({
    updateSelectizeInput(session, 'tasks', choices = c('All',getTaskNames(rawDataSet)), server = TRUE, selected = 'All')
    updateSelectInput(session = session, inputId = "Function", choices = getFunctionalRoles(functionalRoleMapping), selected = 'All')
    updateSelectInput(session = session, inputId = "lcat", choices = getLCATs(rawDataSet$LCAT), selected = 'All')
    updateSelectInput(session = session, inputId = "monthOfInterest", choices = c('N/A','Nov','Dec','Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct'), selected = 'N/A')
    updateSelectInput(session = session, inputId = "allocationFlag", choices = c('All','Over (>120%)','Over (100-120%)','At (80-100%)','Under (1-80%)'), selected = 'All')
  })  
  
  observeEvent(input$resetAll, {
    updateSelectInput(session = session, inputId = "team", selected = 'All')
    updateSelectInput(session = session, inputId = "tasks", selected = 'All')
    updateSelectInput(session = session, inputId = "Function", selected = 'All')
    updateSelectInput(session = session, inputId = "lcat", selected = 'All')
    updateSelectInput(session = session, inputId = "filledStatus", selected = 'All')
    updateSelectInput(session = session, inputId = "monthOfInterest", selected = 'N/A')
    updateSelectInput(session = session, inputId = "allocationFlag", selected = 'All')
  })
  
  output$tbl <- renderDataTable( iris, options = list(lengthChange = FALSE))
  
  output$summaryTable <- DT::renderDataTable(datatable({
    #Pull in full data set
    filteredDataTable <- rawDataSet
    essentialColumns <- names(filteredDataTable)[1:5]
  
    #Get month values from data set
    monthValues <- c()
    for (x in names(filteredDataTable[ ,grepl( "Mo.Hours" , names(filteredDataTable) ) ])){
      monthValues <- c(monthValues,substring(x,10,14))
    }
    
    #Removes Month Total columns from data set.  Those were only valuable for their headers.
    filteredDataTable <- filteredDataTable[ , !grepl( "Mo.Hours" , names(filteredDataTable) ) ]

    #Ability to filter by Delta and Bravo
    if (input$team != "All") {
      if(input$team == 'Bravo'){
        filteredDataTable <- filteredDataTable[,grepl('DEF.D',names(filteredDataTable)) == FALSE]
      }else{
        filteredDataTable <- filteredDataTable[,grepl('DEF.B',names(filteredDataTable)) == FALSE]
      }
    }
    
    #Ability to filter data by tasks and RFSs of interest
    #A lot of columns are removes using numerical calculations.  This preserves the personnel info
    columnsToKeep <- c(essentialColumns)
    if(is.na(length(input$tasks))==FALSE && !('All' %in% input$tasks)){
      for(n in c(1:length(input$tasks))){
        if(grepl('RFS',input$tasks[n])){
          columnsToKeep <- c(columnsToKeep,names(filteredDataTable)[grepl(input$tasks[n],names(filteredDataTable))])
        } else{
          tempColumns <- names(filteredDataTable)[grepl(input$tasks[n],names(filteredDataTable))]
          columnsToKeep <- c(columnsToKeep, tempColumns[grepl('RFS',tempColumns) == FALSE])
        }
      }
      filteredDataTable <- filteredDataTable[,columnsToKeep]
    }
    
    #Table preserving personnel data
    personnelDT <- filteredDataTable[,essentialColumns]
    
    #Gets month totals for each record
    exportTable <- c()
    for(z in c(1:nrow(filteredDataTable))){
      monthTotals <- c()
      for(u in c(1:length(monthValues))){
        currentMonthTotal <- 0
        currentLine <- filteredDataTable[z ,grepl( monthValues[u] , names(filteredDataTable) )]
        currentMonthTotal <- sum(as.numeric(currentLine[grepl('-',currentLine) == FALSE]))
        monthTotals <- c(monthTotals, currentMonthTotal)
      }
      exportTable <- c(exportTable, c(filteredDataTable[z,'ID'],round(sum(monthTotals)/2080,digits=2),monthTotals))
    }
    
    #Final table assembly
    monthDT <- data.frame(matrix(exportTable, ncol = 14, byrow = TRUE), stringsAsFactors = FALSE)
    monthLabels <- c('Nov','Dec','Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct')
    for(u in 1:length(monthLabels)){
      monthLabels[u] <- paste(monthLabels[u],' (',monthlyAllocations[u],')',sep = '')
    }
    names(monthDT) <- c('ID','FTE',monthLabels)
    #Combines personnel info, corrected functional roles, and month aggregates
    finalTable <- merge(merge(personnelDT,monthDT,by="ID"),functionalRoleMapping,all.x = TRUE,all.y = FALSE, by='ID')
    finalColumnOrder <- c('ID','Resource','Functional Role','LCAT','FTE',monthLabels)
    finalTable <- finalTable[,finalColumnOrder]
    #Converts the month totals to numeric for future calculations and sortings
    for(c in 5:ncol(finalTable)){
      finalTable[,c] = as.numeric(finalTable[,c])
    }
    
    #Ability to filter data by functional role of interest
    if ((input$Function != "All") && (length(input$Function))){
      if (input$Function != "Other") {
        #The use of which() here prevents the table from populating with NAs
        finalTable <- finalTable[which(finalTable$"Functional Role" == input$Function), ]
      } else{
        listOfFunctions <- getFunctionalRoles(functionalRoleMapping)
        finalTable <- finalTable[!(finalTable$"Functional Role" %in% listOfFunctions),]
      }
    }
    
    #Ability to filter data by LCAT of interest
    if (input$lcat != "All") {
      if (input$lcat != "Other") {
        finalTable <- finalTable[finalTable$LCAT == input$lcat,]
      } else{
        listOfLcats <- sort(c('Other','Administration/Clerical', 'Applications Developer', 'Applications Systems Analyst', 'Business Process Consultant',
                              'Business Systems Analyst', 'Chief Information Security Officer', 'Computer Forensic & Intrusion Analyst',
                              'Configuration Management Specialist', 'Data Architect', 'Database Specialist', 'Enterprise Architect',
                              'Financial Analyst', 'Hardware Engineer', 'Help Desk Specialist', 'Information Assurance/Security Specialist',
                              'Network Specialist', 'Program Manager', 'Project Manager', 'Quality Assurance Specialist', 'Subject Matter Expert',
                              'Technical Writer', 'Test Engineer', 'Training Specialist', 'Systems Engineer'))
        finalTable <- finalTable[!(finalTable$LCAT %in% listOfLcats),]
      }
    }
    
    #Ability to filter data by position status.  Assuming that every filled position (BAH employee or subcontractor)
    #has a numeric ID
    if (input$filledStatus != "All") {
      if (input$filledStatus == "Filled") {
        finalTable <- finalTable[is.na(as.integer(finalTable$ID)) == FALSE,]
      }else if (input$filledStatus == "Unfilled") {
        finalTable <- finalTable[is.na(as.integer(finalTable$ID)) == TRUE,]
      }
    }
    
    #Ability to filter by allocation.  Compares selected month total to max allocation collected earlier
    if (input$monthOfInterest != 'N/A' && input$allocationFlag != 'All') {
      currentColumn <- monthLabels[grepl(input$monthOfInterest,monthLabels)]
      currentAllocation <- as.numeric(substring(monthLabels[grepl(input$monthOfInterest,monthLabels)],6,8))
      if(input$allocationFlag == 'Over (>120%)'){
        finalTable <- finalTable[finalTable[[currentColumn]] >= currentAllocation*1.2,]
      } else if(input$allocationFlag == 'Over (100-120%)'){
        tempTable <- finalTable[finalTable[[currentColumn]] >= currentAllocation,]
        finalTable <- tempTable[tempTable[[currentColumn]] <= currentAllocation*1.2,]
      }else if(input$allocationFlag == 'At (80-100%)'){
        tempTable <- finalTable[finalTable[[currentColumn]] >= currentAllocation*0.8,]
        finalTable <- tempTable[tempTable[[currentColumn]] <= currentAllocation,]
      }else if(input$allocationFlag == 'Under (1-80%)'){
        tempTable <- finalTable[finalTable[[currentColumn]] > 0,]
        finalTable <- tempTable[tempTable[[currentColumn]] <= currentAllocation*0.8,]
      }
    }
    
    finalTable
    },
    selection = list(mode="single", target="cell")
    # options=list(columnDefs = list(list(visible=FALSE, targets=c(5,7,20:31))))
    ) 
           # %>% 
           # formatStyle(
           #    c("Spend (FTE)","Oct","Nov","Dec","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep"),
           #    textAlign = 'center'
           # ) %>% 
           # formatStyle(
           #   'Oct', 'OctFlag',
           #   backgroundColor = styleEqual(c('Over','Under'), c('#ffcccc','#b3c6ff'))
           # ) %>% 
           # formatStyle(
           #   'Nov', 'NovFlag',
           #   backgroundColor = styleEqual(c('Over','Under'), c('#ffcccc','#b3c6ff'))
           # ) %>% 
           # formatStyle(
           #   'Dec', 'DecFlag',
           #   backgroundColor = styleEqual(c('Over','Under'), c('#ffcccc','#b3c6ff'))
           # ) %>% 
           # formatStyle(
           #   'Jan', 'JanFlag',
           #   backgroundColor = styleEqual(c('Over','Under'), c('#ffcccc','#b3c6ff'))
           # ) %>% 
           # formatStyle(
           #   'Feb', 'FebFlag',
           #   backgroundColor = styleEqual(c('Over','Under'), c('#ffcccc','#b3c6ff'))
           # ) %>% 
           # formatStyle(
           #   'Mar', 'MarFlag',
           #   backgroundColor = styleEqual(c('Over','Under'), c('#ffcccc','#b3c6ff'))
           # ) %>% 
           # formatStyle(
           #   'Apr', 'AprFlag',
           #   backgroundColor = styleEqual(c('Over','Under'), c('#ffcccc','#b3c6ff'))
           # ) %>% 
           # formatStyle(
           #   'May', 'MayFlag',
           #   backgroundColor = styleEqual(c('Over','Under'), c('#ffcccc','#b3c6ff'))
           # ) %>% 
           # formatStyle(
           #   'Jun', 'JunFlag',
           #   backgroundColor = styleEqual(c('Over','Under'), c('#ffcccc','#b3c6ff'))
           # ) %>% 
           # formatStyle(
           #   'Jul', 'JulFlag',
           #   backgroundColor = styleEqual(c('Over','Under'), c('#ffcccc','#b3c6ff'))
           # ) %>% 
           # formatStyle(
           #   'Aug', 'AugFlag',
           #   backgroundColor = styleEqual(c('Over','Under'), c('#ffcccc','#b3c6ff'))
           # ) %>% 
           # formatStyle(
           #   'Sep', 'SepFlag',
           #   backgroundColor = styleEqual(c('Over','Under'), c('#ffcccc','#b3c6ff'))
           # )
  )
  
  output$filteredTable <- DT::renderDataTable(datatable({
    print(input$summaryTable_rows)
    filteredDataTable <- rawDataSet
    essentialColumns <- names(filteredDataTable)[1:5]

    #Get month values from data set
    monthValues <- c()
    for (x in names(filteredDataTable[ ,grepl( "Mo.Hours" , names(filteredDataTable) ) ])){
      monthValues <- c(monthValues,substring(x,10,14))
    }

    #Remove extraneous data from data set
    filteredDataTable <- filteredDataTable[ , !grepl( "Mo.Hours" , names(filteredDataTable) ) ]

    exportTable <- c()


    #Ability to filter out Delta and Bravo
    if (input$team != "All") {
      if(input$team == 'Bravo'){
        filteredDataTable <- filteredDataTable[,grepl('DEF.D',names(filteredDataTable)) == FALSE]
      }else{
        filteredDataTable <- filteredDataTable[,grepl('DEF.B',names(filteredDataTable)) == FALSE]
      }
    }

    #Vector to preserve personnel info
    taskNames <- getTaskNames(filteredDataTable)

    columnsToKeep <- c(essentialColumns)
    if(is.na(length(input$tasks))==FALSE && !('All' %in% input$tasks)){
      for(n in c(1:length(input$tasks))){
        if(grepl('RFS',input$tasks[n])){
          columnsToKeep <- c(columnsToKeep,names(filteredDataTable)[grepl(input$tasks[n],names(filteredDataTable))])
        } else{
          tempColumns <- names(filteredDataTable)[grepl(input$tasks[n],names(filteredDataTable))]
          columnsToKeep <- c(columnsToKeep, tempColumns[grepl('RFS',tempColumns) == FALSE])
        }
      }
      filteredDataTable <- filteredDataTable[,columnsToKeep]
    }

    personnelDT <- filteredDataTable[,essentialColumns]
    for(z in c(1:nrow(filteredDataTable))){
      monthTotals <- c()
      for(u in c(1:length(monthValues))){
        currentMonthTotal <- 0
        currentLine <- filteredDataTable[z ,grepl( monthValues[u] , names(filteredDataTable) )]
        currentMonthTotal <- sum(as.numeric(currentLine[grepl('-',currentLine) == FALSE]))
        monthTotals <- c(monthTotals, currentMonthTotal)
      }
      exportTable <- c(exportTable, c(filteredDataTable[z,'ID'],round(sum(monthTotals)/2080,digits=2),monthTotals))
    }

    monthDT <- data.frame(matrix(exportTable, ncol = 14, byrow = TRUE), stringsAsFactors = FALSE)
    monthLabels <- c('Nov','Dec','Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct')
    for(u in 1:length(monthLabels)){
      monthLabels[u] <- paste(monthLabels[u],' (',monthlyAllocations[u],')',sep = '')
    }
    names(monthDT) <- c('ID','FTE',monthLabels)

    finalTable <- merge(merge(personnelDT,monthDT,by="ID"),functionalRoleMapping,all.x = TRUE,all.y = FALSE, by='ID')

    finalColumnOrder <- c('ID','Resource','Functional Role','LCAT','FTE',monthLabels)
    finalTable <- finalTable[,finalColumnOrder]

    for(c in 5:ncol(finalTable)){
      finalTable[,c] = as.numeric(finalTable[,c])
    }

    if ((input$Function != "All") && (length(input$Function))){
      if (input$Function != "Other") {
        finalTable <- finalTable[which(finalTable$"Functional Role" == input$Function), ]
      } else{
        listOfFunctions <- getFunctionalRoles(functionalRoleMapping)
        finalTable <- finalTable[!(finalTable$"Functional Role" %in% listOfFunctions),]
      }
    }

    if (input$lcat != "All") {
      if (input$lcat != "Other") {
        finalTable <- finalTable[finalTable$LCAT == input$lcat,]
      } else{
        listOfLcats <- sort(c('Other','Administration/Clerical', 'Applications Developer', 'Applications Systems Analyst', 'Business Process Consultant',
                              'Business Systems Analyst', 'Chief Information Security Officer', 'Computer Forensic & Intrusion Analyst',
                              'Configuration Management Specialist', 'Data Architect', 'Database Specialist', 'Enterprise Architect',
                              'Financial Analyst', 'Hardware Engineer', 'Help Desk Specialist', 'Information Assurance/Security Specialist',
                              'Network Specialist', 'Program Manager', 'Project Manager', 'Quality Assurance Specialist', 'Subject Matter Expert',
                              'Technical Writer', 'Test Engineer', 'Training Specialist', 'Systems Engineer'))
        finalTable <- finalTable[!(finalTable$LCAT %in% listOfLcats),]
      }
    }
    if (input$filledStatus != "All") {
      if (input$filledStatus == "Filled") {
        finalTable <- finalTable[is.na(as.integer(finalTable$ID)) == FALSE,]
      }else if (input$filledStatus == "Unfilled") {
        finalTable <- finalTable[is.na(as.integer(finalTable$ID)) == TRUE,]
      }
    }
    if (input$monthOfInterest != 'N/A' && input$allocationFlag != 'All') {
      currentColumn <- monthLabels[grepl(input$monthOfInterest,monthLabels)]
      currentAllocation <- as.numeric(substring(monthLabels[grepl(input$monthOfInterest,monthLabels)],6,8))
      if(input$allocationFlag == 'Over (>120%)'){
        finalTable <- finalTable[finalTable[[currentColumn]] >= currentAllocation*1.2,]
      } else if(input$allocationFlag == 'Over (100-120%)'){
        tempTable <- finalTable[finalTable[[currentColumn]] >= currentAllocation,]
        finalTable <- tempTable[tempTable[[currentColumn]] <= currentAllocation*1.2,]
      }else if(input$allocationFlag == 'At (80-100%)'){
        tempTable <- finalTable[finalTable[[currentColumn]] >= currentAllocation*0.8,]
        finalTable <- tempTable[tempTable[[currentColumn]] <= currentAllocation,]
      }else if(input$allocationFlag == 'Under (1-80%)'){
        tempTable <- finalTable[finalTable[[currentColumn]] > 0,]
        finalTable <- tempTable[tempTable[[currentColumn]] <= currentAllocation*0.8,]
      }
    }

    userSelection <- input$summaryTable_cells_selected

    data <- finalTable

    if (length(userSelection)) {
      data <- data[userSelection[1],]
    }

    if(length(userSelection)){
      if((userSelection[2] >= 6)&(data[userSelection[2]] != 0)){

        #Assemble secondary data table
        userSelectionID <- toString(data[1])
        userSelectedPersonnelInfo <- personnelInfo[personnelInfo$ID == userSelectionID,]

        tempDate <- currentMonthValues[userSelection[2]-5]
        tempTableFilteredByDate <- rawDataSet[ ,names(rawDataSet) == "ID" | grepl( tempDate , names(rawDataSet))]
        tempTableFilteredByID <- tempTableFilteredByDate[tempTableFilteredByDate$ID == userSelectionID,]

        tempColNames <- c()
        tempColVals <- c()
        for(c in c(1:ncol(tempTableFilteredByID))){
          if(any(grep("-",tempTableFilteredByID[1,c]))){
          }else{
            tempColNames <- c(tempColNames,colnames(tempTableFilteredByID)[c])
            tempColVals <- c(tempColVals,tempTableFilteredByID[1,c])
          }
        }

        userSelectedDataTable <- data.frame(matrix(tempColVals, nrow = 1))
        names(userSelectedDataTable) <- tempColNames


        tempColNames2 <- c(names(userSelectedPersonnelInfo),'Total Month Hours','Total B Hours','Total D Hours','Total 2D Hours')
        tempColVals2 <- c(userSelectedPersonnelInfo[1,])

        tempTotal <- userSelectedDataTable[,grepl( "Mo.Hours", names(userSelectedDataTable))][1]
        tempBTotal <- 0
        tempDTotal <- 0
        temp2DTotal <- 0
        tempTaskValues <- c()

        for(q in c(1:ncol(userSelectedDataTable))){
          if(grepl(".B.",names(userSelectedDataTable)[q]) && isFALSE(grepl('total',tolower(names(userSelectedDataTable)[q])))){
            tempBTotal <- tempBTotal + as.numeric(userSelectedDataTable[1,q])
            tempColNames2 <- c(tempColNames2,names(userSelectedDataTable)[q])
            tempTaskValues <- c(tempTaskValues,as.numeric(userSelectedDataTable[1,q]))
          } else if(grepl(".D.",names(userSelectedDataTable)[q]) && isFALSE(grepl('total',tolower(names(userSelectedDataTable)[q])))){
            tempDTotal <- tempDTotal + as.numeric(userSelectedDataTable[1,q])
            tempColNames2 <- c(tempColNames2,names(userSelectedDataTable)[q])
            tempTaskValues <- c(tempTaskValues,as.numeric(userSelectedDataTable[1,q]))
          } else if(grepl(".2D.",names(userSelectedDataTable)[q])){
            temp2DTotal <- temp2DTotal + as.numeric(userSelectedDataTable[1,q])
          }
        }

        tempColVals2 <- c(tempColVals2,tempTotal,tempBTotal,tempDTotal,temp2DTotal,tempTaskValues)
        finalFilteredTable <- data.frame(matrix(tempColVals2, nrow = 1))
        for(x in c(10:length(tempColNames2))){
          tempColNames2[x] = substring(tempColNames2[x],1,nchar(tempColNames2[x])-15)
        }
        names(finalFilteredTable) <- tempColNames2
        colnames(finalFilteredTable) <- gsub("\\.+", " ", colnames(finalFilteredTable))

        return(datatable({finalFilteredTable},
                         options=list(dom = 't', columnDefs = list(list(className = 'dt-center', targets = 6:ncol(finalFilteredTable)),
                                                                   list(visible=FALSE, targets=c(1:5,9))))))
      }
    }

    return()

  }
  )
  )
  # output$downloadData <- downloadHandler(
  #   filename <- 'test',
  #   content = function(file) {
  #     write.csv(output$summaryTable, file, row.names = FALSE)
  #   }
  # )

}
)

getTaskNames <- function(sourceTable){
  tempTaskNames <- names(sourceTable[ ,grepl("Task",names(sourceTable)) | grepl("RFS",names(sourceTable)) | grepl("CLIN",names(sourceTable)) ])
  finalRFSNames <- c()
  finalTaskNames <- c()
  
  for(x in c(1:length(tempTaskNames))){
    if(grepl("RFS",tempTaskNames[x])){
      finalRFSNames <- c(finalRFSNames, substr(tempTaskNames[x],regexpr('RFS',tempTaskNames[x]),regexpr('RFS',tempTaskNames[x])+5))
    }else{
      finalTaskNames <- c(finalTaskNames, substr(tempTaskNames[x],regexpr('Task',tempTaskNames[x]),regexpr('LB',tempTaskNames[x])-2))
    }
  }
  return(c(sort(unique(finalTaskNames)),sort(unique(finalRFSNames))))
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
  defaultFunctionalRoles <- c('Project Manager', 'Lead Systems Integration Manager', 'Cyber Architect', 'Chief Engineer', 
  'RFS Lead (Ongoing Assessment Lead)', 'Deputy Program Manager', 'Executive Assistant', 'Finance Manager', 
  'Market Liaison', 'Ongoing Assessment Lead', 'Ongoing Assessment Manager', 'Ongoing Assessment Support', 
  'PBS Lead', 'PBS Support', 'Procurement Lead', 'Procurement Analyst', 'Project Coordinator/Scheduler', 
  'Project Coordinator/Scheduler Lead', 'Project Expediter', 'Project Operations Lead', 'Quality Lead', 
  'Quality Support', 'Resource Lead', 'Resource Support', 'RFS Portfolio Manager', 'RFS Response Lead', 
  'RFS Support', 'SubKs', 'Tech Writer', 'Transition-In Manager', 'Transition-In Support', 'Delivery Manager', 
  'Agency Lead', 'Data Integration Engineer', 'Data Integration Team Lead', 'Data Integration Tech Lead', 
  'Lab Support', 'Products Lead', 'Product Owner', 'Requirements Lead', 'Requirements Analyst', 
  'Solution Design and Development Manager', 'Solution Architect Lead', 'Solution Architect', 'Engineer', 
  'Cloud and Mobile Capability Lead', 'Data Presentation Capability Engineer', 'Data Presentation Capability Lead', 
  'Data Protection Capability Lead', 'Deputy SIO Manager', 'Endpoint Security Capability Lead', 
  'Engineering Capability Lead', 'Engineering Resource Lead', 'IDAM and PAM Capability Lead', 
  'Incident Lead', 'Integration Lead', 'IOS Engineer (Tool)', 'IOS Lead', 'IOS OPS Liaison', 'IOS Project Coordinator',
  'ITSM Lead CSI', 'Lab Architect', 'Lab Manager', 'Lab Support', 'Perimeter Defense Capability Lead', 
  'ITIL Process Lead', 'Resource Allocation Lead', 'ServiceNow Capability Lead', 'ServiceNow Developer', 
  'ServiceNow Engineer', 'Solution Integration and Operations Manager', 'Agency & Test Integration Engineer', 
  'Deputy Solution Assurance Manager', 'Governance/Training Lead', 'Governance Analyst', 
  'Information Assurance Engineer', 'Information Assurance Lead', 'Information Assurance SME', 
  'Incident Response Lead', 'Quality Assurance/Metrics Lead', 'Quality Assurance Support', 'Solution Assurance Manager', 
  'Testing Lead', 'Test Engineer', 'Training Lead', 'Vulnerability Manager', 'Technical Writer', 'Automation Lead', 
  'Cyber Operations Manager', 'Cyber Ops Team Lead', 'Cyber Tools Lead', 'Forensics Engineer', 
  'Incident Response Team Lead', 'Intrusion Detection Analyst', 'IR Process Manager', 'Penetration Tester', 
  'Project Coordinator2', 'SIEM Analyst', 'Threat Indicator/Sharing Lead', 'Vulnerability Manager Surge Cyber')
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