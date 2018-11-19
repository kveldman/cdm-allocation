shinyServer(function(input, output,session) {
  setwd("C:/Users/Kyle/Documents/CDM/R Testing/Allocation/cdm-allocation")
  
  rawDataSet <- read.table("1113spendPlan.txt", sep = "\t", header = TRUE, fill = TRUE, stringsAsFactors=FALSE)
  # rfs10 <- read.table("rfs10_draft.txt", sep = "\t", header = TRUE, fill = TRUE, stringsAsFactors=FALSE)
  
  # importFutureRFS(rfs10)
  rawDataSet <- purgeEmptyColumns(rawDataSet)
  # vectorizeData(rawDataSet)
  # print(rawDataSet[rawDataSet$ID == 'D-R5-TBD21-4',])
 
  
  #Table containing position information for display
  personnelInfo <- data.frame(rawDataSet[,2:6])
  names(personnelInfo) <- names(rawDataSet)[2:6]
  # print(names(personnelInfo))
  names(personnelInfo)[4] <- 'LCAT'
  # print(names(personnelInfo))
  
  #Table gathering all month totals for employees.  
  monthValues <- rawDataSet[ , names(rawDataSet) == "ID" | grepl( "Mo.Hours" , names(rawDataSet) ) ]

  # monthValues <- getMonthTotals(rawDataSet)
  
  #Gathers headers in a non-User-friendly format
  currentHeaderNames <- names(monthValues)
  
  #Converts dates in current headings to readable format and collects month values
  newHeaderValues <- c('ID')
  # print(currentHeaderNames)
  currentMonthValues <- c()
  for (x in currentHeaderNames){
    if(x!='ID'){
      newHeaderValues <- c(newHeaderValues,format((as.Date(as.numeric(substring(x,10,14)), origin="1899-12-30")), "%Y-%m"))
      currentMonthValues <- c(currentMonthValues,substring(x,10,14))
    }
  }
  
  #Changes the headers
  names(monthValues) <- newHeaderValues

  
  #Collects all monthly allocations from the header
  monthlyAllocations <- c()
  for (x in currentHeaderNames){
    if (x != "ID") {
      monthlyAllocations <- c(monthlyAllocations, as.numeric(substring(x,nchar(x)-2,nchar(x))))
    }
  }
  
  #Parsing all employees for allocation information
  testMonthValues <- c() #List of monthly totals for each employee
  formattingValues <- c() #Booleans used to determine column fill
  dataForFinalTable <- c() #What will be used to populate final table in UI
  tempTotalSpend <- 0
  
  
  for(w in c(1:nrow(monthValues))){
    for(t in c(2:length(monthValues[w,]))){
      if(any(grep("-",monthValues[w,t]))){
        testMonthValues <- c(testMonthValues,0)
      } else{
        testMonthValues <- c(testMonthValues,as.numeric(monthValues[w,t]))
        tempTotalSpend <- tempTotalSpend + as.numeric(monthValues[w,t])
      }
    }
    #If overallocated, populates rows with ID | Month Totals | Overallocation Booleans
    
    overAllocatedFlag <- 0
    underAllocatedFlag <- 0
    allocationLabels <- c()
    for(p in c(1:length(testMonthValues))){
      if(testMonthValues[p] > monthlyAllocations[p]){
        overAllocatedFlag <- 1
        allocationLabels <- c(allocationLabels, 'Over')
      }else if((testMonthValues[p] <= 0.8 * monthlyAllocations[p]) && (testMonthValues[p] != 0)){
        underAllocatedFlag <- 1
        allocationLabels <- c(allocationLabels, 'Under')
      }else{
        allocationLabels <- c(allocationLabels, 'At')
      }
    }
    if((overAllocatedFlag == 1) & (underAllocatedFlag ==  1)){
      dataForFinalTable <- c(dataForFinalTable, monthValues[w,1], round(tempTotalSpend/2080.0,2), "Both", testMonthValues,allocationLabels)
    }else if(overAllocatedFlag == 1){
      dataForFinalTable <- c(dataForFinalTable, monthValues[w,1], round(tempTotalSpend/2080.0,2), "Over", testMonthValues,allocationLabels)
    }else if(underAllocatedFlag == 1){
      dataForFinalTable <- c(dataForFinalTable, monthValues[w,1], round(tempTotalSpend/2080.0,2), "Under", testMonthValues,allocationLabels)
    }else{
      dataForFinalTable <- c(dataForFinalTable, monthValues[w,1], round(tempTotalSpend/2080.0,2), "At", testMonthValues,allocationLabels)
    }
    
    testMonthValues <- c()
    tempTotalSpend <- 0
  }

  #Table to load in UI
  
  finalTable <- data.frame(matrix(dataForFinalTable, ncol = 27, byrow = TRUE))
  names(finalTable) <- c("ID","Spend (FTE)","Allocation Flag","Nov","Dec","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct",
                         "NovFlag","DecFlag","JanFlag","FebFlag","MarFlag","AprFlag","MayFlag","JunFlag","JulFlag","AugFlag","SepFlag","OctFlag")


  #Assembly of final table to be displayed
  mergedTable <- merge(personnelInfo,finalTable,by="ID")
  listOfFunctions <- c()
  
  
  observe({
    # listOfLcats <- unique(mergedTable$LCAT)[order(unique(mergedTable$LCAT))]
    listOfLcats <- sort(c('Other','Administration/Clerical', 'Applications Developer', 'Applications Systems Analyst', 'Business Process Consultant', 
                     'Business Systems Analyst', 'Chief Information Security Officer', 'Computer Forensic & Intrusion Analyst', 
                     'Configuration Management Specialist', 'Data Architect', 'Database Specialist', 'Enterprise Architect', 
                     'Financial Analyst', 'Hardware Engineer', 'Help Desk Specialist', 'Information Assurance/Security Specialist', 
                     'Network Specialist', 'Program Manager', 'Project Manager', 'Quality Assurance Specialist', 'Subject Matter Expert', 
                     'Technical Writer', 'Test Engineer', 'Training Specialist', 'Systems Engineer'))
    listOfFunctions <- sort(c('Other','Project Manager', 'Lead Systems Integration Manager', 'Cyber Architect', 'Chief Engineer', 
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
                     'Project Coordinator2', 'SIEM Analyst', 'Threat Indicator/Sharing Lead', 'Vulnerability Manager Surge Cyber'))
    # listOfFunctions <- unique(mergedTable$Function)[order(unique(mergedTable$Function))]
    listOfTasks <- getTaskNames(rawDataSet)
    
    updateSelectInput(session = session, inputId = "allocationFlag", choices = c('All','Over','Under'), selected = 'Over')
    updateSelectInput(session = session, inputId = "Function", choices = c('All',listOfFunctions), selected = 'All')
    updateSelectInput(session = session, inputId = "lcat", choices = c('All',listOfLcats), selected = 'All')
    updateSelectizeInput(session, 'tasks', choices = c('All',listOfTasks), server = TRUE, selected = 'All')
    updateSelectInput(session = session, inputId = "futureAvailability", choices = c('N/A',names(finalTable)[4:15]), selected = 'N/A')
  })  
  
  output$flaggedTable <- DT::renderDataTable(datatable({
    # testTable <- rawDataSet
    # 
    # monthValues <- c()
    # for (x in names(testTable[ ,grepl( "Mo.Hours" , names(testTable) ) ])){
    #   monthValues <- c(monthValues,substring(x,10,14))
    # }
    # 
    # testTable <- testTable[ , !grepl( "Mo.Hours" , names(testTable) ) ]
    # 
    # exportTable <- c()
    # 
    # if (input$team != "All") {
    #   if(input$team == 'Bravo'){
    #     testTable <- testTable[,grepl('DEF.D',names(testTable)) == FALSE]
    #   }else{
    #     testTable <- testTable[,grepl('DEF.B',names(testTable)) == FALSE]
    #   }
    # }
    # 
    # if (!('All' %in% input$tasks)) {
    #   currentTasks <- input$tasks[1]
    #   print(length(currentTasks[1]))
    # }
    # 
    # print(length(as.vector(input$tasks)))
    
    # for(z in c(1:nrow(testTable))){
    #   monthTotals <- c()
    #   for(u in c(1:length(monthValues))){
    #     currentMonthTotal <- 0
    #     currentLine <- testTable[z ,grepl( monthValues[u] , names(testTable) )]
    #     currentMonthTotal <- sum(as.numeric(currentLine[grepl('-',currentLine) == FALSE]))
    #     monthTotals <- c(monthTotals, currentMonthTotal)
    #   }
    #   exportTable <- c(exportTable, c(testTable[z,'ID'],monthTotals))
    # }
    # 
    # print(count)
    # monthDT <- data.frame(matrix(exportTable, ncol = 13, byrow = TRUE), stringsAsFactors = FALSE)
    # print(monthDT[1:30,])
    
    
    data <- mergedTable
    if (input$allocationFlag != "All") {
      data <- data[data$"Allocation Flag" == input$allocationFlag | data$"Allocation Flag" == 'Both',]
    }
    if (input$Function != "All") {
      if (input$Function != "Other") {
        data <- data[data$Function == input$Function,]
      } else{
        listOfFunctions <- sort(c('Other','Project Manager', 'Lead Systems Integration Manager', 'Cyber Architect', 'Chief Engineer', 
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
                                  'Project Coordinator2', 'SIEM Analyst', 'Threat Indicator/Sharing Lead', 'Vulnerability Manager Surge Cyber'))
        data <- data[!(data$Function %in% listOfFunctions),]
      }
    }
    if (input$lcat != "All") {
      if (input$lcat != "Other") {
        data <- data[data$LCAT == input$lcat,]
      } else{
        listOfLcats <- sort(c('Other','Administration/Clerical', 'Applications Developer', 'Applications Systems Analyst', 'Business Process Consultant', 
                              'Business Systems Analyst', 'Chief Information Security Officer', 'Computer Forensic & Intrusion Analyst', 
                              'Configuration Management Specialist', 'Data Architect', 'Database Specialist', 'Enterprise Architect', 
                              'Financial Analyst', 'Hardware Engineer', 'Help Desk Specialist', 'Information Assurance/Security Specialist', 
                              'Network Specialist', 'Program Manager', 'Project Manager', 'Quality Assurance Specialist', 'Subject Matter Expert', 
                              'Technical Writer', 'Test Engineer', 'Training Specialist', 'Systems Engineer'))
        data <- data[!(data$LCAT %in% listOfLcats),]
      }
    }
    if (input$filledStatus != "All") {
      if (input$filledStatus == "Filled") {
        data <- data[is.na(as.integer(data$ID)) == FALSE,]
      }else if (input$filledStatus == "Unfilled") {
        data <- data[is.na(as.integer(data$ID)) == TRUE,]
      }
    }
    if (input$futureAvailability != 'N/A') {
      monthSelector = paste(input$futureAvailability,'Flag', sep='')
      data <- data[data[[monthSelector]] == 'Under',]
    }
    data
    },
    selection = list(mode="single", target="cell"),
    options=list(columnDefs = list(list(visible=FALSE, targets=c(5,7,20:31))))) %>% 
           formatStyle(
              c("Spend (FTE)","Oct","Nov","Dec","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep"),
              textAlign = 'center'
           ) %>% 
           formatStyle(
             'Oct', 'OctFlag',
             backgroundColor = styleEqual(c('Over','Under'), c('#ffcccc','#b3c6ff'))
           ) %>% 
           formatStyle(
             'Nov', 'NovFlag',
             backgroundColor = styleEqual(c('Over','Under'), c('#ffcccc','#b3c6ff'))
           ) %>% 
           formatStyle(
             'Dec', 'DecFlag',
             backgroundColor = styleEqual(c('Over','Under'), c('#ffcccc','#b3c6ff'))
           ) %>% 
           formatStyle(
             'Jan', 'JanFlag',
             backgroundColor = styleEqual(c('Over','Under'), c('#ffcccc','#b3c6ff'))
           ) %>% 
           formatStyle(
             'Feb', 'FebFlag',
             backgroundColor = styleEqual(c('Over','Under'), c('#ffcccc','#b3c6ff'))
           ) %>% 
           formatStyle(
             'Mar', 'MarFlag',
             backgroundColor = styleEqual(c('Over','Under'), c('#ffcccc','#b3c6ff'))
           ) %>% 
           formatStyle(
             'Apr', 'AprFlag',
             backgroundColor = styleEqual(c('Over','Under'), c('#ffcccc','#b3c6ff'))
           ) %>% 
           formatStyle(
             'May', 'MayFlag',
             backgroundColor = styleEqual(c('Over','Under'), c('#ffcccc','#b3c6ff'))
           ) %>% 
           formatStyle(
             'Jun', 'JunFlag',
             backgroundColor = styleEqual(c('Over','Under'), c('#ffcccc','#b3c6ff'))
           ) %>% 
           formatStyle(
             'Jul', 'JulFlag',
             backgroundColor = styleEqual(c('Over','Under'), c('#ffcccc','#b3c6ff'))
           ) %>% 
           formatStyle(
             'Aug', 'AugFlag',
             backgroundColor = styleEqual(c('Over','Under'), c('#ffcccc','#b3c6ff'))
           ) %>% 
           formatStyle(
             'Sep', 'SepFlag',
             backgroundColor = styleEqual(c('Over','Under'), c('#ffcccc','#b3c6ff'))
           )
  )
  
  output$filteredTable <- DT::renderDataTable(datatable({
    
    userSelection <- input$flaggedTable_cells_selected
    
    data <- mergedTable
    if (input$allocationFlag != "All") {
      data <- data[data$"Allocation Flag" == input$allocationFlag | data$"Allocation Flag" == 'Both',]
    }
    if (input$Function != "All") {
      data <- data[data$Function == input$Function,]
    }
    if (input$lcat != "All") {
      data <- data[data$LCAT == input$lcat,]
    }
    if (input$filledStatus != "All") {
      if (input$filledStatus == "Filled") {
        data <- data[is.na(as.integer(data$ID)) == FALSE,]
      }else if (input$filledStatus == "Unfilled") {
        data <- data[is.na(as.integer(data$ID)) == TRUE,]
      }
    }
    
    if (input$futureAvailability != 'N/A') {
      monthSelector = paste(input$futureAvailability,'Flag', sep='')
      data <- data[data[[monthSelector]] == 'Under',]
    }
    if (length(userSelection)) {
      data <- data[userSelection[1],]
    }
    
    if(length(userSelection)){
      if((userSelection[2] > 7)&(data[userSelection[2]] != 0)){
        
        #Assemble secondary data table
        userSelectionID <- toString(data[1])
        userSelectedPersonnelInfo <- personnelInfo[personnelInfo$ID == userSelectionID,]
        
        tempDate <- currentMonthValues[userSelection[2]-7]
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

purgeEmptyColumns <- function(sourceTable){
  colsToKeep <- c()
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

getMonthTotals <- function(sourceTable){
  taskNames <- getTaskNames(sourceTable)
  
  monthValues <- c()
  for (x in names(sourceTable[ ,grepl( "Mo.Hours" , names(sourceTable) ) ])){
    monthValues <- c(monthValues,substring(x,10,14))
  }
  
  exportTable <- c()
  count <- 0
  
  # print(names(sourceTable))
  
  monthVector <- c()
  taskVector <- c()
  
  for (c in 1:length(names(sourceTable))){
    foundMonth <- 0
    foundTask <- 0
    
    for (f in 1:length(monthValues)){
      if(grepl(monthValues[f],names(sourceTable)[c])){
        monthVector <- c(monthVector,monthValues[f])
        foundMonth <- 1
        break
      } 
    }
    if(foundMonth == 0){
      monthVector <- c(monthVector,'')
    }
    
    for (k in 1:length(taskNames)){
      if(grepl(taskNames[k],names(sourceTable)[c])){
        taskVector <- c(taskVector,taskNames[k])
        foundTask <- 1
        break
      }
    }
    if(foundTask == 0){
      taskVector <- c(taskVector,'')
    }
  }

  for(z in c(1:nrow(sourceTable))){
    monthTotals <- c()
    for(u in c(1:length(monthValues))){
      currentMonthTotal <- 0
      for(h in c(1:length(taskNames))){
        currentLine <- sourceTable[z ,grepl( monthValues[u] , names(sourceTable) ) & grepl( taskNames[h] , names(sourceTable) ) ]
        currentMonthTotal <- currentMonthTotal + sum(as.numeric(currentLine[grepl('-',currentLine) == FALSE]))
        count <- count + 1
      }
      monthTotals <- c(monthTotals, currentMonthTotal)
    }
    exportTable <- c(exportTable, c(sourceTable[z,'ID'],monthTotals))
  }
  
  # print(count)
  monthDT <- data.frame(matrix(exportTable, ncol = 13, byrow = TRUE), stringsAsFactors = FALSE)
  names(monthDT) <- names(sourceTable[ , names(sourceTable) == "ID" | grepl( "Mo.Hours" , names(sourceTable) ) ])
  
  return(monthDT)
}

importFutureRFS <- function(rfsTable){
  #Purge unnecessary columns
  rfsTable <- rfsTable[,c(1:7,18:ncol(rfsTable))]
  rfsData <- c()
  for(x in 1:nrow(rfsTable)){
    tempID <- paste('RFS10','-',as.character(x),sep='')
    tempLcat <- rfsTable[x,6]
    
  }
  print(rfsTable)
}

# vectorizeData <- function(unformattedTable){
#   print(names(unformattedTable))
#   for(x in 1:nrow(unformattedTable)){
#     currentID <- unformattedTable[x,]$ID
#     for(y in 1:ncol(unformattedTable)){
#       
#     }
#     # print(currentID)
#     # if(is.null(unformattedTable[x,]$ID) == FALSE){
#     #   currentID <- unformattedTable[x,]$ID
#     #   
#     # }
#   }
# }
