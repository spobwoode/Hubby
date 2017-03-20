# this is the filepath where i have been building all the tests
currentPath <- getwd()
# this is the filepath to the test data file
testsFile <- paste(currentPath, '/TestsToRun.xlsx', sep="")
# This is the directory the output file will be saved to
outputPath <- paste(currentPath, '/output/', sep="")
# The outpt filename will be constructed as {THIS FILENAME VALUE}_YYYY-MM-DD.csv, where YYYY_MM_DD is the run date
outputFileName <- 'HiltonHubSanityTests'
# This is the directory where the package will look for sql files if they are named the same as TestName and the column is empty for that test
sqlPath <- paste(currentPath, '/sql/', sep="")
# mThis is the directory for the log file which is generated when the tests are run
logFilePath <- paste(currentPath, '/log/', sep="")




# values for testing
# test <- testDatas(
#    programme='INTL',
#    testName='Kensoo PPC Test',
#    SQLConnectionString='SQLConnectionString',
#    SQLFilePath='sqlqueryorfilelocation',
#    comparisonDataFolderPath='C:/Users/thomas.hamblin/Documents/Git Repositories/R-packages/',
#    comparisonDataFileName='HubUnitTestDSAccount-LevelCost\\d\\d.csv',
#    comparisonMetric='Paid Search Cost',
#    testType="percentageDifference",
#    threshold=10,
#    minimumThreshold=20,
#    headerRow='Date',
#    endRow='469',
#    columnsToInclude="1,2,3"
#   )


# programme <- 'INTL'
# testName <- 'Kensoo PPC Test'
# SQLConnectionString <- 'SQLConnectionString'
# SQLFilePath <- 'sqlqueryorfilelocation'
# comparisonDataFolderPath <- 'C:/Users/thomas.hamblin/Documents/Git Repositories/R-packages/'
# comparisonDataFileName <- 'HubUnitTestDSAccount-LevelCost\\d\\d.csv'
# comparisonMetric <- 'Paid Search Cost'
# testType <- "percentageDifference"
# threshold <- 10
# minimumThreshold <- 20
# headerRow <- 'Date'
# endRow <- '469'
# columnsToInclude <- "1,2,3"







# function to load/install required libraries
installPackages <- function(packageName) {
  if(!packageName %in% installed.packages()) {
    install.packages(packageName,quiet=TRUE)
  } else {
    suppressWarnings(require(package=packageName, character.only=TRUE, quietly=TRUE, warn.conflicts=FALSE))
  }
}

# load required libraries
# RODBC allows R to connect to SQL instances
installPackages( "RODBC" )
# data tables are faster version of data frames
installPackages('data.table')
# readxl allows you to read excel documents
installPackages('readxl')




writeTestResult <- function(
                    programme,
                    testName,
                    threshold,
                    minimumThreshold,
                    comparisonMetric,
                    result
                  ) {

  fileName <-  paste(outputFileName, '_', format(Sys.time(), "%Y-%m-%d"), '.csv', sep="")
  filePath <-  paste(outputPath, fileName, sep="")

  fileList <- list.files(outputPath)

  if( ! is.data.table(result) ) {

    logToFile(programme, testName, 'Error', 'Writing Error: Output is not in data.table format - check package run through with input data')

  } else {

      result[,Programme := programme]
      result[,'Test Name' := testName]
      result[,'Difference Threshold' := threshold]
      result[,'Percentage Difference Threshold' := minimumThreshold]
      result[,'Comparison Metric' := comparisonMetric]

      # output column order
      testcols <- c(
        "Programme",
        "Test Name",
        "Percentage Difference Threshold",
        "Difference Threshold",
        "Difference",
        "Percentage Difference",
        "Test Passed?",
        "Comparison Metric",
        "Comparison Metric Key",
        "SQL Comparison Metric Value",
        "Source Comparison Metric Value"
        )


      # order with test columns and extra columns
      # existingcols <- colnames(result)[! (colnames(result) %in% testcols)]
      # setcolorder(result,c(testcols)),existingcols))

      # remove uneeded columns
      result <- result[,..testcols]

      if(! file.exists(filePath) ) {
        file.create(filePath, showWarnings=FALSE)
        fwrite(result, file=filePath, col.names=TRUE, quote=TRUE)
      } else {
        fwrite(result, file=filePath, append=TRUE,  col.names=FALSE, quote=TRUE)
      }


   }


  return( 'SUCCESS' )
}






logToFile <- function(
  programme,
  testName,
  status,
  message
) {



  filePath <- paste(logFilePath, 'HubTestLog-', format(Sys.time(), "%Y-%m-%d"), '.txt', sep='')

  if(! file.exists(filePath) ) {
      file.create(filePath, showWarnings=FALSE)
  }


  cat( paste(
      # ...output data here...
      format(Sys.time(), "%Y-%m-%dT%X"),
      programme, 
      testName,
      status,
      message,
      sep="  -  "
    ),
    file=filePath,
    sep="\r\n",
    append=TRUE
  )

}








testDatas <- function(
                programme,
                testName,
                SQLConnectionString,
                SQLFilePath="",
                comparisonDataFolderPath,
                comparisonDataFileName,
                comparisonMetric,
                testType="percentageDifference",
                threshold=3,
                minimumThreshold=100,
                headerRow=1,
                endRow=-1,
                columnsToInclude="",
                ignoredValues=""
              ) {
  # CHECK VALUES PASSED TO FUNCTION

  # comparisonDataSource <- 'C:/Users/thomas.hamblin/Documents/Git Repositories/R-packages/HubUnitTestDSAccount-LevelCost.xlsx'
  # SQLConnectionString <-'C:/Users/thomas.hamblin/Documents/Git Repositories/R-packages/MockSqlData.xlsx'


  # exit if the programme value is not INTL or US.
  if( !( programme == "INTL" || programme == "US" ) ) {
      logToFile(programme,testName, "Error","Initialisation Error: programme must be INTL or US")
      return( )
  }

  # TODO: Error logging around these
  threshold <- as.numeric(threshold)
  minimumThreshold <- as.numeric(minimumThreshold)


  if( is.na(threshold) ) {
    logToFile(programme,testName, "Error","Initialisation Error: threshold must be a numeric value")
    return(  )
  }

  if( is.na(minimumThreshold) ) {
    logToFile(programme,testName, "Error","Initialisation Error: minimumThreshold must be a numeric value")
    return(  )
  }

  if( length(list.files(path=comparisonDataFolderPath, pattern=comparisonDataFileName, full.names=TRUE)) == 0) {
    logToFile(programme,testName, "Error","Initialisation Error: comparisonDataSource file does not exist, check the URL")
    return(  )
  }

  # if no name is entered - then check for {testName}.sql in the sql path directory
  if(is.na(SQLFilePath)) {
    SQLFilePath <- paste(sqlPath, testName, '.sql', sep="")
    if(! file.exists(SQLFilePath)) {
        logToFile(programme,testName, "Error",paste('Initialisation Error: SQL file does not exist, make sure the SQL script is named "', testName, '.sql" and it is in ', sqlPath, sep=""))
        return(  )
    } else {
      SQLQuery <- readChar(SQLFilePath, file.info(SQLFilePath)$size)
    }
  } else if(file.exists(SQLFilePath)) {
    SQLQuery <- readChar(SQLFilePath, file.info(SQLFilePath)$size)
  } else {
    logToFile(programme,testName, "Error",'Initialisation Error: SQL Query File Path passed is not a valid file path! Check file path is correct.')
    return(  )
  }

  # default endrow vlaue if empty character is passed
  if(is.character(endRow)) {
    if(endRow == "") {
      endRow <- -1
    }
  }



  # all files that match the filename regex within the folder
  availableDataFiles <- file.info(list.files(path=comparisonDataFolderPath, pattern=comparisonDataFileName, full.names=TRUE))
  # the most recent file
  comparisonDataSource <- rownames(availableDataFiles[rev(order(as.Date(availableDataFiles$mtime))),][1,])



  ##### TODO: Try catches round this -> in case the obdc hasn't been set up
  # acquire SQL Datas
  db <- odbcDriverConnect(paste('driver={SQL Server};',SQLConnectionString,sep=""))
  SQLData <- sqlQuery( db, SQLQuery )
  odbcClose( db )


  # cast endRow to number (if its a text identifier this will be checked)
  endRowNumber <- suppressWarnings(as.numeric(endRow))
  headerRowNumber <- suppressWarnings(as.numeric(headerRow))

  if(!is.na(headerRowNumber)) {
    if(!is.na(endRowNumber)) {
        endRowNumber <- endRowNumber - headerRowNumber
    }
  }
  
  ##### TODO: Try catches round this -> in case there are processing errors
  skip <- 0
  if(!is.na(headerRowNumber)) {
    skip <- headerRowNumber
  }

  if(grepl('\\.csv$',comparisonDataFileName)) {
    #read csv sourceData file
    sourceData <- read.csv(comparisonDataSource,stringsAsFactors=FALSE,header=FALSE,skip=skip)
  } else if(grepl('\\.xlsx?$',comparisonDataFileName)) {
    # read excel sourceData file
    # read it in once to get the number of columns (this is ridiculous there must be a better way)
    sourceData <- read_excel(comparisonDataSource,col_names=FALSE)
    # read it in again but set all the columns to text
    textRep <- rep("text",ncol(sourceData))
    sourceData <- read_excel(comparisonDataSource, col_types=textRep, na="", col_names=FALSE,skip=skip)
  }
  # log filename used
  logToFile(programme,testName, "Processing",paste('Used file ', comparisonDataSource, sep=""))



  #take the bottom off first so it doesn't get moved when you take the top off :)
  # if its a row identfier instead of a row number
  if(!is.na(endRowNumber)) {
    # if the endRow is -1 just use the whole file.
    if(endRowNumber == -1) {
      endRow <- length(sourceData[1,])
    } else {
      endRow <- endRowNumber
    }
  } else {
    endRow <- which(sourceData[,1] == endRow) -1
  }


  #take the top off the file up to the header row
  if(is.na(headerRowNumber)) {
    headerRow <- which(sourceData[,1] == headerRow)
  } else {
    headerRow <- 1
  }


  # validate header/endrow locations (dont want them the wrong way round)
  if(headerRow > endRow) {
    logToFile(programme,testName,"Error","Initialisation Error: endRow must be after headerRow")
  }

  # strip off excess rows
  # source data may have multiple uneeded rows - so strip off any that aren't part of the test
  sourceData <- sourceData[headerRow:endRow,]

  # if columns to include is empty then include all rows
  if(! is.na(columnsToInclude)) {
    columnsToIncludeProcessed <- strsplit(columnsToInclude, ",") 
    columnsToIncludeProcessed <-  do.call(rbind, columnsToIncludeProcessed)
    columnsToIncludeProcessed <- suppressWarnings(as.numeric(columnsToIncludeProcessed[1,]))

    if(any(is.na(columnsToIncludeProcessed))) {
      logToFile(programme,testName,"Error",paste("Initialisation Error: columnsToInclude must be column numbers seperated by commas, a value has been used that is not a number: ", columnsToInclude, sep=""))
      return(  )
    }

    if(any(columnsToIncludeProcessed > length(sourceData[1,]))) {
      logToFile(programme,testName, "Error","Initialisation Error: columnsToInclude specifies column numbers that don't exist")
      return(  )
    } else {
      sourceData <- sourceData[,columnsToIncludeProcessed]
    }
  }


  # take column names row off now that its not needed
  names(sourceData) <- sourceData[1,]
  # remove the headers row
  sourceData <- sourceData[-1,]


  # cast comparison field to numerics
  sourceData[,comparisonMetric] <- suppressWarnings(as.numeric(sourceData[, comparisonMetric]))
  SQLData[,comparisonMetric] <- suppressWarnings(as.numeric(SQLData[, comparisonMetric]))


  # perform sanity checks on sql and source data before attempting to join
  if(length(SQLData[1,]) != length(sourceData)) {
      if(length(SQLData[1,]) > length(sourceData)) {
        more <- 'more'
      } else {
        more <- 'less'
      }

      logToFile(programme,testName, "Error",paste("Initialisation Error: SQL Query returns ",more,' columns than the sourceData, please check query columns match the sourcedata', sep=""))
      return(  )
  } else if (!any(colnames(sourceData) == colnames(sourceData))) {
      logToFile(programme,testName, "Error","Initialisation Error: column names and number of columns must match exactly between SQL and Source Data")
      return(  )
  }



  src <- data.table(sourceData)
  sql <- data.table(SQLData)

  nonComparisonCols <- colnames(src)[which(colnames(src) != comparisonMetric)]


  # src[,'Comparison Metric Key':=' ']

  for (i in 1:nrow(src)) {
    set(src, i, 'Comparison Metric Key', paste(src[i,..nonComparisonCols],collapse="|"))
  }
  


  # left join src data to sql data - i.e for all columns of sourcedata join values of sqldata or NA if it is not matched
  src <- sql[src, on=nonComparisonCols]

  # set NA's to 0 (missin datas on sql side)
  set(src,which(is.na(src[[comparisonMetric]])),comparisonMetric,0)

  # work out the difference (join creates a duplicate column named i.ColumnNameOfJoin)
  src$ValueDiff <- (src[[paste("i.",comparisonMetric,sep="")]] - src[[comparisonMetric]])

  # work out diff as percentage
  src$PercentageDiff <- (src$ValueDiff/src[[paste("i.",comparisonMetric,sep="")]] )*100

  # change column names to SQL Comparison Metric Value and Source Comparison Metric Value
  colnames(src)[which(colnames(src) == paste("i.",comparisonMetric,sep=""))] <- "Source Comparison Metric Value"
  colnames(src)[which(colnames(src) == comparisonMetric)] <- "SQL Comparison Metric Value"

  #KeyCol <- src[,paste(.SD, collapse="|"), by=c(comparisonMetric)]


  # Set test Pass/Fail result
  src[,PercentageDiffPass:= !( (PercentageDiff > threshold & ValueDiff > minimumThreshold) | (PercentageDiff < (threshold*-1) & ValueDiff < (-1*minimumThreshold)) )]

  src$PercentageDiffPass <- as.character(src[,PercentageDiffPass])

  # Set ignored values to IGNORED instead of Pass Fail.  

  if(!any(is.na(ignoredValues))) {
    for(i in 1:nrow(src)) {
      if(length(intersect(ignoredValues, as.matrix(src[i,..nonComparisonCols]))) > 0) {
        src[i,PercentageDiffPass := 'IGNORED']
      }
    }
  }


  colnames(src)[which(colnames(src) == 'PercentageDiff')] <- "Percentage Difference"
  colnames(src)[which(colnames(src) == 'ValueDiff')] <- "Difference"
  colnames(src)[which(colnames(src) == 'PercentageDiffPass')] <- "Test Passed?"

  # log a complete message
  logToFile(programme,testName, "Success","Test Completed - writing to file...")

  

  # write test result to output file
  writeResultStatus <- writeTestResult(
    programme,
    testName,
    threshold,
    minimumThreshold,
    comparisonMetric,
    src
  )

  if( !is.null(writeResultStatus) ) {
    logToFile(programme,testName, "Success","Test Completed - output file additions complete")
  } else {
    logToFile(programme,testName,"Error",'Test Completed - Writing to file failed')
  }


  return( writeResultStatus )

}



formatIgnoredValues <- function(valueString) {

  ignoredValues <- NA

  if(! is.na(valueString)) {
    ignoredValues <- strsplit(valueString, ",") 
    ignoredValues <-  do.call(rbind, ignoredValues)


    # Possible yesterday values:
    # YESTERDAY - changes this to yesterdays date in format: yyyy-mm-dd
    if(any(ignoredValues == 'YESTERDAY')) {
      ignoredValues[which(ignoredValues == 'YESTERDAY')] <- format(Sys.time()-(60*60*24),"%Y-%m-%d")
    }

    # YESTERDAY(formatstring) - changes yesterdays date in format matching formatString
    # must be an R date string
    formatYesterday <- grepl('^YESTERDAY\\([^\\)]+\\)$', ignoredValues)

    if(any(formatYesterday)) {
      # replace the YESTERDAY(...) with just the ...
      ignoredValues[formatYesterday] <- sub('YESTERDAY\\(([^\\)]+)\\)', '\\1', ignoredValues[formatYesterday])

      #use the value as a format string
      ignoredValues[formatYesterday] <- format(Sys.time()-(60*60*24),ignoredValues[formatYesterday])
    }

  }


  return( ignoredValues )
}







runTests <- function() {

  # read all the tests from the Excel Sheet
  text14 <- rep("text",14)
  testList <- tests <- read_excel(testsFile, col_types=text14, na="", skip=24, col_names=FALSE)

  # find last row of data
  if( ! is.na( which(is.na(testList[,1]))[1] ) ) {
    lastRow <- which(is.na(testList[,1]))[1] -1
  } else {
    lastRow <- nrow(testList)
  }


  # TODO: Try catches around this!
  for(i in 1:lastRow) {


    if(is.character(testList[i,1]) & testList[i,1] == 'Enabled') {
      
      ignoredValues <- NA
      
      if(!is.na(testList[i,14])) {
        ignoredValues <- formatIgnoredValues(testList[i,14])
      }
      
      result <- testDatas(
          programme=testList[i,2],
          testName=testList[i,3],
          SQLConnectionString=testList[i,4],
          SQLFilePath=testList[i,5],
          comparisonDataFolderPath=testList[i,6],
          comparisonDataFileName=testList[i,7],
          comparisonMetric=testList[i,8],
          threshold=testList[i,9],
          minimumThreshold=testList[i,10],
          headerRow=testList[i,11],
          endRow=testList[i,12],
          columnsToInclude=testList[i,13],
          ignoredValues=ignoredValues
          # testType=testList[i,9]
      )

      if(is.character(result)) {
        print( paste(testList[i,2], " - ", testList[i,3], ' ran successfully.', sep="") )
      } else {
        warning(paste('Something went wrong on: "', testList[i,2], " - ", testList[i,3], '". Check the logs for more info', sep=""))
      }
    } else {
        logToFile(testList[i,2],testList[i,3], "Skipped","Test is not Enabled")
        print( paste('Skipped "', testList[i,2], " - ", testList[i,3], '" test is not Enabled.', sep="") )
    }
  }

  # TODO: log to file for test completed / errored / whatever

}


# Run it!
runTests()