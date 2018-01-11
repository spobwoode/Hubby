#######
# If you're running this in R Studio on windows please ensure the file has been loaded with the correct encoding
#
# If thats not the case then hit File -> Reopen With Encoding...  and select UTF-8
#
# ï»¿   <--- this should appear as an i with two dots, two small >'s and an upside down question mark :)
#######







# this is the filepath where i have been building all the tests
currentPath <- 'C:/HiltonGuestShare/Hubby' # 
#currentPath <- getwd()

# empty list of temp files - this will be filled as the tests are run,
# and used to delete the temp files downloaded from external sources
tempFiles <- list()
cleanupList <- list()

# this is the filepath to the test definitions
testsFile <- paste(currentPath, '/SourceVHubDataTests_newDec17.xlsx', sep="")

##########
#### ALL OF THE BELOW CAN NOW BE OVERRIDDEN IN THE TEST SHEET OR IN CMD LINE PARAMS
#### THESE ARE USED AS DEFAULT VALUES IF THEY ARE NOT SPECIFIED AS ARGUMENTS


# outputType of "File" or "SQL"
outputType <- 'File'

# This is the directory the output file will be saved to
# or the sql connection string to access the the output database
#outputPath <- '//hiltonagg01/hiltonagg01data/Box Sync/HiltonHubSanityTests/' # paste(currentPath, '/output/', sep="") # 
outputPath <- '//hiltonagg01/hiltonagg01data/PowerBIOneDrive/OneDrive - Rakuten Attribution/Developers/HiltonHubSanityTests/'

# The outpt filename will be constructed as {THIS FILENAME VALUE}_YYYY-MM-DD.csv, where YYYY_MM_DD is the run date
# or the sql table name
outputFileName <- 'HiltonHubSanityTests'

# This is the directory where the package will look for sql files if they are named the same as TestName and the column is empty for that test
sqlPath <- paste(currentPath, '/sql/', sep="")

# This is the directory for the log file which is generated when the tests are run
logFilePath <- paste(currentPath, '/log/', sep="")

# This is the directory for the temp files
tempFilesPath <- paste(currentPath, '/tmp/', sep="")

# the time the test script was
runStartTime <- Sys.time()

# this is the format that all dates will be coerced to.
coercedDateFormat <- '%Y-%m-%d'

# set to 1 to have the programme print everything its doing to the console as well as the log.
# set to 0 to quiet down
# this can be overriden with the cmd line params or in the test definitions file
verbose <- 0

####
##########



# function to load/install required libraries
installPackages <- function(packageName) {
  if(!packageName %in% installed.packages()) {
    install.packages(packageName,quiet=TRUE, repos="http://cran.rstudio.com/")
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
# curl library to get file names of ftp files
installPackages('RCurl')
# janitor for data cleaning functions
installPackages("janitor")
# janitor for data cleaning functions
installPackages("stringr")







writeTestResult <- function(
  programme,
  testName,
  threshold,
  minimumThreshold,
  comparisonMetric,
  result
) {
  
  fileName <-  paste(outputFileName, '_', format(runStartTime, "%Y-%m-%dT%H-%M"), '.csv', sep="")
  filePath <-  paste(outputPath, fileName, sep="")
  dateTime <- format(runStartTime,"%Y-%m-%d %H:%M:%OS")
  
  fileList <- list.files(outputPath)
  
  if( ! is.data.table(result) ) {
    
    logToFile(programme, testName, 'Error', 'Writing Error: Output is not in data.table format - check package run through with input data')
    
  } else {
    result[,'DateTime' := dateTime]
    result[,'Programme' := programme]
    result[,'Test Name' := testName]
    result[,'Difference Threshold' := threshold]
    result[,'Percentage Difference Threshold' := minimumThreshold]
    result[,'Comparison Metric' := comparisonMetric]
    
    # output column order
    testcols <- c(
      "DateTime",
      "Programme",
      "Test Name",
      "Percentage Difference Threshold",
      "Difference Threshold",
      "Difference",
      "Percentage Difference",
      "Test Passed?",
      "Comparison Metric",
      "Comparison Metric Key",
      "Source Comparison Metric Value",
      "Hub Comparison Metric Value"
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


sanitizeSQLStringInput <- function(str) {
  if(is.na(str) | is.null(str)) {
    return('NULL')
  } else {
    return(paste("'", gsub("'","''",as.character(str)), "'", sep=""))
  }
}


sqlBatchInsertResults <- function(dbconn, table, tablename, positionindex=1, insertRowLimit=500) {
  if(!is.data.table(table)) {
    return()
  } else {
    
    if(all(grepl("^[a-zA-Z0-9\\.]+$",tablename))) {
      lastRow = nrow(table)
      
      insertIntoStatment <- paste(
        'Insert Into ', tablename, ' (',
        'logTestDateTime, ',
        'logDateId, ',
        'logProgramme, ',
        'logTestName, ',
        'logPercentageDifferenceThreshold, ',
        'logDifferenceThreshold, ',
        'logDifference, ',
        'logPercentageDifference, ',
        'logTestPassed, ',
        'logComparisonMetric, ',
        'logComparisonMetricKey, ',
        'logSQLComparisonMetricValue, ',
        'logSourceComparisonMetricValue ',
        ') Values ',
        sep=""
      )
      
      for(i in positionindex:min(insertRowLimit + positionindex - 1,lastRow)) {
        if(i != positionindex) {
          insertIntoStatment <- paste(insertIntoStatment, ', ', sep="")
        }
        insertIntoStatment <- paste(insertIntoStatment,
                                    '(',
                                    sanitizeSQLStringInput(table$logTestDateTime[i]),                   ",",
                                    sanitizeSQLStringInput(table$logDateId[i]),                         ",",
                                    sanitizeSQLStringInput(table$logProgramme[i]),                      ",",
                                    sanitizeSQLStringInput(table$logTestName[i]),                       ",",
                                    sanitizeSQLStringInput(table$logPercentageDifferenceThreshold[i]),  ",",
                                    sanitizeSQLStringInput(table$logDifferenceThreshold[i]),            ",",
                                    sanitizeSQLStringInput(table$logDifference[i]),                     ",",
                                    sanitizeSQLStringInput(table$logPercentageDifference[i]),           ",",
                                    sanitizeSQLStringInput(table$logTestPassed[i]),                     ",",
                                    sanitizeSQLStringInput(table$logComparisonMetric[i]),               ",",
                                    sanitizeSQLStringInput(table$logComparisonMetricKey[i]),            ",",
                                    sanitizeSQLStringInput(table$logSQLComparisonMetricValue[i]),       ",",
                                    sanitizeSQLStringInput(table$logSourceComparisonMetricValue[i]),
                                    ')',
                                    sep=""
        )
      }
      
      sqlQuery(dbconn, insertIntoStatment)
      # print(insertIntoStatment)
      
      if(lastRow > insertRowLimit + positionindex-1) {
        sqlBatchInsertResults(dbconn, table, tablename, insertRowLimit + positionindex)
      }
    }
  }
}



sqlwriteTestResult <- function(
  programme,
  testName,
  threshold,
  minimumThreshold,
  comparisonMetric,
  result
) {
  
  if( ! is.data.table(result) ) {
    
    logToFile(programme, testName, 'Error', 'Writing Error: Output is not in data.table format - check package run through with input data')
    
  } else {
    
    connectionString <- outputPath
    tablename <- outputFileName
    dateTime <- format(runStartTime,"%Y-%m-%d %H:%M:%OS")
    logdateid <- format(runStartTime,"%Y%m%d")
    
    # temptablename <- paste('#TMPHubby',format(runStartTime, "%Y%m%d%H%M%OS"), sep="")
    
    # add static row values
    result[,'logTestDateTime' := dateTime]
    result[,'logDateId' := logdateid]
    result[,'logProgramme' := programme]
    result[,'logTestName' := testName]
    result[,'logDifferenceThreshold' := threshold]
    result[,'logPercentageDifferenceThreshold' := minimumThreshold]
    result[,'logComparisonMetric' := comparisonMetric]
    
    # change names to match the SQL columns
    colnames(result)[which(colnames(result) == 'Percentage Difference')] <- "logPercentageDifference"
    colnames(result)[which(colnames(result) == 'Difference')] <- "logDifference"
    colnames(result)[which(colnames(result) == 'Test Passed?')] <- "logTestPassed"
    colnames(result)[which(colnames(result) == "Comparison Metric Key")] <- "logComparisonMetricKey"
    
    # looks like these are labelled in reverse within the DB.
    colnames(result)[which(colnames(result) == "Hub Comparison Metric Value")] <- "logSourceComparisonMetricValue"
    colnames(result)[which(colnames(result) == "Source Comparison Metric Value")] <- "logSQLComparisonMetricValue"
    
    # output column order
    testcols <- c(
      "logTestDateTime",
      "logDateId",
      "logProgramme",
      "logTestName",
      "logPercentageDifferenceThreshold",
      "logDifferenceThreshold",
      "logDifference",
      "logPercentageDifference",
      "logTestPassed",
      "logComparisonMetric",
      "logComparisonMetricKey",
      "logSQLComparisonMetricValue",
      "logSourceComparisonMetricValue"
    )
    
    # remove uneeded columns
    result <- result[,..testcols]
    
    # open up the db connection
    db <- odbcDriverConnect(paste("driver={SQL Server};", connectionString, sep=""))
    
    # shove the test data on the end of the table
    #    sqlSave(db, result, tablename=tablename, rownames=FALSE, append=TRUE, safer=TRUE, test=TRUE, verbose=TRUE)
    sqlBatchInsertResults( db, result, tablename)
    
    # close up the connection
    odbcClose( db )
    
  }
  
  return( 'SUCCESS' )
}





logToFile <- function(
  programme,
  testName,
  status,
  message
) {
  
  
  
  filePath <- paste(logFilePath, 'HubTestLog.txt', sep="")#, format(runStartTime, "%Y-%m-%dT%H-%M"), '.txt', sep='')
  
  if(! file.exists(filePath) ) {
    file.create(filePath, showWarnings=FALSE)
  }
  
  
  message <- paste(
    # ...output data here...
    format(Sys.time(), "%Y-%m-%dT%X"),
    programme, 
    testName,
    status,
    message,
    sep="  -  "
  )
  
  if(verbose) {
    print(message)
  }
  
  
  cat( 
    message,
    file=filePath,
    sep="\r\n",
    append=TRUE
  )
  
}




# take a argument list of vectors - coalesce the NA's.
# ie return a list of the same length where the first available non NA
# vlaue has been put in
coalesce2 <- function(...) {
  Reduce(function(x, y) {
    i <- which(is.na(x))
    x[i] <- y[i]
    x},
    list(...))
}



# Checks if a column is in a listed date format
# use with lapply
# All or nothing (all vlaues are dates or no values are dates)
convertDates <- function(inputData, formats=NA, programme, testName, hubOrSource) {
  
  
  # checks to see if the inputData is all in an excel date format (eg '42568') and converts it
  if (all(grepl('^[0-9]{5}$',inputData))) {
    inputData <- do.call(c,lapply(as.numeric(inputData),excel_numeric_to_date))
  }
  
  # this is a POSIX date field already - nothing to see here.
  if(any(class(inputData) == "POSIXt")) {
    return( format(as.Date(inputData), format=coercedDateFormat) )
  }
  
  
  posixed <- format(as.Date(rep(NA,length(inputData))), format=coercedDateFormat)
  
  
  # try the format split if its there
  if( !is.na(formats) & is.character(formats) ) {
    formatSplit <- strsplit(formats, ",")
    formatSplit <- do.call(rbind, formatSplit)
    
    for(i in 1:length(formatSplit)) {
      posixed <- coalesce2(posixed, format(as.Date( as.character(inputData), format=formatSplit[i]), format=coercedDateFormat) )
      
      if(!any(is.na(posixed))) {
        return( posixed )
      } else if(any(!is.na(posixed))) {
        logToFile(programme,testName,"Error",paste(hubOrSource ,' file has dates which do not match the specified date formats.', sep=""))
      }
    }
  }
  
  
  posixed <- coalesce2(posixed, tryCatch({
    return( format(as.Date(as.POSIXlt( as.character(inputData) )), format=coercedDateFormat) )
  }, error = function (err){ 
    return( format(as.Date(rep(NA,length(inputData))), format=coercedDateFormat) )
  }));
  
  
  # check if its all date formatted
  if(!any(is.na( posixed ))) {
    return( posixed )
  }
  
  
  # no formats specified or no formats matched - just return the data.
  return ( inputData )
  
  
}






getFTPFileList <- function(
  url, 
  usercreds
) {
  # log on to ftp and get file list
  ftpFL <- getURL(url, userpwd = usercreds, ftp.use.epsv = FALSE)
  
  ## ftp getURL on a folder returns a string of file info as below:
  # "-rw-r--r--    1 ftp      ftp          4259 May 07  2015 README.TXT\r\n-rw-r--r--    1 ftp      ftp            35 Mar 20 14:45 test.TXT"
  ## so the below process it into a data frame which can be sorted and filtered.
  
  if(length(grep('\r\n',ftpFL)) > 0) {
    ftpFL <- unlist(strsplit(ftpFL, '\r\n'))
  } else {
    ftpFL <- unlist(strsplit(ftpFL, '\n'))
  }
  
  
  # filter out directory names
  ftpFL <- grep("^d", ftpFL, value=TRUE, invert=TRUE)
  # filter out . and .. directory
  ftpFL <- grep("\\s+\\.?\\.$", ftpFL, value=TRUE, invert=TRUE)
  
  if(length(ftpFL) > 0) {
    # data frame it!
    ftpFL <- data.frame(matrix(unlist(strsplit(ftpFL,'\\s+')), ncol=9, byrow=TRUE))
    
    # give columns logical names
    names(ftpFL) <- c('permissions','noFiles','owner', 'group', 'size', 'uploadMonth', 'uploadDay', 'uploadYH','name')
    
    # get rid of the ones we don't need
    ftpFL$size <- NULL
    ftpFL$permissions <- NULL
    ftpFL$noFiles <- NULL
    ftpFL$owner <- NULL
    ftpFL$gorup <- NULL
    
    # add in a couple to sort out time.
    ftpFL$uploadTime <- '00:00'
    ftpFL$uploadYear <- format(runStartTime-(60*60*24),"%Y")
    
    # change year/time into seperate columns
    ftpFL$uploadTime[grep('^\\d\\d:\\d\\d$',ftpFL$uploadYH)] <- grep('^\\d\\d:\\d\\d$',ftpFL$uploadYH, value=TRUE)
    ftpFL$uploadYear[grep('^\\d\\d\\d\\d$',ftpFL$uploadYH)] <- grep('^\\d\\d\\d\\d$',ftpFL$uploadYH, value=TRUE)
    
    # create datetime filed to order by
    ftpFL$date <- strptime(paste(ftpFL$uploadYear, ftpFL$uploadMonth, ftpFL$uploadDay, ftpFL$uploadTime, sep=" "), '%Y %b %d %H:%M')
    
    # get rid of useless fields
    ftpFL$uploadMonth <- NULL
    ftpFL$uploadDay <- NULL
    ftpFL$uploadTime <- NULL
    ftpFL$uploadYear <- NULL
    ftpFL$uploadYH <- NULL
    
    return( ftpFL )
    
  } else {
    
    return( ftpFL )
    
  }
  
}





getExcelFile <- function(filepath, skip=0, castDates=FALSE, headerValues=FALSE,sheet=1) {
  
  skip <- ifelse(is.logical(skip) & ! skip,0, skip)
  
  if(is.na(sheet)) {
    sheet <- 1;
  }
  
  # read excel data file
  # read it in once to get the number of columns (this is ridiculous there must be a better way)
  excelData <- read_excel(filepath,col_names=FALSE,skip=skip,sheet=sheet)
  
  # read it in again but set all the columns to text
  textRep <- rep("text",ncol(excelData))
  
  if(castDates == TRUE) {
    textRep[which(sapply(excelData,function(x) inherits(x, 'POSIXt' )))] <- 'date'
  }
  
  excelData <- read_excel(filepath, col_types=textRep, na="", col_names=FALSE, skip=skip, sheet=sheet)
  
  # if(any(sapply(excelData,function(x) inherits(x, 'POSIXt' )))) {
  #     excelData[,which(sapply(excelData,function(x) inherits(x, 'POSIXt' )))] <- sapply(excelData[,which(sapply(excelData,function(x) inherits(x, 'POSIXt' )))], function(x) {as.character(x)})
  # }
  
  
  
  
  return( excelData )
  
}



getCSVFile <- function(filepath, skip=FALSE, header=FALSE) {
  
  #read csv data file
  #return( read.csv(filepath,stringsAsFactors=FALSE,header=header,skip=skip,fill=TRUE) )
  
  #readlines
  lines <- readLines(filepath,n=30)
  maxColumns <- max(str_count(lines,","))
  
  colnames <- c("V1")
  
  if(maxColumns > 1) {
    for(i in 1:maxColumns) {
      colnames <- c(colnames, paste("V", i+1,sep=""))
    }
  }
  
  csvData <- read.csv(filepath,stringsAsFactors=FALSE,header=FALSE,skip=skip,fill=TRUE,col.names=colnames)
  
  # Strip out the utf-8 BOM
  csvData[1,1] <- gsub("^\\ï\\»\\¿", "", csvData[1,1])
  
  return( csvData )
  
}



getLocalData <- function(programme, testName, hubOrSource, filepath, filename, headerRow, endRow, headerValues, sheet,expectedDateFormats) {
  
  
  # default HubEndrow vlaue if empty character is passed
  if((is.character(endRow) & endRow == '') | is.na(endRow)) {
    endRow <- -1
  }
  
  if((is.character(headerRow) & headerRow == '') | is.na(headerRow)) {
    if(headerRow == "" | is.na(headerRow)) {
      headerRow <- 1
    }
  }
  
  # check file exists
  if( length(list.files(path=filepath, pattern=filename, full.names=TRUE)) == 0) {
    logToFile(programme,testName, "Error",paste("Initialisation Error: ",hubOrSource," Data file does not exist, check the File Path and File Name", sep=""))
    return(  )
  }
  
  # all files that match the filename regex within the folder
  availableDataFiles <- file.info(list.files(path=filepath, pattern=filename, full.names=TRUE))
  # the most recent file
  dataLocation <- rownames(availableDataFiles[rev(order(as.Date(availableDataFiles$mtime))),][1,])
  
  
  
  # cast HubEndRow to number (if its a text identifier this will be checked)
  endRowNumber <- suppressWarnings(as.numeric(endRow))
  headerRowNumber <- suppressWarnings(as.numeric(headerRow))
  
  headerValues <- suppressWarnings(as.logical(headerValues))
  
  skipRows <- FALSE
  
  rowBeforeHeader <- 0
  
  # If the user passed a number - then the head row is in the row specified
  # skip all rows prior to the header row (e.g row 3 specified then skip 2 rows (1 & 2)
  # end orw number specified by user then needs to be 2 rows less as the final read data won't include 1 & 2.
  if(!is.na(headerRowNumber) & headerRowNumber > 1 ) { 
    rowBeforeHeader <- headerRowNumber - 1
    skipRows <- rowBeforeHeader +1
    if(!is.na(endRowNumber)) {
      endRowNumber <- endRowNumber - rowBeforeHeader
    }
    headerRowNumber <- 1
  }
  
  removeHeaderRow <- TRUE
  
  
  
  
  # get data file
  if(grepl('\\.csv$',filename)) {
    
    #read csv data file, chop off the top of the file if necesary
    localData <- getCSVFile(dataLocation,skip=skipRows,header=headerValues)
    
  } else if(grepl('\\.xlsx?$',filename)) {
    
    # read excel data file
    localData <- suppressWarnings(getExcelFile(dataLocation,skip=skipRows,headerValues=headerValues,sheet=sheet))
    # the read_excel library automatically takes the header row out if its all lined up right
  }
  
  
  # endRow is a number
  if(!is.na(endRowNumber)) {
    # if the endRow is -1 just use the whole file.
    if(endRowNumber < 0) {
      endRow <- nrow(localData[,1])
      if(is.null(endRow)) {
        # nrow sometimes returns null occasionally instead of the length.
        endRow <- length(localData[,1])
      }
    } else {
      endRow <- endRowNumber
    }
  } 
  # its a string row identfier instead of a row number
  else {
    endRowNumber <- which(localData[,1] == endRow) -1
    if(length(endRowNumber) == 0) {
      logToFile(programme,testName,"Error",paste(hubOrSource ,' End Row identifier "', endRow, '" is not present in column 1 of the source data file. Check correct file/identifier has been used', sep=""))
      return(   )
    } else {
      endRow <- endRowNumber
    }
  }
  
  # print(nrow(localData[,1]))
  # print(endRow)
  # print(endRowNumber)
  
  #take the top off the file up to the header row
  if(is.na(headerRowNumber)) {
    # headerRow is not a number
    if(!(headerRow %in% names(localData))) {
      # pull the row number where the identifier appears
      
      rowBeforeHeader <- which(localData[,1] == headerRow) - 1
      if(length(rowBeforeHeader) == 0) {
        logToFile(programme,testName,"Error",paste(hubOrSource ,' Header Row identifier "', headerRow, '" is not present in column 1 of the data file. Check correct file/identifier has been used', sep=""))
        return(   )
      } else {
        headerRow <- 1
        endRow <- endRow - rowBeforeHeader
      }
    } else {
      rowBeforeHeader <- 0
      headerRow <- 1
    }
  } else {
    headerRow <- ifelse(headerRowNumber > 0, headerRowNumber, 0)
  }
  
  
  # validate header/endRow locations (dont want them the wrong way round)
  if(headerRow >= endRow) {
    logToFile(programme,testName,"Error",paste("Initialisation Error: ", hubOrSource ," endRow must be after headerRow",sep=""))
    return (  )
  }
  
  
  if(grepl('\\.xlsx?$',filename)) {
    
    skipRows <- rowBeforeHeader
    
    # read excel data file again so that dates are cast properly.
    localData <- suppressWarnings(getExcelFile(dataLocation, skip=skipRows, castDates=TRUE,headerValues=headerValues,sheet=sheet))
    
    #removeHeaderRow <- TRUE
  } else if( grepl('\\.csv$',filename) & any(rowBeforeHeader > 1) & any(skipRows == FALSE)) {
    
    headerRow <- rowBeforeHeader + 1
    endRow <- endRow + rowBeforeHeader
    
  }
  
  
  # strip off excess rows
  # data may have multiple uneeded rows - so strip off any that aren't part of the test
  localData <- localData[headerRow:endRow,]
  
  # remove header line for CSV files
  if(removeHeaderRow) {
    # take column names row off now that its not needed
    names(localData) <- localData[1,]
    # remove the headers row
    localData <- localData[-1,]
  }
  
  #format dates in header if necessary
  if(isTRUE(headerValues)) {
    for(i in 2:ncol(localData)) {
      colnames(localData)[i] = convertDates(colnames(localData)[i],expectedDateFormats,programme,testName,hubOrSource)
      #if (!grepl('/',colnames(localData)[i])) { #coerce excel dates if they exist
      #colnames(localData)[i] <- gsub("(^|[^0-9])0+", "\\1", format(as.Date(as.Date('1900-01-01')+as.numeric(colnames(localData)[i])-2), "X%m.%d.%Y"), perl = TRUE)
      #}
    }
  }
  
  
  
  # log filename used
  logToFile(programme,testName, "Processing",paste('Used file ', dataLocation, ' for ',  hubOrSource, ' Data', sep=""))
  
  return( localData )
  
  
}



getFTPData <- function(programme, testName, hubOrSource, filepath, filename, headerRow, endRow, headerValues) {
  url <- ''
  # if its already in tempfiles then no wories mate!
  if(paste(filepath, filename, sep="") %in% tempFiles) {
    
    filepath <- tempFilesPath
    
  } else {
    
    # expecting ftp connection string as follows:
    # ftp://host/pathanme,name:password
    url <- strsplit(filepath, ",") 
    url <- do.call(rbind, url)
    usercreds <- url[1,2]
    url <- url[1,1]
    
    # get the list of ftp files to check for the file
    ftpFL <- getFTPFileList(url, usercreds)
    
    if( length(ftpFL) == 0 ) {
      logToFile(programme,testName,"Error",paste( hubOrSource, ' Data file not found on the FTP, check ftp location string is correct',filename))
      return(  )
    }
    
    # find the file name
    fileNameMatch <- grep(filename, ftpFL$name)
    
    # if its there
    if(length(fileNameMatch) > 0) {
      ftpFL <- ftpFL[fileNameMatch,]
      mostRecentMatchingFile <- ftpFL$name[rev(order(ftpFL$date))][1]
      
      # try to create a new file int he tmeporary folder
      if(file.create(paste(tempFilesPath,mostRecentMatchingFile,sep=""), showWarnings=FALSE)) {
        # dump the downloaded files content in the new file
        cat(getURL(paste(url,mostRecentMatchingFile,sep=""), userpwd = usercreds, ftp.use.epsv = FALSE), file=paste(tempFilesPath,mostRecentMatchingFile,sep=""))
        
        # add the new file to the tempfiles list - in case another test uses it
        assign('tempFiles', c(tempFiles, paste(filepath, mostRecentMatchingFile, sep="")), envir= .GlobalEnv)
        assign('cleanupList', c(cleanupList, paste(tempFilesPath, mostRecentMatchingFile, sep="")), envir= .GlobalEnv)
        
        # change the folder path to the new tempfile and carry on as normal
        filepath <- tempFilesPath
        
      } else {
        # couldn't create the file
        logToFile(programme,testName,"Error",paste('Could not download ',hubOrSource,' Data file to tempFilesPath - check permissions on ', tempFilesPath, sep=""))
        return(  )
      }
    } else {
      # couldnt find the reuqired source data file
      logToFile(programme,testName,"Error", paste(hubOrSource,' Data file not found on the FTP', sep=""))
      return(  )
    }
    
    # dump the uneeded params
    usercreds <- NULL
    url <- NULL
    ftpFL <- NULL
    fileNameMatch <- NULL
    
  }
  
  ftpData <- getLocalData( programme, testName, hubOrSource, filepath, filename, headerRow, endRow, headerValues, sheet,expectedDateFormats )
  
  if(is.null(ftpData)) {
    logToFile(programme,testName,"Error", paste(hubOrSource,' The file was downloaded but can no longer be accessed - please check the permissions on the temp folder: ', tempFilesPath, sep=""))
    return(  )
  } else {
    
    return( ftpData )
  }
}



getSQLData <- function(programme, testName, hubOrSource, connectionString, queryFilePath) {
  SQLQuery <- ''
  
  # if no name is entered - then check for {testName}.sql in the sql path directory
  if(is.na(queryFilePath)) {
    queryFilePath <- paste(sqlPath, testName, '.sql', sep="")
    if(! file.exists(queryFilePath)) {
      logToFile(programme,testName, "Error",paste('Initialisation Error: ',hubOrSource,'SQL file does not exist, make sure the SQL script is named "', testName, '.sql" and it is in ', sqlPath, sep=""))
      return(  )
    } else {
      SQLQuery <- readChar(queryFilePath, file.info(queryFilePath)$size)
    }
  } else if(file.exists(queryFilePath)) {
    SQLQuery <- readChar(queryFilePath, file.info(queryFilePath)$size)
  } else {
    logToFile(programme,testName, "Error",paste('Initialisation Error: ', hubOrSource,' SQL Query File Path passed is not a valid file path! Check file path is correct. ',queryFilePath,sep=""))
    return(  )
  }
  
  
  ##### TODO: Try catches round this
  # acquire SQL Datas
  db <- odbcDriverConnect(paste('driver={SQL Server};',connectionString,sep=""))
  sqlData <- sqlQuery( db, SQLQuery, stringsAsFactors=FALSE )
  odbcClose( db )
  
  return( sqlData )
}



getData <- function( programme, testName, hubOrSource, connectionPathString, fileName,  columnsToInclude, headerRow, endRow, headerValues, expectedDateFormats, sheet) {
  
  if(is.na(headerValues)) {
    headerValues <- FALSE
  }
  
  # Detect what data we're looking at
  if(grepl('^s?ftp://', connectionPathString)) {
    # file is in an ftp location, download it and process it
    retrievedData <- getFTPData(programme, testName, hubOrSource, connectionPathString, fileName, headerRow, endRow)
    
  } else if ( grepl("((server|database|uid|pwd)=[^;]+(;\\s*)?){4}", connectionPathString) ) {
    # matches the server connection query string format
    retrievedData <- getSQLData(programme, testName, hubOrSource, connectionString=connectionPathString, queryFilePath=fileName)
    
  } else {
    # dunno what it is try open it as a file
    retrievedData <- getLocalData(programme, testName, hubOrSource, connectionPathString, fileName, headerRow, endRow, headerValues, sheet,expectedDateFormats)
    
  }
  
  if(is.null(retrievedData)) {
    # no datas returned
    return (  )
  }
  
  
  
  # if columns to include is empty then include all rows
  if(! is.na(columnsToInclude) & columnsToInclude != '') {
    columnsToIncludeProcessed <- strsplit(columnsToInclude, ",") 
    columnsToIncludeProcessed <-  do.call(rbind, columnsToIncludeProcessed)
    columnsToIncludeProcessed <- suppressWarnings(as.numeric(columnsToIncludeProcessed[1,]))
    
    
    if(any(is.na(columnsToIncludeProcessed))) {
      logToFile(programme,testName,"Error",paste("Initialisation Error: ", hubOrSource ,"ColumnsToInclude must be column numbers seperated by commas, a value has been used that is not a number: ", columnsToInclude, sep=""))
      return(  )
    }
    
    if(any(columnsToIncludeProcessed > length(retrievedData[1,]))) {
      logToFile(programme,testName, "Error",paste("Initialisation Error: ", hubOrSource ,"ColumnsToInclude specifies column numbers that don't exist", sep=""))
      return(  )
    } else {
      retrievedData <- retrievedData[,columnsToIncludeProcessed]
    }
  }
  
  
  # check if any columns can be converted to dates
  retrievedData <- sapply(retrievedData, convertDates, formats=expectedDateFormats ,programme,testName,hubOrSource)
  
  # print("")
  # print("Final retrievedData")
  # print("")
  # print(head(retrievedData))
  # print(tail(retrievedData))
  # print("")
  
  
  return( retrievedData )
}





testDatas <- function(
  
  programme,
  testName,
  comparisonMetric,
  threshold=3,
  minimumThreshold=100,
  ignoredValues="",
  expectedDateFormats='',
  
  SourceDataPath,
  SourceDataFileName,
  SourceColumnsToInclude="",
  SourceHeaderRow=1,
  SourceEndRow=-1,
  SourceHeaderValues=FALSE,
  SourceSheet=1,
  
  HubDataPath,
  HubDataFileName,
  HubColumnsToInclude="",
  HubHeaderRow=1,
  HubEndRow=-1,
  HubHeaderValues=FALSE,
  HubSheet=1
  
) {
  
  
  
  #####
  ##
  ## CHECK VALUES PASSED TO FUNCTION
  ##
  ####
  
  
  # exit if the programme value is not INTL or US.
  if( !( programme == "INTL" || programme == "US" ) ) {
    logToFile(programme,testName, "Error","Initialisation Error: programme must be INTL or US")
    return( )
  }
  
  # Cast fields to the types we want them in
  # theres something weird with tibbles in newer versions of R so just ot be sure cast everything...
  comparisonMetric <- as.character(comparisonMetric)
  expectedDateFormats  <- as.character(expectedDateFormats)
  SourceDataPath  <- as.character(SourceDataPath)
  SourceDataFileName  <- as.character(SourceDataFileName)
  SourceColumnsToInclude  <- as.character(SourceColumnsToInclude)
  HubDataPath  <- as.character(HubDataPath)
  HubDataFileName  <- as.character(HubDataFileName)
  HubColumnsToInclude  <- as.character(HubColumnsToInclude)
  SourceHeaderRow  <- as.character(SourceHeaderRow)
  SourceEndRow  <- as.character(SourceEndRow)
  HubHeaderRow  <- as.character(HubHeaderRow)
  HubEndRow  <- as.character(HubEndRow)
  #SourceSheet <- as.character(SourceSheet)
  #HubSheet <- as.character(HubSheet)
  
  if (!is.na(SourceSheet)) {
    if (!is.na(as.numeric(SourceSheet))) {
      SourceSheet <- as.numeric(SourceSheet)
    } else {
      SourceSheet <- as.character(SourceSheet)
    }
  }
  
  if (!is.na(HubSheet)) {
    if (!is.na(as.numeric(HubSheet))) {
      HubSheet <- as.numeric(HubSheet)
    } else {
      HubSheet <- as.character(HubSheet)
    }
  }
  
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
  
  
  
  
  
  #####A
  ##
  ##   GET DATA
  ##
  #####
  
  
  # print("")
  # print('-------------Source----------------')
  
  # Get source data
  sourceData <- getData(programme, testName, 'Source', SourceDataPath, SourceDataFileName, SourceColumnsToInclude, SourceHeaderRow, SourceEndRow, SourceHeaderValues, expectedDateFormats, SourceSheet)
  
  if(is.null(sourceData)) {
    # source data didn't come home
    return(  )
  }
  
  
  # print("")
  # print('-------------Hub----------------')
  
  # Get Hub data
  hubData <- getData(programme, testName, 'Hub', HubDataPath, HubDataFileName, HubColumnsToInclude, HubHeaderRow, HubEndRow, HubHeaderValues,  expectedDateFormats, HubSheet)
  
  if(is.null(hubData)) {
    # theres no hub data there...
    return(  )
  }
  
  
  ## GG we got datas
  
  # perform sanity checks on source and hub data before attempting to join
  if(length(sourceData[1,]) != length(hubData[1,])) {
    if(length(sourceData[1,]) > length(hubData[1,])) {
      more <- 'more'
    } else {
      more <- 'less'
    }
    
    logToFile(programme,testName, "Error",paste("Initialisation Error: Source Data returns ",more,' columns than the Hub Data, please check columns match', sep=""))
    return(  )
  } else if (!any(colnames(hubData) == colnames(hubData))) {
    logToFile(programme,testName, "Error","Initialisation Error: column names and number of columns must match exactly between Source Data and Hub Data")
    return(  )
  }
  
  
  # cast data to data.tables
  sourceData <- data.table(sourceData)
  hubData <- data.table(hubData)
  
  
  
  
  
  #####
  ##
  ##   SANITIZE DATA
  ##
  #####
  
  
  SourceHeaderValues <- suppressWarnings(as.logical(SourceHeaderValues))
  HubHeaderValues <- suppressWarnings(as.logical(SourceHeaderValues))
  
  # pivot matrices (have headerValues set to true)
  if(isTRUE(SourceHeaderValues)) {
    sourceData <- melt(sourceData, id=c(colnames(sourceData)[1]))
    colnames(sourceData)[3] = comparisonMetric
    sourceData$variable <- gsub('[.]','-',sourceData$variable)
  }
  if(isTRUE(HubHeaderValues)) {
    hubData <- melt(hubData, id=c(colnames(hubData)[1]))
    colnames(hubData)[3] = comparisonMetric
    hubData$variable <- gsub('[.]','-',hubData$variable)
  }
  
  
  # cast as numeric
  
  if(!(comparisonMetric %in% colnames(sourceData))) {
    logToFile(programme,testName, "Error","Initialisation Error: comparison column not found in source data file.")
    
    # print(head(sourceData))
    
    return(  )
  }
  if(!(comparisonMetric %in% colnames(hubData))) {
    logToFile(programme,testName, "Error","Initialisation Error: comparison column not found in Hub data file.")
    return(  )
  }
  
  
  sourceData[[comparisonMetric]] <- suppressWarnings(as.numeric(gsub("[^0-9\\.\\, a-fA-F\\-]","",sourceData[[comparisonMetric]])))
  hubData[[comparisonMetric]] <- suppressWarnings(as.numeric(gsub("[^0-9\\.\\, a-fA-f\\-]","",hubData[[comparisonMetric]])))
  
  
  # select all columns that are not the comparison column
  nonComparisonCols <- colnames(hubData)[which(colnames(hubData) != comparisonMetric)]
  
  
  # sum up duplicated rows
  sourceData <- sourceData[,lapply(.SD, sum), by=nonComparisonCols]
  hubData <- hubData[,lapply(.SD, sum), by=nonComparisonCols]
  
  
  
  
  
  #####
  ##
  ##   JOIN DATA SOURCES
  ##
  ###
  
  
  # left join sourceData to hubData - i.e for all columns of sourceData join values of hubData or NA if it is not matched
  sourceData <- hubData[sourceData, on=nonComparisonCols]
  
  
  # clear the hubData out of memory we dont need it anymore
  hubData <- NULL
  
  
  
  
  
  #####
  ##
  ##   PERFORM TESTS
  ##
  #####
  
  # create the Comparison Metric Key column
  sourceData[,'Comparison Metric Key':=' ']
  
  # join all nonComparisonCols using pipes into the Comparison Metric Key value
  for (i in 1:nrow(sourceData)) {
    set(sourceData, i, 'Comparison Metric Key', paste(sourceData[i,..nonComparisonCols],collapse="|"))
  }
  
  # set NA's to 0 (indicates missing datas on hubData side)
  set(sourceData,which(is.na(sourceData[[comparisonMetric]])),comparisonMetric,0)
  
  # work out the difference (join creates a column named i.ColumnNameOfJoin which contains the sourceData values, comparisonMetric column actually contains hubData)
  sourceData$ValueDiff <- (sourceData[[paste("i.",comparisonMetric,sep="")]] - sourceData[[comparisonMetric]])
  
  # work out diff as percentage
  sourceData$PercentageDiff <- (sourceData$ValueDiff/sourceData[[paste("i.",comparisonMetric,sep="")]] )*100
  # set any NaN to 0
  sourceData$PercentageDiff[is.nan(sourceData$PercentageDiff)] <- 0
  
  # round values
  sourceData$ValueDiff <- round(sourceData$ValueDiff, 4)
  sourceData$PercentageDiff <- round(sourceData$PercentageDiff,2)
  
  # change column names to Hub Comparison Metric Value and Source Comparison Metric Value
  colnames(sourceData)[which(colnames(sourceData) == comparisonMetric)] <- "Hub Comparison Metric Value"
  colnames(sourceData)[which(colnames(sourceData) == paste("i.",comparisonMetric,sep=""))] <- "Source Comparison Metric Value"
  
  #KeyCol <- sourceData[,paste(.SD, collapse="|"), by=c(comparisonMetric)]
  
  
  # Set test Pass/Fail result to TRUE/FALSE
  sourceData[,PercentageDiffPass:= !( (PercentageDiff > threshold & ValueDiff > minimumThreshold) | (PercentageDiff < (threshold*-1) & ValueDiff < (-1*minimumThreshold)) )]
  
  # Cast Boolean column to character so we can add "IGNORED" values
  sourceData$PercentageDiffPass <- as.numeric(sourceData[,PercentageDiffPass])
  
  # Set ignored values to IGNORED instead of TRUE/FALSE. 2 = INGORED  
  if(!any(is.na(ignoredValues))) {
    for(i in 1:nrow(sourceData)) {
      if(length(intersect(ignoredValues, as.matrix(sourceData[i,..nonComparisonCols]))) > 0) {
        sourceData[i,PercentageDiffPass := 2]
      }
    }
  }
  
  
  
  
  
  #####
  ##
  ##   OUTPUT
  ##
  #####
  
  # Give columns human readable names rather than shorthand.
  colnames(sourceData)[which(colnames(sourceData) == 'PercentageDiff')] <- "Percentage Difference"
  colnames(sourceData)[which(colnames(sourceData) == 'ValueDiff')] <- "Difference"
  colnames(sourceData)[which(colnames(sourceData) == 'PercentageDiffPass')] <- "Test Passed?"
  
  
  if(outputType == 'File') {
    
    # log a complete message
    logToFile(programme,testName, "Success","Test Completed - writing to file...")
    
    # write test result to output file
    writeResultStatus <- writeTestResult(
      programme,
      testName,
      threshold,
      minimumThreshold,
      comparisonMetric,
      sourceData
    )
  } else if (outputType == 'SQL') {
    
    
    # log a complete message
    logToFile(programme,testName, "Success","Test Completed - inserting into SQL table")
    
    # write test result to output file
    writeResultStatus <- sqlwriteTestResult(
      programme,
      testName,
      threshold,
      minimumThreshold,
      comparisonMetric,
      sourceData
    )
  } else {
    
    # log a complete message
    logToFile(programme,testName, "Error",'Could not write results, unrecognised output type. Ensure the value is either "File" or "SQL"')
  }
  
  if( !is.null(writeResultStatus) ) {
    logToFile(programme,testName, "Success","Test Completed - output file additions complete")
  } else {
    logToFile(programme,testName,"Error",'Test Completed - Writing results failed')
  }
  
  
  return( writeResultStatus )
  
}



formatIgnoredValues <- function(valueString) {
  
  ignoredValues <- NA
  
  if(! is.na(valueString)) {
    ignoredValues <- strsplit(as.character(valueString), ",") 
    ignoredValues <-  do.call(rbind, ignoredValues)
    
    # take lookback from end of ignoredValues, i.e. '-1' is the day before yesterday
    lookbackDays <- str_match(ignoredValues,'^YESTERDAY[\\(]?[^\\)]*[\\)]?-?([0-9]+)$')[,2]
    lookbackDays[is.na(lookbackDays)] <- 0
    lookbackDays <- as.numeric(lookbackDays) + 1
    ignoredValues <- format(runStartTime-(60*60*24*lookbackDays),coercedDateFormat)
    
    # YESTERDAY(formatstring) - changes yesterdays date in format matching formatString
    # must be an R date string
    formatYesterday <- grepl('^YESTERDAY\\([^\\)]+\\)?-?[0-9]?$', ignoredValues)
    
    if(any(formatYesterday)) {
      # replace the YESTERDAY(...) with just the ...
      ignoredValues[formatYesterday] <- sub('YESTERDAY\\(([^\\)]+)\\)', '\\1', ignoredValues[formatYesterday])
      
      #use the value as a format string
      ignoredValues[formatYesterday] <- format(runStartTime-(60*60*24*lookbackDays),ignoredValues[formatYesterday])
      
      if(! format(runStartTime-(60*60*24*lookbackDays),coercedDateFormat) %in% ignoredValues) {
        ignoredValues <- c(ignoredValues, format(runStartTime-(60*60*24*lookbackDays),coercedDateFormat),gsub("(^|[^0-9])0+", "\\1", ignoredValues))
      }
    }
    
  }
  
  
  return( ignoredValues )
}



runSingleTest <- function(programme, testName, comparisonMetric, threshold, minimumThreshold, ignoredValues, expectedDateFormats, SourceDataPath, SourceDataFileName, SourceHeaderRow, SourceEndRow, SourceColumnsToInclude, SourceHeaderValues, SourceSheet, HubDataPath, HubDataFileName, HubHeaderRow, HubEndRow, HubColumnsToInclude, HubHeaderValues, HubSheet) {
  
  if(!is.na(ignoredValues)) {
    ignoredValues <- formatIgnoredValues(ignoredValues)
  }
  
  
  tryCatch({
    
    result <- testDatas(
      programme=programme,
      testName=testName,
      comparisonMetric=comparisonMetric,
      threshold=threshold,
      minimumThreshold=minimumThreshold,
      ignoredValues=ignoredValues,
      expectedDateFormats=expectedDateFormats,
      
      SourceDataPath=SourceDataPath,
      SourceDataFileName=SourceDataFileName,
      SourceHeaderRow=SourceHeaderRow,
      SourceEndRow=SourceEndRow,
      SourceColumnsToInclude=SourceColumnsToInclude,
      SourceHeaderValues=SourceHeaderValues,
      SourceSheet=SourceSheet,
      
      HubDataPath=HubDataPath,
      HubDataFileName=HubDataFileName,
      HubHeaderRow=HubHeaderRow,
      HubEndRow=HubEndRow,
      HubColumnsToInclude=HubColumnsToInclude,
      HubHeaderValues=HubHeaderValues,
      HubSheet=HubSheet
    )
    
    if(is.character(result)) {
      print( paste(programme, " - ", testName, ' ran successfully.', sep="") )
    } else {
      warning(paste('Something went wrong on: "', programme, " - ", testName, '". Check the logs for more info', sep=""))
    }
    
  }, error=function(err) {
    
    logToFile(programme,testName, "Error", err )
    warning(paste('Something went wrong on: "', programme, " - ", testName, '". Check the logs for more info', sep="")) 
    
  });
  
  
}



# checks if a string ends with a particular value.
# we're using it to check if filename arguments end with a slash
strEndsWith <- function(haystack, needle)
{
  hl <- nchar(haystack)
  nl <- nchar(needle)
  if(nl>hl)
  {
    return(F)
  } else
  {
    return(substr(haystack, hl-nl+1, hl) == needle)
  }
}

# make sure filepaths that will have file names appended all end with a forward slash.
appendForwardSlash <- function(str) {
  if(!strEndsWith(str, '/')) {
    str <- paste(str,'/',sep="")
  }
  return(str)
}


runTestsFromDefinitionSheet <- function() {
  
  # read all the tests from the Excel Sheet
  text22 <- rep("text",22)
  testList <- read_excel(testsFile, col_types=text22, na="", skip=29, col_names=FALSE, sheet=1)
  
  globalArguments <- tryCatch({
    suppressWarnings(read_excel(testsFile, na="", skip=8, col_names=FALSE, sheet=2)[3])
  }, error=function(err) {
  });
  
  if(!is.null(globalArguments) & nrow(globalArguments) > 0) {  #runtime specifics
    if(globalArguments[1,1] == "1") {
      verbose <<- 1
    }
    if(!is.na(globalArguments[3,1]) & !identical(globalArguments[3,1],'')) {
      sqlPath <<- appendForwardSlash(as.character(globalArguments[3,1]))
      
    }
    if(!is.na(globalArguments[4,1]) & !identical(globalArguments[4,1],'')) {
      logFilePath <<- appendForwardSlash(as.character(globalArguments[4,1]))
      
    }
    if(!is.na(globalArguments[5,1]) & !identical(globalArguments[5,1],'')) {
      tempFilesPath <<- appendForwardSlash(as.character(globalArguments[5,1]))
    }
    
    #Output param specifics
    if(!is.na(globalArguments[7,1]) & !identical(globalArguments[7,1],'')) {
      outputType <<- as.character(globalArguments[7,1])
    }
    if(!is.na(globalArguments[8,1]) & !identical(globalArguments[8,1],'')) {
      outputPath <<- as.character(globalArguments[8,1])
      if(!any(grep('database=', outputPath)) ) {
        outputPath <<- appendForwardSlash(outputPath)
      }
    }
    if(!is.na(globalArguments[9,1]) & !identical(globalArguments[9,1],'')) {
      outputFileName <<- as.character(globalArguments[9,1])
    }
  }
  
  # find last row of data
  if( ! is.na( which(is.na(testList[,1]))[1] ) ) {
    lastRow <- which(is.na(testList[,1]))[1] -1
  } else {
    lastRow <- nrow(testList)
  }
  
  
  
  
  # TODO: Try catches around this!
  for(i in 1:lastRow) {
    
    
    if(testList[i,1] == 'Enabled') {
      runSingleTest(
        programme=testList[i,2],
        testName=testList[i,3],
        comparisonMetric=testList[i,4],
        threshold=testList[i,5],
        minimumThreshold=testList[i,6],
        ignoredValues=testList[i,7],
        expectedDateFormats=testList[i,8],
        
        SourceDataPath=testList[i,9],
        SourceDataFileName=testList[i,10],
        SourceSheet=testList[i,11],
        SourceHeaderRow=testList[i,12],
        SourceEndRow=testList[i,13],
        SourceColumnsToInclude=testList[i,14],
        SourceHeaderValues=testList[i,15],
        
        HubDataPath=testList[i,16],
        HubDataFileName=testList[i,17],
        HubSheet=testList[i,18],
        HubHeaderRow=testList[i,19],
        HubEndRow=testList[i,20],
        HubColumnsToInclude=testList[i,21],
        HubHeaderValues=testList[i,22]
      )
    } else if (is.character(testList[i,1]) & testList[i,1] == 'Example') {
      logToFile(testList[i,1],testList[i,3], "Skipped", paste("this line in the tests file is an example and will not be run.", sep="") )
    } else {
      logToFile(testList[i,2],testList[i,3], "Skipped","Test is not Enabled")
      if(verbose) {
        print( paste('Skipped "', testList[i,2], " - ", testList[i,3], '" test is not Enabled.', sep="") )
      }
    }
    
  }
  
  
}


splitParameter <- function(arg) {
  if( !is.na(arg) & is.character(arg) ) {
    spl <- strsplit(arg, "=")
    spl <- do.call(rbind, spl)
    
    return( spl )
  }
}

pivotParamList <- function(cmdargs) {
  arguments <- c()
  lengthArgs <- length(cmdargs)
  
  for(i in 2:lengthArgs) {
    if(!any(grep('^-\\w+$',cmdargs[i]))){
      splitArg <- splitParameter(cmdargs[i])
      
      arguments[splitArg[1,1]] <- splitArg[1,2]
    }
  }
  
  return(arguments)
}


# check for command line arguments
cmdargs <- commandArgs(trailingOnly=TRUE)

# switch to verbose output if v is passed in the runtime args
if(length(cmdargs) > 0){
  
  formattedArgs <- pivotParamList(cmdargs)
  
  if( any(grep('-\\w*v', cmdargs[1])) ) {
    verbose <<- 1
  }
  
  
  # set global params if they are included as arguments
  if("DefaultSQLPath" %in% colnames(formattedArgs) & !identical(formattedArgs["DefaultSQLPath"],'')) {
    sqlPath <<- appendForwardSlash(as.character(formattedArgs["DefaultSQLPath"]))
  }
  if("DefaultLogFilePath" %in% colnames(formattedArgs) & !identical(formattedArgs["DefaultLogFilePath"], '')) {
    logFilePath <<- appendForwardSlash(as.character(formattedArgs["DefaultLogFilePath"]))
  }
  if("DefaultTempFilesPath" %in% colnames(formattedArgs) & !identical(formattedArgs["DefaultTempFilesPath"], '')) {
    tempFilesPath <<- appendForwardSlash(as.character(formattedArgs["DefaultTempFilesPath"]))
    
  }
  
  # set output parameters
  if("OutputType" %in% colnames(formattedArgs) & !identical(formattedArgs["OutputType"], '')) {
    outputType <<- as.character(formattedArgs["OutputType"])
  }
  if("OutputPath" %in% colnames(formattedArgs) & !identical(formattedArgs["OutputPath"], '')) {
    outputPath <<- as.character(formattedArgs["OutputPath"])
    if(!any(grep('database=', outputPath)) & !strEndsWith(outputPath, '/')) {
      outputPath <<- appendForwardSlash(outputPath)
    }
  }
  if("OutputFileName" %in% colnames(formattedArgs) & !identical(formattedArgs["OutputFileName"], '')) {
    outputFileName <<- as.character(formattedArgs["OutputFileName"])
  }
  
  # if there are any and the first one is -r then run a single test
  if (any(grep('-\\w*r', cmdargs[1]))) {
    # the -r param tells the script to run as a one off with the rest of the params as
    # input values
    
    if(length(cmdargs) < 7) {
      print('Please ensure you have included all required arguments to run a valid test')
    } else {
      
      runSingleTest(
        programme=formattedArgs["Programme"],
        testName=formattedArgs["TestName"],
        comparisonMetric=formattedArgs["ComparisonMetric"],
        threshold=formattedArgs["Threshold"],
        minimumThreshold=formattedArgs["MinimumThreshold"],
        ignoredValues=formattedArgs["IgnoredValues"],
        expectedDateFormats=formattedArgs["ExepectedDateFormats"],
        
        SourceDataPath=formattedArgs["SourceDataConnectionString"],
        SourceDataFileName=formattedArgs["SourceDataFileName"],
        SourceSheet=formattedArgs["SourceDataSheet"],
        SourceHeaderRow=formattedArgs["SourceDataHeaderRowIdentifier"],
        SourceEndRow=formattedArgs["SourceDataEndRowIdentifier"],
        SourceColumnsToInclude=formattedArgs["SourceDataColumnsToInclude"],
        SourceHeaderValues=formattedArgs["SourceDataHeaderValues"],
        
        HubDataPath=formattedArgs["HubDataConnectionString"],
        HubDataFileName=formattedArgs["HubDataFileName"],
        HubSheet=formattedArgs["HubDataSheet"],
        HubHeaderRow=formattedArgs["HubDataHeaderRowIdentifier"],
        HubEndRow=formattedArgs["HubDataEndRowIdentifier"],
        HubColumnsToInclude=formattedArgs["HubDataColumnsToInclude"],
        HubHeaderValues=formattedArgs["HubDataHeaderValues"]
      )
    }
  } 
} else {
  
  # Run it!
  runTestsFromDefinitionSheet()
  
}



# Cleanup any temporary files created during batch runs
if(length(cleanupList) > 0) {
  for(i in 1:length(cleanupList)) {
    if(file.exists(cleanupList[[i]])) {
      file.remove(cleanupList[[i]])
    }
  }
}
