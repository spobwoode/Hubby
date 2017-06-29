# this is the filepath where i have been building all the tests
currentPath <- 'C:/HiltonGuestShare/Hubby'
#currentPath <- getwd()
# this is the filepath to the test data file
testsFile <- paste(currentPath, '/SourceVHubDataTests.xlsx', sep="")
# This is the directory the output file will be saved to
outputPath <- '//hiltonagg01/hiltonagg01data/Box Sync/HiltonHubSanityTests/'
# The outpt filename will be constructed as {THIS FILENAME VALUE}_YYYY-MM-DD.csv, where YYYY_MM_DD is the run date
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

# empty list of temp files - this will be filled as the tests are run,
# and used to delete the temp files downloaded from external sources
tempFiles <- list()
cleanupList <- list()


# set to 1 to have the programme print everything its doing to the console as well as the log.
# set to 0 to quiet down
verbose <- 1



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
# curl library to get file names of ftp files
installPackages('RCurl')
# janitor for data cleaning funcitons
installPackages("janitor")







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
convertDates <- function(data, formats=NA) {
  
  # checks to see if the data is all in an excel date format (eg '42568') and converts it
  if (all(grepl('^[0-9]{5}$',data))) {
    data <- do.call(c,lapply(as.numeric(data),excel_numeric_to_date))
  }
  
  # this is a POSIX date field already - nothing to see here.
  if(any(class(data) == "POSIXt")) {
    return( format(as.Date(data), format=coercedDateFormat) )
  }
  
  
  posixed <- format(as.Date(rep(NA,length(data))), format=coercedDateFormat)
  
  
  # try the format split if its there
  if( !is.na(formats) & is.character(formats) ) {
    formatSplit <- strsplit(formats, ",")
    formatSplit <- do.call(rbind, formatSplit)
    
    for(i in 1:length(formatSplit)) {
      posixed <- coalesce2(posixed, format(as.Date( as.character(data), format=formatSplit[i]), format=coercedDateFormat) )
      
      if(!any(is.na(posixed))) {
        return( posixed )
      }
    }
  }
  
  
  posixed <- coalesce2(posixed, tryCatch({
    return( format(as.Date(as.POSIXlt( as.character(data) )), format=coercedDateFormat) )
  }, error = function (err){ 
    return( format(as.Date(rep(NA,length(data))), format=coercedDateFormat) )
  }));
  
  
  # check if its all date formatted
  if(!any(is.na( posixed ))) {
    return( posixed )
  }
  
  
  # no formats specified or no formats matched - just return the data.
  return ( data )
  
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





getExcelFile <- function(filepath, skip=0, castDates=FALSE, headerValues=FALSE) {
  
  # read excel data file
  # read it in once to get the number of columns (this is ridiculous there must be a better way)
  data <- read_excel(filepath,col_names=TRUE,skip=skip)
  
  
  # read it in again but set all the columns to text
  textRep <- rep("text",ncol(data))
  
  if(castDates == TRUE) {
    textRep[which(sapply(data,function(x) inherits(x, 'POSIXt' )))] <- 'date'
  }
  
  data <- read_excel(filepath, col_types=textRep, na="", col_names=TRUE, skip=skip)
  
  # if(any(sapply(data,function(x) inherits(x, 'POSIXt' )))) {
  #     data[,which(sapply(data,function(x) inherits(x, 'POSIXt' )))] <- sapply(data[,which(sapply(data,function(x) inherits(x, 'POSIXt' )))], function(x) {as.character(x)})
  # }
  
  #format dates in header if necessary
  if(isTRUE(headerValues)) {
    for(i in 2:ncol(data)) {
      colnames(data)[i] <- gsub("(^|[^0-9])0+", "\\1", format(as.Date(as.Date('1900-01-01')+as.numeric(colnames(data)[i])-2), "X%m.%d.%Y"), perl = TRUE)
    }
  }
  
  return( data )
  
}



getCSVFile <- function(filepath, skip=0, header=FALSE) {
  
  #read csv data file
  return( read.csv(filepath,stringsAsFactors=FALSE,header=header,skip=skip) )
  
}



getLocalData <- function(programme, testName, hubOrSource, filepath, filename, headerRow, endRow, headerValues) {
  
  
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
  
  skipRows <- 1
  
  if(!is.na(headerRowNumber)) { 
    skipRows <- headerRowNumber
    if(!is.na(endRowNumber)) {
      endRowNumber <- endRowNumber - headerRowNumber
    }
    headerRowNumber <- 1
  }
  
  removeHeaderRow <- TRUE
  
  # don't skip the header
  if(isTRUE(headerValues)) { 
    skipRows <- skipRows -1
    removeHeaderRow <- FALSE
  }
  
  # get data file
  if(grepl('\\.csv$',filename)) {
    
    #read csv data file, chop off the top of the file if necesary
    data <- getCSVFile(dataLocation,skip=skipRows,headerValues)
    
  } else if(grepl('\\.xlsx?$',filename)) {
    
    # read excel data file
    data <- suppressWarnings(getExcelFile(dataLocation,skip=skipRows,headerValues=headerValues))
    # the read_excel library automatically takes the header row out if its all lined up right
    removeHeaderRow <- FALSE
    
  }
  
  
  
  
  
  # endRow is a number
  if(!is.na(endRowNumber)) {
    # if the endRow is -1 just use the whole file.
    if(endRowNumber == -1) {
      endRow <- length(data[,1])
    } else {
      endRow <- endRowNumber
    }
  } 
  # its a string row identfier instead of a row number
  else {
    endRowNumber <- which(data[,1] == endRow) -1
    if(length(endRowNumber) == 0) {
      logToFile(programme,testName,"Error",paste(hubOrSource ,' End Row identifier "', endRow, '" is not present in column 1 of the source data file. Check correct file/identifier has been used', sep=""))
      return(   )
    } else {
      endRow <- endRowNumber
    }
  }
  
  
  #take the top off the file up to the header row
  if(is.na(headerRowNumber)) {
    # headerRow is not a number
    if(!(headerRow %in% names(data))) {
      # pull the row number where the identifier appears
      headerRowNumber <- which(data[,1] == headerRow)
      if(length(headerRowNumber) == 0) {
        logToFile(programme,testName,"Error",paste(hubOrSource ,' Header Row identifier "', headerRow, '" is not present in column 1 of the data file. Check correct file/identifier has been used', sep=""))
        return(   )
      } else {
        headerRow <- headerRowNumber
      }
    } else {
      headerRow <- 0
    }
  } else {
    headerRow <- ifelse(headerRowNumber > 0, headerRowNumber -1, 0)
  }
  
  
  # validate header/endRow locations (dont want them the wrong way round)
  if(headerRow > endRow) {
    logToFile(programme,testName,"Error",paste("Initialisation Error: ", hubOrSource ," endRow must be after headerRow",sep=""))
    return (  )
  }
  
  
  if(grepl('\\.xlsx?$',filename)) {
    
    endRow <- endRow - headerRow
    # read excel data file again so that dates are cast properly.
    data <- suppressWarnings(getExcelFile(dataLocation, skip=skipRows, castDates=TRUE,headerValues=headerValues))
    
  }
  
  
  # strip off excess rows
  # data may have multiple uneeded rows - so strip off any that aren't part of the test
  data <- data[headerRow:endRow,]
  
  
  # remove header line for CSV files
  if(removeHeaderRow) {
    # take column names row off now that its not needed
    names(data) <- data[1,]
    # remove the headers row
    data <- data[-1,]
  }
  
  # log filename used
  logToFile(programme,testName, "Processing",paste('Used file ', dataLocation, ' for ',  hubOrSource, ' Data', sep=""))
  
  return( data )
  
  
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
  
  data <- getLocalData( programme, testName, hubOrSource, filepath, filename, headerRow, endRow, headerValues )
  
  if(is.null(data)) {
    logToFile(programme,testName,"Error", paste(hubOrSource,' The file was downloaded but can no longer be accessed - please check the permissions on the temp folder: ', tempFilesPath, sep=""))
    return(  )
  } else {
    
    return( data )
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
  data <- sqlQuery( db, SQLQuery, stringsAsFactors=FALSE )
  odbcClose( db )
  
  return( data )
}



getData <- function( programme, testName, hubOrSource, connectionPathString, fileName,  columnsToInclude, headerRow, endRow, headerValues, expectedDateFormats) {
  
  if(is.na(headerValues)) {
    headerValues <- FALSE
  }
  
  # Detect what data we're looking at
  if(grepl('^s?ftp://', connectionPathString)) {
    # file is in an ftp location, download it and process it
    data <- getFTPData(programme, testName, hubOrSource, connectionPathString, fileName, headerRow, endRow)
    
  } else if ( grepl("((server|database|uid|pwd)=[^;]+(;\\s*)?){4}", connectionPathString) ) {
    # matches the server connection query string format
    data <- getSQLData(programme, testName, hubOrSource, connectionString=connectionPathString, queryFilePath=fileName)
    
  } else {
    # dunno what it is try open it as a file
    data <- getLocalData(programme, testName, hubOrSource, connectionPathString, fileName, headerRow, endRow, headerValues)
    
  }
  
  if(is.null(data)) {
    # no datas returned
    return (  )
  }
  
  
  # if columns to include is empty then include all rows
  if(! is.na(columnsToInclude)) {
    columnsToIncludeProcessed <- strsplit(columnsToInclude, ",") 
    columnsToIncludeProcessed <-  do.call(rbind, columnsToIncludeProcessed)
    columnsToIncludeProcessed <- suppressWarnings(as.numeric(columnsToIncludeProcessed[1,]))
    
    if(any(is.na(columnsToIncludeProcessed))) {
      logToFile(programme,testName,"Error",paste("Initialisation Error: ", hubOrSource ,"ColumnsToInclude must be column numbers seperated by commas, a value has been used that is not a number: ", columnsToInclude, sep=""))
      return(  )
    }
    
    if(any(columnsToIncludeProcessed > length(data[1,]))) {
      logToFile(programme,testName, "Error",paste("Initialisation Error: ", hubOrSource ,"ColumnsToInclude specifies column numbers that don't exist", sep=""))
      return(  )
    } else {
      data <- data[,columnsToIncludeProcessed]
    }
  }
  
  
  # check if any columns can be converted to dates
  data <- sapply(data, convertDates, formats=expectedDateFormats)
  
  
  return( data )
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
  
  HubDataPath,
  HubDataFileName,
  HubColumnsToInclude="",
  HubHeaderRow=1,
  HubEndRow=-1,
  HubHeaderValues=FALSE
  
) {
  # CHECK VALUES PASSED TO FUNCTION
  
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
  
  
  
  # Get source data
  sourceData <- getData(programme, testName, 'Source', SourceDataPath, SourceDataFileName, SourceColumnsToInclude, SourceHeaderRow, SourceEndRow, SourceHeaderValues, expectedDateFormats)
  
  if(is.null(sourceData)) {
    # source data didn't come home
    return(  )
  }
  
  
  # Get Hub data
  hubData <- getData(programme, testName, 'Hub', HubDataPath, HubDataFileName, HubColumnsToInclude, HubHeaderRow, HubEndRow, HubHeaderValues,  expectedDateFormats)
  
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
  
  
  # cast sourceData to data table
  sourceData <- data.table(sourceData)
  hubData <- data.table(hubData)
  
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
  sourceData[[comparisonMetric]] <- suppressWarnings(as.numeric(sourceData[[comparisonMetric]]))
  hubData[[comparisonMetric]] <- suppressWarnings(as.numeric(hubData[[comparisonMetric]]))
  
  
  nonComparisonCols <- colnames(hubData)[which(colnames(hubData) != comparisonMetric)]
  
  
  
  
  # left join sourceData to hubData - i.e for all columns of sourceData join values of hubData or NA if it is not matched
  sourceData <- hubData[sourceData, on=nonComparisonCols]
  # clear the hubData out of memory
  hubData <- NULL
  
  
  sourceData[,'Comparison Metric Key':=' ']
  
  for (i in 1:nrow(sourceData)) {
    set(sourceData, i, 'Comparison Metric Key', paste(sourceData[i,..nonComparisonCols],collapse="|"))
  }
  
  # set NA's to 0 (missin datas on hubData side)
  set(sourceData,which(is.na(sourceData[[comparisonMetric]])),comparisonMetric,0)
  
  # work out the difference (join creates a duplicate column named i.ColumnNameOfJoin)
  sourceData$ValueDiff <- (sourceData[[paste("i.",comparisonMetric,sep="")]] - sourceData[[comparisonMetric]])
  
  # work out diff as percentage
  sourceData$PercentageDiff <- (sourceData$ValueDiff/sourceData[[paste("i.",comparisonMetric,sep="")]] )*100
  
  # change column names to Hub Comparison Metric Value and Source Comparison Metric Value
  colnames(sourceData)[which(colnames(sourceData) == comparisonMetric)] <- "Hub Comparison Metric Value"
  colnames(sourceData)[which(colnames(sourceData) == paste("i.",comparisonMetric,sep=""))] <- "Source Comparison Metric Value"
  
  #KeyCol <- sourceData[,paste(.SD, collapse="|"), by=c(comparisonMetric)]
  
  
  # Set test Pass/Fail result
  sourceData[,PercentageDiffPass:= !( (PercentageDiff > threshold & ValueDiff > minimumThreshold) | (PercentageDiff < (threshold*-1) & ValueDiff < (-1*minimumThreshold)) )]
  
  sourceData$PercentageDiffPass <- as.character(sourceData[,PercentageDiffPass])
  
  # Set ignored values to IGNORED instead of Pass Fail.  
  
  if(!any(is.na(ignoredValues))) {
    for(i in 1:nrow(sourceData)) {
      if(length(intersect(ignoredValues, as.matrix(sourceData[i,..nonComparisonCols]))) > 0) {
        sourceData[i,PercentageDiffPass := 'IGNORED']
      }
    }
  }
  
  
  colnames(sourceData)[which(colnames(sourceData) == 'PercentageDiff')] <- "Percentage Difference"
  colnames(sourceData)[which(colnames(sourceData) == 'ValueDiff')] <- "Difference"
  colnames(sourceData)[which(colnames(sourceData) == 'PercentageDiffPass')] <- "Test Passed?"
  
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
    ignoredValues <- strsplit(as.character(valueString), ",") 
    ignoredValues <-  do.call(rbind, ignoredValues)
    
    
    # Possible yesterday values:
    # YESTERDAY - changes this to yesterdays date in format: yyyy-mm-dd
    if(any(ignoredValues == 'YESTERDAY')) {
      ignoredValues[which(ignoredValues == 'YESTERDAY')] <- format(runStartTime-(60*60*24),coercedDateFormat)
    }
    
    # YESTERDAY(formatstring) - changes yesterdays date in format matching formatString
    # must be an R date string
    formatYesterday <- grepl('^YESTERDAY\\([^\\)]+\\)$', ignoredValues)
    
    if(any(formatYesterday)) {
      # replace the YESTERDAY(...) with just the ...
      ignoredValues[formatYesterday] <- sub('YESTERDAY\\(([^\\)]+)\\)', '\\1', ignoredValues[formatYesterday])
      
      #use the value as a format string
      ignoredValues[formatYesterday] <- format(runStartTime-(60*60*24),ignoredValues[formatYesterday])
      
      if(! format(runStartTime-(60*60*24),coercedDateFormat) %in% ignoredValues) {
        ignoredValues <- c(ignoredValues, format(runStartTime-(60*60*24),coercedDateFormat),gsub("(^|[^0-9])0+", "\\1", ignoredValues))
      }
    }
    
  }
  
  
  return( ignoredValues )
}



runTests <- function() {
  
  # read all the tests from the Excel Sheet
  text14 <- rep("text",20)
  testList <- tests <- read_excel(testsFile, col_types=text14, na="", skip=28, col_names=FALSE)
  
  # find last row of data
  if( ! is.na( which(is.na(testList[,1]))[1] ) ) {
    lastRow <- which(is.na(testList[,1]))[1] -1
  } else {
    lastRow <- nrow(testList)
  }
  
  
  # TODO: Try catches around this!
  for(i in 1:lastRow) {
    
    
    if(testList[i,1] == 'Enabled') {
      
      ignoredValues <- NA
      
      if(!is.na(testList[i,7])) {
        ignoredValues <- formatIgnoredValues(testList[i,7])
      }
      
      
      tryCatch({
        
        result <- testDatas(
          programme=testList[i,2],
          testName=testList[i,3],
          comparisonMetric=testList[i,4],
          threshold=testList[i,5],
          minimumThreshold=testList[i,6],
          ignoredValues=ignoredValues,
          expectedDateFormats=testList[i,8],
          
          SourceDataPath=testList[i,9],
          SourceDataFileName=testList[i,10],
          SourceHeaderRow=testList[i,11],
          SourceEndRow=testList[i,12],
          SourceColumnsToInclude=testList[i,13],
          SourceHeaderValues=testList[i,14],
          
          HubDataPath=testList[i,15],
          HubDataFileName=testList[i,16],
          HubHeaderRow=testList[i,17],
          HubEndRow=testList[i,18],
          HubColumnsToInclude=testList[i,19],
          HubHeaderValues=testList[i,20]
        )
        
        if(is.character(result)) {
          print( paste(testList[i,2], " - ", testList[i,3], ' ran successfully.', sep="") )
        } else {
          warning(paste('Something went wrong on: "', testList[i,2], " - ", testList[i,3], '". Check the logs for more info', sep=""))
        }
        
      }, error=function(err) {
        
        logToFile(testList[i,2],testList[i,3], "Error", paste(" Programmer Error (Tom) - ",err, sep="") )
        warning(paste('Something serious went wrong on: "', testList[i,2], " - ", testList[i,3], '". Check the logs for more info', sep="")) 
        
      });
      
      
    } else if (is.character(testList[i,1]) & testList[i,1] == 'Example') {
      logToFile(testList[i,2],testList[i,3], "Skipped", paste("this line in the tests file is an example and will not be run.", sep="") )
    } else {
      logToFile(testList[i,2],testList[i,3], "Skipped","Test is not Enabled")
      print( paste('Skipped "', testList[i,2], " - ", testList[i,3], '" test is not Enabled.', sep="") )
    }
  }
  
  
  # TODO: cleanup test files
  if(length(cleanupList) > 0) {
    for(i in 1:length(cleanupList)) {
      if(file.exists(cleanupList[[i]])) {
        file.remove(cleanupList[[i]])
      }
    }
  }
  
  # TODO: log to file for test completed / errored / whatever
  
  
  
}


# Run it!
runTests()
