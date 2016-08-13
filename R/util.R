##INTERNAL FUNCTION TO BUILD QUERY LIST ----------------------------------------

buildQuery <- function(id, dimensions, metrics, start, end, token, sort, max, 
                       segment, filters, samplingLevel, includeEmptyRows) 
{
  q <- lapply( id, function(x) {
    list(
      'ids'           = x,
      'dimensions'    = dimensions,
      'metrics'       = metrics,
      'start-date'    = if(length(start)>1) { as.character(start[match(x,id)]) } else as.character(start),
      'end-date'      = if(length(end)>1) { as.character(end[match(x,id)]) } else as.character(end),
      'sort'          = sort,
      'max-results'   = max,
      'filters'       = filters,
      'segment'       = segment,
      'access_token'  = token,
      'samplingLevel' = samplingLevel,
      'include_empty_rows' = includeEmptyRows
    )
  }
  )
  names(q) <- id 
  
  ##REMOVE ANY UNUSED PARAMETERS
  q <- lapply( q, function(x) { x[!is.na(x)] } )
  
  return(q)
}

##INTERNAL FUNCTION BUILD AND FETCH --------------------------------------------
buildAndFetch <- function(q){
  ##BUILD QUERY AND FETCH RESULTS
  r <- lapply(q, function(x) { 
    GET("https://www.googleapis.com/analytics/v3/data/ga", 
        query = x) 
  }
  )
  
  #PARSE JSON RESPONSE FROM GA
  r <- lapply( r, function(x) { 
    jsonlite::fromJSON(
      jsonlite::toJSON(content(x)) 
      ,flatten=T)
  } 
  )
  
  return(r)
}


## INTERNAL FUNCTION BUILD DF OF QUERY DETAILS ---------------------------------
requestDetails <- function(x) {
  df <- data.frame(
    'profileId'             = x$profileInfo$profileId,
    'accountId'             = x$profileInfo$accountId,
    'webPropertyId'         = x$profileInfo$webPropertyId,
    'internalWebPropertyId' = x$profileInfo$internalWebPropertyId,
    'profileName'           = x$profileInfo$profileName,
    'tableId'               = x$profileInfo$tableId,
    'start-date'            = x$query$`start-date`,
    'end-date'              = x$query$`end-date`,
    'containsSampledData'   = x$containsSampledData,
    'totalResults'          = x$totalResults,
    stringsAsFactors = F
  )
  return(df)
}

## INTERNAL FUNCTION BUILD DF OF QUERY ROW RESULTS ----------------------------_

requestResults <- function(x) {
  if(x$totalResults==0) {
    df <- data.frame( matrix( nrow=1, ncol= length(x$columnHeaders$name) ))
  } else {
    df <- data.frame(x$rows, stringsAsFactors=FALSE)
  }
  
  colnames(df) <- unlist(x$columnHeaders$name)
  
  #FORMAT METRIC DATA AS NUMERIC
  metricNames <- as.vector(x$query$metrics)
  for (i in metricNames){
    df[,i] <- as.numeric(df[,i])
  }
  
  return(df)
}


##INTERNAL FUNCTION ERROR CHECK RESULTS ----------------------------------------

errorCheck <- function(x) {
  
  error <- lapply(x, function(x) {names(x)} )
  error <- grepl('error',error)
  
  errorData <- x[error==T]
  
  names(errorData) <- NULL
  
  if(length(errorData) > 0) {
    errorData <- lapply( errorData, function(x) { 
      data.frame(
        x$error$errors$domain[1],
        #x$error$errors$reason[1],
        x$error$code,
        x$error$message,
        stringsAsFactors = F
      )
    })
    errorData <- do.call('rbind',errorData)
    colnames(errorData) <- c('domain','code','message')
    
  }
  
  output <- list(
    errorIndex=error,
    errorDf=errorData,
    finalDf=x[error==F]
  )
  
  return(output)
  
}

##INTERNAL FUNCTION LIST TO DF -------------------------------------------------

toDF <- function(x) {
  
  df <- lapply(x, function(x) {
    merge(
      requestDetails(x),
      requestResults(x)
    )
  }
  )
  
  df <- do.call('rbind', df)
  
  ##REMOVE GA FROM COLUMN NAMES
  colnames(df) <- gsub('ga:','',colnames(df))
  
  return(df)
}


## INTERNAL FUNCTION TO VALIDATE ARGUMENTS USED IN GAREQUEST -------------------
validateArgs <- function(dimensions, metrics, start, end, token, sort, max, 
                         segment, filters, allResults, samplingLevel, includeEmptyRows) {
  validate <- list(
    dimensions = all(is.na(dimensions) | class(dimensions) == 'character'),
    metrics = all(class(metrics) == 'character'),
    start = all(grepl('[0-9]{4}-[0-9]{2}-[0-9]{2}|today|yesterday|[0-9]+(daysAgo)',start)),
    end = all(grepl('[0-9]{4}-[0-9]{2}-[0-9]{2}|today|yesterday|[0-9]+(daysAgo)',end)),
    token = is.na(token) | class(token) == 'character',
    sort = all(is.na(sort) | class(sort) == 'character'),
    max = is.numeric(max),
    filters = is.na(filters) | class(filters) == 'character',
    segment = is.na(segment) | class(segment) == 'character',
    allResults = is.logical(allResults),
    samplingLevel = class(samplingLevel) == 'character',
    includeEmptyRows = is.logical(includeEmptyRows)
  )
  
  results <- list(
    validated = all(validate==TRUE),
    data = validate,
    inValidArguments=validate[validate==FALSE]  
  )
  
  return(results)
  
}

## INTERNAL FUNCTION TO ADD GA PREFIX TO METRICS, DIMS, SORT, FILTERS. ---------

gaPrefix <- function(parm) {
  
  if(class(parm)=='logical') {
    
    parm <- parm
    
  } else {
    
    ## SPLIT STRINGS
    if(length(parm)==1) {
      parm <- strsplit(parm,',')[[1]]
    } else {
      parm <- parm
    } 
    
    ## ADD ASC OR DESC TO PRESERVE SORT ORDER
    names(parm) <- ifelse(grepl('-',parm),'DESC','ASC')
    
    ## REMOVE SORT ORDER
    parm <- gsub('-','', parm)
    
    ## ADD "GA" PREFIX
    parm <- sapply(parm, function(x) 
      
      if(grepl('ga:',x)) { 
        return(x) 
      } else { 
        paste('ga',x,sep=':') 
      } 
    )
    
    ## ADD SORT ORDER BACK TO ITEM
    
    desc <- grep('DESC',names(parm))
    parm[desc] <-  paste0('-',parm[desc])
  
  }
  return(parm)
}