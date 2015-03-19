gaRequest <-
function(id, dimensions=NA, metrics, start, end, token=NA, sort=NA, max=NA, segment=NA, filters=NA) {
    
  ##PRIORITIZES SUPPLIED TOKEN FIRST. THEN CHECKS FOR ENV VARIABLE, THEN DEFAULTS TO NA.
  if (!is.na(token)){
    token <- token
  } else if (exists('GAR_ACCESS_TOKEN', envir=envGAR)){
    token <- get('GAR_ACCESS_TOKEN', envir=envGAR)
  } else {
    token <- token
  }
  
  
    ##CREATE LIST OF QUERY PARAMETERS
    queryList <- as.list(id)
    for (x in 1:length(id)) { 
      queryList[[x]] <- list(
        'ids'=id[x],
        'dimensions'=dimensions,
        'metrics'=metrics,
        'start-date'=start,
        'end-date'=end,
        'sort'=sort,
        'max-results'=max,
        'filters'=filters,
        'segment'=segment,
        'access_token'=token
      )
    }
    
    ##REMOVE ANY UNUSED PARAMETERS
    for (x in 1:length(id)){
      queryList[[x]] <- queryList[[x]][!is.na(queryList[[x]])]
    }
    
    ##BUILD QUERY AND FETCH RESULTS
    response <- as.list(id)
    for (x in 1:length(id)) {
      response[[x]] <- GET("https://www.googleapis.com/analytics/v3/data/ga",
                           query = queryList[[x]]
      )
    }
    
    #PARSE JSON RESPONSE FROM GA
    for (x in 1:length(id)) {
      response[[x]] <- jsonlite::toJSON(content(response[[x]]))
      response[[x]] <- jsonlite::fromJSON(response[[x]], flatten=TRUE)
    }
    
    ##ERROR HANDLING
    
    if(TRUE %in% grepl('error', response)){
      ##IF ERROR THEN DATA FRAME OF ERROR RESPONSE CODE AND MESSAGE
      
      df <- list()
      for (x in 1: length(id)){
        df[x] <- list(data.frame(response[[x]]))
      }
      
      df <- do.call('rbind',df)
      
    } else { 
      ## IF NO ERROR, BUILD DATA FRAME OF RESPONSE DATA
      
      df <- list()
      for (x in 1:length(id)) {
        df[x] <- list(cbind(
          data.frame(response[[x]]$profileInfo, stringsAsFactors=FALSE),
          data.frame(response[[x]]$query['start-date'], stringsAsFactors=FALSE),
          data.frame(response[[x]]$query['end-date'], stringsAsFactors=FALSE),
          data.frame(response[[x]]$rows, stringsAsFactors=FALSE)
        ))
      }
      
      df <- do.call('rbind', df)
      
      ##RENAME COLUMNS
      colnames(df) <- c(
        names(response[[1]]$profileInfo),
        names(response[[1]]$query['start-date']),
        names(response[[1]]$query['end-date']),
        unlist(response[[1]]$columnHeaders$name)
      )
      
      #FORMAT METRIC DATA AS NUMERIC
      metricNames <- as.vector(response[[1]]$query$metrics)
      for (i in metricNames){
        df[,i] <- as.numeric(df[,i])
      }
      
      ##REMOVE GA FROM COLUMN NAMES
      colnames(df) <- gsub('ga:','',colnames(df))
      
    } #END IF ELSE STATEMENT
    
    
    return(df)
  }