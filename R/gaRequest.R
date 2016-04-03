gaRequest <- function(id, dimensions = NA, metrics, start, end, token = NA, 
                      sort = NA, max = 1000, segment = NA, filters = NA, 
                      allResults = FALSE, samplingLevel='DEFAULT') {
  
  ## PRIORITIZES SUPPLIED TOKEN FIRST. 
  ## THEN CHECKS FOR ENV VARIABLE.
  ## THEN DEFAULTS TO NA. -----------------------------------------------------
  
  if (!is.na(token)) {
    token <- token
  } else if (exists("GAR_ACCESS_TOKEN", envir = envGAR)) {
    token <- get("GAR_ACCESS_TOKEN", envir = envGAR)
  } else {
    token <- token
  }
  
  ## CREATE LIST OF INITIAL QUERY PARAMETERS
  ## --------------------------------------------------------------------------------------------------------
  
  queryList <- buildQuery(id, dimensions, metrics, start, end, token, sort, max, 
    segment, filters, samplingLevel)
  
  ## BUILD AND FETCH INITIAL QUERY
  ## --------------------------------------------------------------------------------------------------------
  finalDf <- buildAndFetch(queryList)
  
  ### ERROR CHECK
  ### --------------------------------------------------------------------------------------------------------
  
  errors <- errorCheck(finalDf)
  
  if ("TRUE" %in% errors$errorIndex) {
    if ("FALSE" %in% errors$errorIndex) {
      print(errors$errorDf)
      warning("One or more requests contained errors. See console log.")
      finalDf <- errors$finalDf
    } else {
      warning("One or more requests contained errors. See resulting data frame.")
      finalDf <- errors$errorDf
    }
    
  } else {
    
    finalDf <- errors$finalDf
    
  }
  if (length(colnames(finalDf)) == 3) {
    
    return(finalDf)
    
  } else {
    
    ### BUILD DATA FRAME OF RESULTS
    ### ----------------------------------------------------------------------------------------------------------------
    
    finalDf <- toDF(finalDf)
    
    ## CREATE LIST OF PAGINATION QUERY PARAMETERS
    ## --------------------------------------------------------------------------------------------------------
    if (allResults == TRUE & as.numeric(max) == 10000 & finalDf$totalResults[1] > 
      10000) {
      
      totalResults <- aggregate(data = finalDf, totalResults ~ tableId, FUN = mean)
      totalResults$pages <- floor(totalResults$totalResults/as.numeric(max))
      
      totalResults <- lapply(totalResults$tableId, function(x) {
        data.frame(profileId = totalResults$tableId[totalResults$tableId == 
          x], pages = 0:totalResults$pages[totalResults$tableId == x], stringsAsFactors = FALSE, 
          row.names = NULL)
      })
      
      totalResults <- do.call("rbind", totalResults)
      totalResults$start_index <- (totalResults$pages * as.numeric(max)) + 
        1
      totalResults <- totalResults[!is.na(totalResults$pages) & totalResults$pages > 
        0, ]
      
      
      queryListPaginate <- lapply(totalResults$profileId, function(x) {
        queryList[[x]]
      })
      
      for (i in 1:length(queryListPaginate)) {
        queryListPaginate[[i]]$`start-index` <- totalResults$start_index[i]
      }
      
      ## BUILD AND FETCH INITIAL QUERY
      ## --------------------------------------------------------------------------------------------------------
      paginateDf <- buildAndFetch(queryListPaginate)
      paginateDf <- toDF(paginateDf)
      
      finalDf <- rbind(finalDf, paginateDf)
      
    } else {
      
      finalDf <- finalDf
      
    }
    
    return(finalDf)
    
  }
}