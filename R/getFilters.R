getFilters <- function(account_id, token) {
  
  ## BUILD AND REQUEST DATA
  baseUrl <- 'https://www.googleapis.com/analytics/v3'
  type <- paste0('/management/accounts/', account_id, '/filters')
  results <- GET(paste0(baseUrl, type, '?access_token=', token))
  
  
  ##PARSE JSON DATA TO DATAFRAME
  results <- content(results)
  totalResults <- as.numeric(results$totalResults)
  results <- results$items
  
  #CREATE DATAFRAME FOR USE
  
  if (totalResults > 0 & exists('totalResults')) {
    df <- lapply(1:length(results), function(i) {
      
      data.frame(
        accountId = results[[i]]$accountId,
        filterName = results[[i]]$name,
        filterId = results[[i]]$id,
        kind = results[[i]]$kind,
        selfLink = results[[i]]$selfLink,
        type = results[[i]]$type,
        parentLink = results[[i]]$parentLink$href,
        created = results[[i]]$created,
        updated = results[[i]]$updated,
        details = paste0(paste(
                              names(unlist(results[[i]][10])),
                              unlist(results[[i]][10], use.names = FALSE),
                              sep = '='
                              ), collapse = ' | '),
        stringsAsFactors = F
      )
    })
    
    df <- do.call('rbind', df)
    
  } else {
    df <- data.frame(
      accountId = account_id,
      filterName = NA,
      filterId = NA,
      kind = NA,
      selfLink = NA,
      type = NA,
      parentLink = NA,
      created = NA,
      updated = NA,
      details = NA,
      stringsAsFactors = F
    )
    
  }
  
  
  
  ##FORMAT FIELDS
  
  df[df == 'NA' | df == '--'] <- NA
  df$created <- strptime(df$created, "%Y-%m-%dT%H:%M:%S")
  df$updated <- strptime(df$updated, "%Y-%m-%dT%H:%M:%S")
  
  return(df)
}
