getGoals <- function(account_id, property_id, profile_id, token) {
  
  ## BUILD AND REQUEST DATA
  baseUrl <- 'https://www.googleapis.com/analytics/v3'
  type <-
    paste0(
      '/management/accounts/',
      account_id,
      '/webproperties/',
      property_id,
      '/profiles/',
      profile_id,
      '/goals'
    )
  results <- GET(paste0(baseUrl, type, '?access_token=', token))
  
  ##PARSE JSON DATA TO DATAFRAME
  results <- content(results)
  totalResults <- as.numeric(results$totalResults)
  results <- results$items
  
  #CREATE DATAFRAME FOR USE
  
  ## TEST FOR EMPTY RESULTS
  if (totalResults > 0) {
    ### BUILD DATA FRAME FOR NON EMPTY RESULTS
    df <- lapply(1:length(results), function(i) {
      data.frame(
        accountId = results[[i]]$accountId,
        propertyId = results[[i]]$webPropertyId,
        profileId = results[[i]]$profileId,
        goalName = results[[i]]$name,
        goalValue = results[[i]]$value,
        goalActive = results[[i]]$active,
        goalType = results[[i]]$type,
        created = results[[i]]$created,
        updated = results[[i]]$updated,
        urlDestinationUrl = ifelse(is.null(results[[i]]$urlDestinationDetails$url), NA, results[[i]]$urlDestinationDetails$url),
        caseSensitive = ifelse( is.null(results[[i]]$urlDestinationDetails$caseSensitive), NA, results[[i]]$urlDestinationDetails$caseSensitive),
        urlDestinationMatchType = ifelse(is.null(results[[i]]$urlDestinationDetails$matchType), NA, results[[i]]$urlDestinationDetails$matchType),
        urlDestinationfirstStepRequired = ifelse(is.null(results[[i]]$urlDestinationDetails$firstStepRequired), NA, results[[i]]$urlDestinationDetails$firstStepRequired ),
        
        #urlDestinationSteps                =ifelse(is.null(results[[i]]$urlDestinationDetails$steps),
        #NA,results[[i]]$urlDestinationDetails$steps),
        
        visitTimeOnSiteComparisonType = ifelse( is.null(results[[i]]$visitTimeOnSiteDetails$comparisonType), NA, results[[i]]$visitTimeOnSiteDetails$comparisonType),
        visitTimeOnSiteComparisonValue = ifelse( is.null(results[[i]]$visitTimeOnSiteDetails$comparisonValue), NA, results[[i]]$visitTimeOnSiteDetails$comparisonValue ),
        visitNumPagesComparisonType = ifelse( is.null(results[[i]]$visitNumPagesDetails$comparisonType), NA, results[[i]]$visitNumPagesDetails$comparisonType),
        visitNumPagesComparisonValue = ifelse( is.null(results[[i]]$visitNumPagesDetails$comparisonValue), NA, results[[i]]$visitNumPagesDetails$comparisonType),
        eventDetailsValue = ifelse( is.null(results[[i]]$eventDetails$useEventValue), NA, results[[i]]$eventDetails$useEventValue),
        eventDetailsType = ifelse( is.null(results[[i]]$eventDetails$eventConditions[[1]]$type), NA, results[[i]]$eventDetails$eventConditions[[1]]$type),
        eventDetailsMatchType = ifelse( is.null(results[[i]]$eventDetails$eventConditions[[1]]$matchType), NA, results[[i]]$eventDetails$eventConditions[[1]]$matchType),
        eventDetailsExpression = ifelse( is.null(results[[i]]$eventDetails$eventConditions[[1]]$expression), NA, results[[i]]$eventDetails$eventConditions[[1]]$expression),
        eventDetailsComparisonValue = ifelse( is.null( results[[i]]$eventDetails$eventConditions[[1]]$comparisonValue ), NA, results[[i]]$eventDetails$eventConditions[[1]]$comparisonValue),
        stringsAsFactors = F
      )
    })
    
    df <- do.call('rbind', df)
    
  } else {
    
    ## BUILD EMPTY DATAFRAME FOR EMPTY RESULTS
    df <- data.frame(
      accountId = account_id,
      propertyId = property_id,
      profileId = profile_id,
      goalName = 'NA',
      goalValue = 'NA',
      goalActive = 'NA',
      goalType = 'NA',
      created = 'NA',
      updated = 'NA',
      urlDestinationUrl = 'NA',
      caseSensitive = 'NA',
      urlDestinationMatchType = 'NA',
      urlDestinationfirstStepRequired = 'NA',
      #urlDestinationSteps ='NA',
      visitTimeOnSiteComparisonType = 'NA',
      visitTimeOnSiteComparisonValue = 'NA',
      visitNumPagesComparisonType = 'NA',
      visitNumPagesComparisonValue = 'NA',
      eventDetailsValue = 'NA',
      eventDetailsType = 'NA',
      eventDetailsMatchType = 'NA',
      eventDetailsExpression = 'NA',
      eventDetailsComparisonValue = 'NA',
      stringsAsFactors = F
    )
    
  }
  
  
  ##FORMAT FIELDS
  
  df[df == 'NA' | df == '--'] <- NA
  df$created <- strptime(df$created, "%Y-%m-%dT%H:%M:%S")
  df$updated <- strptime(df$updated, "%Y-%m-%dT%H:%M:%S")
  
  return(df)
}
