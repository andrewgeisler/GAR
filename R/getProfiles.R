getProfiles <- function(account_id, property_id, token) {
  
  ## BUILD AND REQUEST DATA
  baseUrl <- 'https://www.googleapis.com/analytics/v3'
  type <-
    paste0('/management/accounts/',
           account_id,
           '/webproperties/',
           property_id,
           '/profiles')
  results <- GET(paste0(baseUrl, type, '?access_token=', token))
  
  
  ##PARSE JSON DATA TO DATAFRAME
  results <- content(results)
  results <- results$items
  
  #CREATE DATAFRAME FOR USE
  
  df <- lapply(1:length(results), function(i) {
    data.frame(
      profileName = results[[i]]$name,
      profileId = results[[i]]$id,
      accountId = results[[i]]$accountId,
      propertyId = results[[i]]$internalWebPropertyId,
      propertyUrl = ifelse(!is.null(results[[i]]$websiteUrl),
                           results[[i]]$websiteUrl,
                           'NA'),
      currency = results[[i]]$currency,
      timezone = results[[i]]$timezone,
      type = results[[i]]$type,
      ecommerce = results[[i]]$eCommerceTracking,
      siteSearchParameter = ifelse(
        !is.null(results[[i]]$siteSearchQueryParameters),
        results[[i]]$siteSearchQueryParameters,
        'NA'
      ),
      siteSearchCategoryParameter = ifelse(
        !is.null(results[[i]]$siteSearchQueryCategoryParameters),
        results[[i]]$siteSearchQueryCategoryParameters,
        'NA'
      ),
      created = results[[i]]$created,
      updated = results[[i]]$updated,
      stringsAsFactors = F
    )
  })
  df <- do.call('rbind', df)
  
  ##FORMAT FIELDS
  
  df[df == 'NA' | df == '--'] <- NA
  df$created <- strptime(df$created, "%Y-%m-%dT%H:%M:%S")
  df$updated <- strptime(df$updated, "%Y-%m-%dT%H:%M:%S")
  
  return(df)
}
