getFilterLinks <- function(account_id, property_id, profile_id, token) {

  ## BUILD AND REQUEST DATA
  baseUrl <- 'https://www.googleapis.com/analytics/v3'
  type <- paste0('/management/accounts/',account_id,'/webproperties/',property_id,'/profiles/',profile_id,'/profileFilterLinks')
  results <- GET(paste0(baseUrl,type,'?access_token=',token))

  ##PARSE JSON DATA TO DATAFRAME
  results <- content(results)
  totalResults <- as.numeric(results$totalResults)
  results <- results$items
  
  #CREATE DATAFRAME FOR USE
  
  
  if (totalResults>0 & exists('totalResults')) {
  
     df <- lapply(1:length(results), function(i) { 
                                                  data.frame(
                                                  accountId=results[[i]]$profileRef$accountId,
                                                  propertyId=results[[i]]$profileRef$webPropertyId,
                                                  profileId=results[[i]]$profileRef$id,
                                                  profileName=results[[i]]$profileRef$name,
                                                  filterRank=results[[i]]$rank,
                                                  filterId=results[[i]]$filterRef$id,
                                                  filterName=results[[i]]$filterRef$name,
                                                  stringsAsFactors = F)
                                                  } 
                                                  )
      
      df <- do.call('rbind',df)
      
  } else {
    
      df <- data.frame(
                                                  accountId=account_id,
                                                  propertyId=property_id,
                                                  profileId=profile_id,
                                                  profileName='NA',
                                                  filterRank='NA',
                                                  filterId='NA',
                                                  filterName='NA',
                                                  stringsAsFactors = F
                                                  )
      
  }

  
  ##FORMAT FIELDS
  
  df[df=='NA'|df=='--'] <- NA

  return(df)
}
