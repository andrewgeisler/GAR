getAccountSummary <- function(token) {


  ## BUILD AND REQUEST DATA
  baseUrl <- 'https://www.googleapis.com/analytics/v3'
  type <- '/management/accountSummaries'
  results <- GET(paste0(baseUrl,type,'?access_token=',token))

  
  ##PARSE JSON DATA TO DATAFRAME
  results <- content(results)
  results <- results$items

  #CREATE DATAFRAME FOR USE
  df <- list()
  c<-0

  #LOOP THROUGH NESTED LIST TO BUILD DATAFRAME
  for (i in 1:length(results)) {
  
    for (x in 1:length(results[[i]]$webProperties)) {
      
      for (y in 1:length(results[[i]]$webProperties[[x]]$profiles)) {
                                                                c<-c+1
                                                                df[[c]] <- data.frame(
                                                                          accountName          =results[[i]]$name,
                                                                          accountId            =results[[i]]$id,
                                                                          propertyName         =results[[i]]$webProperties[[x]]$name,
                                                                          propertyId           =results[[i]]$webProperties[[x]]$id,
                                                                          propertyUrl          = ifelse(
                                                                                                        !is.null(results[[i]]$webProperties[[x]]$websiteUrl),
                                                                                                        results[[i]]$webProperties[[x]]$websiteUrl,
                                                                                                        'NA'
                                                                                                        ),
                                                                          profileName           =results[[i]]$webProperties[[x]]$profiles[[y]]$name,
                                                                          profileId             =results[[i]]$webProperties[[x]]$profiles[[y]]$id,
                                                                          profileType           =results[[i]]$webProperties[[x]]$profiles[[y]]$type
                                                                , stringsAsFactors = F)
        
  }}}
  rm(c,i,x,y)
  
  df <- do.call('rbind',df)
  df[df=='NA'|df=='--'] <- NA
  return(df)
}
