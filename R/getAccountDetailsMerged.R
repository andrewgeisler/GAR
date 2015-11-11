getAccountDetailsMerged <- function(token) {
  
  
            df <- getAccountSummary(token)
            
            dfFilters <- lapply(unique(df$accountId),function(x) {getFilters(x,token)})
            dfFilters <- do.call('rbind', dfFilters )
            
            dfFilterLinks <- lapply(1:nrow(df), function(i) {
              #s <- Sys.time()
              
              getFilterLinks(account_id=df$accountId[i],property_id=df$propertyId[i],profile_id=df$profileId[i],token)
              
              #e <- Sys.time()
              #duration <- as.numeric(e-s)
              #Sys.sleep(1-duration)
              
            })
            
            dfFilterLinks <- do.call('rbind', dfFilterLinks )
            
            
            dfGoals <- lapply(1:nrow(df), function(i) {
                    
                      #s <- Sys.time()
                      
                      getGoals(account_id=df$accountId[i],property_id=df$propertyId[i],profile=df$profileId[i],token)
                      
                      #e <- Sys.time()
                      #duration <- as.numeric(e-s)
                      #Sys.sleep(1-duration)
                })

            
            dfGoals <- do.call('rbind', dfGoals )
            
            
            ### MERGE
            
            dfMerge <- merge(x=df, y=dfGoals, by.x = c('accountId','propertyId','profileId'), by.y= c('accountId','propertyId','profileId'))
            
            dfMerge <- merge(x=dfMerge, y=dfFilterLinks, by.x = c('accountId','propertyId','profileId'), by.y= c('accountId','propertyId','profileId'))
            
            dfMerge <- merge(x=dfMerge, y=dfFilters, by.x = c('accountId','filterId'), by.y= c('accountId','filterId'), all=T)

  
  return(dfMerge)
  
  
  
  
}