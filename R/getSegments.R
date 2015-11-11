getSegments <- function(token) {

  ## BUILD AND REQUEST DATA
  baseUrl <- 'https://www.googleapis.com/analytics/v3'
  type <- paste0('/management/segments')
  results <- GET(paste0(baseUrl,type,'?access_token=',token))


  ##PARSE JSON DATA TO DATAFRAME
  results <- content(results)
  results <- results$items

  #CREATE DATAFRAME FOR USE
  
  df <- lapply(1:length(results), function(i) { 
                                              data.frame(
                                              segmentName=results[[i]]$name,
                                              segmentId=results[[i]]$segmentId,
                                              definition=results[[i]]$definition,
                                              type=results[[i]]$type,
                                              stringsAsFactors = F)
                                              } 
                                              )
  df <- do.call('rbind',df)
  
  ##FORMAT FIELDS
  
  df[df=='NA'|df=='--'] <- NA

  return(df)
}
