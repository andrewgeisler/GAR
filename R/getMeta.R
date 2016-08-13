getMeta <- function() {
  
  response <- GET('https://www.googleapis.com/analytics/v3/metadata/ga/columns')
  response <- jsonlite::toJSON(content(response))
  response <- jsonlite::fromJSON(response, flatten = TRUE)
  
  df <- response$items
  
  for (x in 1:length(df)) {
    df[[x]] <- gsub("NULL", "NA", df[[x]])
    rm(x)
    
  }
  
  colnames(df) <- gsub('attributes.', '', colnames(df))
  
  df[df == 'NA'] <- NA
  
  return(df)
  
}
