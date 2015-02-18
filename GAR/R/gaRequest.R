gaRequest <-
function(id, dimensions, metrics, start, end, token, sort=NA, max=NA, filters=NA) {
  
                      ##CREATE LIST OF QUERY PARAMETERS
                      queryList <- list(
                                    'ids'=id,
                                    'dimensions'=dimensions,
                                    'metrics'=metrics,
                                    'start-date'=start,
                                    'end-date'=end,
                                    'sort'=sort,
                                    'max-results'=max,
                                    'filters'=filters,
                                    'access_token'=token
                                  )
                      
                      ##REMOVE ANY UNUSED PARAMETERS
                      queryList <- queryList[!is.na(queryList)]
                      
                      ##BUILD QUERY AND FETCH RESULTS
                      response <- GET("https://www.googleapis.com/analytics/v3/data/ga",
                          query = queryList
                          )
                      
                      #PARSE JSON RESPONSE FROM GA
                      response <- jsonlite::toJSON(content(response))
                      response <- jsonlite::fromJSON(response, flatten=TRUE)
                      
                      #CREATE DATAFRAME FOR RESULT ROWS
                      df <- data.frame(response$rows, stringsAsFactors=FALSE)
                      
                      #RENAME DF COLUMN NAME
                      colnames(df) <- unlist(response$columnHeaders$name)
                      
                      #FORMAT METRIC DATA AS NUMERIC
                      for (i in 1:length(response$query$metrics)) {
                                        df[,response$query$metrics[i]] <- as.numeric(df[,response$query$metrics[i]])
                                        }
                      
                      ##CREATE DATE FRAME FOR START AND END DATE
                      dates <- data.frame(
                                         response$query['start-date'],
                                         response$query['end-date'], 
                                         stringsAsFactors=FALSE
                                         )
                      
                      #CREATE DATA FRAME FOR PROFILE INFORMATION
                      profileInfo <- data.frame(response$profileInfo, stringsAsFactors=FALSE)
                      
                      #FINAL DATA FRAME WITH DATE RANGE, PROFILE INFO AND RESULT ROWS
                      finalDF <- cbind(profileInfo,dates, df)
                      return(finalDF)
}
