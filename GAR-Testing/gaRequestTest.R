#require(devtools)
#install_github('andrewgeisler/GAR', ref='master')
#require(GAR)

require(httr)
require(jsonlite)

##### FUNCTION TEST
gaRequestTest <- function(parms, testResults) {
  
  df <- gaRequest(
    id=parms$id, 
    dimensions=parms$dimensions,
    metrics=parms$metrics,
    start=parms$start,
    end=parms$end,
    sort=parms$sort,
    max=parms$max,
    filters=parms$filters,
    segment=parms$segment,
    allResults=parms$allResults,
    token=parms$token,
    samplingLevel=parms$samplingLevel,
    includeEmptyRows=parms$includeEmptyRows
    )
  

  #NON-ERROR REQUEST
  testResults$noError <- testResults$noError==(nrow(df) >=1 & ncol(df) > 3)
  # ERROR REQUEST
  testResults$Error <- testResults$Error==(ncol(df) == 3)
  # One ID
  
  if(ncol(df)==3) {
    testResults$idsOne <- TRUE
  } else {
    testResults$idsOne <- testResults$idsOne==(length(unique(df$profileId)) == 1)
  }

  # MANY ID
  
  if(ncol(df)==3) {
    testResults$idsMany <- TRUE
  } else {
    testResults$idsMany <- testResults$idsMany==(length(unique(df$profileId)) > 1)
  }
  
  testResults <- lapply(testResults, function(x) ifelse(x==TRUE,'PASSED','FAILED'))
  
  
  results <- list(testResults=testResults,df=df)
  
  return(results)
  
}


idOne <- Sys.getenv('GARidOne')
idTwo <- Sys.getenv('GARidTwo')


### TEST1 GENERIC SIMPLE QUERY --------

test1Simple <- gaRequestTest(
                            parms=list(
                              id=idTwo, 
                              dimensions='ga:date,ga:month',
                              metrics='ga:sessions,ga:users,ga:pageviews',
                              start='2015-01-01',
                              end='2015-11-10',
                              sort='-ga:sessions,ga:users',
                              max='1000',
                              filters='ga:browser==Chrome,ga:city==Nashville',
                              segment='gaid::-3',
                              allResults=FALSE,
                              samplingLevel='FASTER',
                              includeEmptyRows=FALSE,
                              token=GAR::tokenRefresh()
                            ),

                            testResults=list(
                              noError=TRUE,
                              Error=FALSE,
                              idsOne=TRUE,
                              idsMany=FALSE
                            )
)



### TEST2 MULTIPLE IDS - GENERIC SIMPLE QUERY --------


test2MultipleIds <- gaRequestTest(
  parms=list(
    id=c(idOne, idTwo), 
    dimensions='ga:date,ga:month', 
    metrics='ga:sessions,ga:users,ga:pageviews', 
    start='2015-01-01',
    end='2015-11-10',
    sort='-ga:sessions,ga:users',
    max='1000',
    filters='ga:browser==Chrome,ga:city==Nashville',
    segment='gaid::-3',
    allResults=FALSE,
    samplingLevel='FASTER',
    includeEmptyRows=FALSE,
    token=GAR::tokenRefresh()
  ),
  
  testResults=list(
    noError=TRUE,
    Error=FALSE,
    idsOne=FALSE,
    idsMany=TRUE
  )
)



### TEST3 MULTIPLE IDS AND MULTIPLE DATES ------

test3MultipeIdsAndDates <- gaRequestTest(
  parms=list(
    id=c(idOne, idTwo), 
    dimensions='ga:date,ga:month', 
    metrics='ga:sessions,ga:users,ga:pageviews', 
    start=c('2015-01-01','2015-02-01'),
    end=c('2015-11-10','2015-10-31'),
    sort='-ga:sessions,ga:users',
    max='1000',
    filters='ga:browser==Chrome,ga:city==Nashville',
    segment='gaid::-3',
    allResults=FALSE,
    samplingLevel='FASTER',
    includeEmptyRows=FALSE,
    token=GAR::tokenRefresh()
  ),
  
  testResults=list(
    noError=TRUE,
    Error=FALSE,
    idsOne=FALSE,
    idsMany=TRUE
  )
)


### TEST4 PAGINATE AND MULTIPLE OPTIONS ---------


test4PaginateAndMultipleIdsAndDates <- gaRequestTest(
  parms=list(
    id=c(idOne, idTwo), 
    dimensions='ga:date,ga:month,ga:hour', 
    metrics='ga:sessions,ga:users,ga:pageviews', 
    start=c('2014-07-01','2014-07-01'),
    end=c('2015-11-10','2015-10-31'),
    sort='-ga:sessions,ga:users',
    max='10000',
    filters='ga:browser==Chrome,ga:city==Nashville',
    segment='gaid::-3',
    allResults=TRUE,
    samplingLevel='FASTER',
    includeEmptyRows=FALSE,
    token=GAR::tokenRefresh()
  ),
  
  testResults=list(
    noError=TRUE,
    Error=FALSE,
    idsOne=FALSE,
    idsMany=TRUE
  )
)

### TEST5 ERROR MESSAGES - 1 id - query with bad dates ------

test5BadDates <- gaRequestTest(
  parms=list(
    id=idTwo, 
    dimensions='ga:date,ga:month', 
    metrics='ga:sessions,ga:users,ga:pageviews', 
    start='2015-1-01',
    end='2015-11-10',
    sort='-ga:sessions,ga:users',
    max='1000',
    filters='ga:browser==Chrome,ga:city==Nashville',
    segment='gaid::-3',
    allResults=FALSE,
    samplingLevel='FASTER',
    includeEmptyRows=FALSE,
    token=GAR::tokenRefresh()
  ),
  
  testResults=list(
    noError=FALSE,
    Error=TRUE,
    idsOne=TRUE,
    idsMany=FALSE
  )
)



### TEST6 ERROR MESSAGES - 1 id - query with bad parms --------

test6BadParms <- gaRequestTest(
  parms=list(
    id=idTwo, 
    dimensions='ga:datea,ga:month', 
    metrics='ga:sessions,ga:users,ga:pageviews', 
    start='2015-10-01',
    end='2015-11-10',
    sort='-ga:sessions,ga:users',
    max='1000',
    filters='ga:browser==Chrome,ga:city==Nashville',
    segment='gaid::-3',
    allResults=FALSE,
    samplingLevel='FASTER',
    includeEmptyRows=FALSE,
    token=GAR::tokenRefresh()
  ),
  
  testResults=list(
    noError=FALSE,
    Error=TRUE,
    idsOne=TRUE,
    idsMany=FALSE
  )
)


### TEST7 ERROR MESSAGES - 1 id - query with bad metrics -----

test7BadMetrics <- gaRequestTest(
  parms=list(
    id=idTwo, 
    dimensions='ga:date,ga:month', 
    metrics='ga:sessionas,ga:users,ga:pageviews', 
    start='2015-10-01',
    end='2015-11-10',
    sort='-ga:sessions,ga:users',
    max='1000',
    filters='ga:browser==Chrome,ga:city==Nashville',
    segment='gaid::-3',
    allResults=FALSE,
    samplingLevel='FASTER',
    includeEmptyRows=FALSE,
    token=GAR::tokenRefresh()
  ),
  
  testResults=list(
    noError=FALSE,
    Error=TRUE,
    idsOne=TRUE,
    idsMany=FALSE
  )
)

### TEST8 ERROR MESSAGES - 1 id - query with bad date,parms,metrics ------

test8BadDateParmMetrics <- gaRequestTest(
  parms=list(
    id=idTwo, 
    dimensions='ga:datea,ga:month', 
    metrics='ga:sessionas,ga:users,ga:pageviews', 
    start='2015-0-01',
    end='2015-11-10',
    sort='-ga:sessions,ga:users',
    max='1000',
    filters='ga:browser==Chrome,ga:city==Nashville',
    segment='gaid::-3',
    allResults=FALSE,
    samplingLevel='FASTER',
    includeEmptyRows=FALSE,
    token=GAR::tokenRefresh()
  ),
  
  testResults=list(
    noError=FALSE,
    Error=TRUE,
    idsOne=TRUE,
    idsMany=FALSE
  )
)


### TEST9 ERROR MESSAGES - 2 id - 1 id query with bad dates ------

test9OneIdBadDates <- gaRequestTest(
  parms=list(
    id=c(idOne, idTwo), 
    dimensions='ga:date,ga:month', 
    metrics='ga:sessions,ga:users,ga:pageviews', 
    start=c('2015-01-01','2015-2-01'),
    end=c('2015-11-10','2015-10-31'),
    sort='-ga:sessions,ga:users',
    max='1000',
    filters='ga:browser==Chrome,ga:city==Nashville',
    segment='gaid::-3',
    allResults=FALSE,
    samplingLevel='FASTER',
    includeEmptyRows=FALSE,
    token=GAR::tokenRefresh()
  ),
  
  testResults=list(
    noError=TRUE,
    Error=FALSE,
    idsOne=TRUE,
    idsMany=FALSE
  )
)


### TEST10 ERROR MESSAGES - 2 id - query with bad dates ------

test10TwoIdsBadDates <- gaRequestTest(
  parms=list(
    id=c(idOne, idTwo), 
    dimensions='ga:date,ga:month', 
    metrics='ga:sessions,ga:users,ga:pageviews', 
    start=c('2015-01-01','2015-2-01'),
    end=c('2015-1-10','2015-10-31'),
    sort='-ga:sessions,ga:users',
    max='1000',
    filters='ga:browser==Chrome,ga:city==Nashville',
    segment='gaid::-3',
    allResults=FALSE,
    samplingLevel='FASTER',
    includeEmptyRows=FALSE,
    token=GAR::tokenRefresh()
  ),
  
  testResults=list(
    noError=FALSE,
    Error=TRUE,
    idsOne=FALSE,
    idsMany=TRUE
  )
)



### TEST11 ERROR MESSAGES - 2 id - query with bad parms ------

test11TwoIdsBadParms <- gaRequestTest(
  parms=list(
    id=c(idOne, idTwo), 
    dimensions='ga:datae,ga:month', 
    metrics='ga:sessions,ga:users,ga:pageviews', 
    start=c('2015-01-01','2015-02-01'),
    end=c('2015-11-10','2015-10-31'),
    sort='-ga:sessions,ga:users',
    max='1000',
    filters='ga:browser==Chrome,ga:city==Nashville',
    segment='gaid::-3',
    allResults=FALSE,
    samplingLevel='FASTER',
    includeEmptyRows=FALSE,
    token=GAR::tokenRefresh()
  ),
  
  testResults=list(
    noError=FALSE,
    Error=TRUE,
    idsOne=FALSE,
    idsMany=TRUE
  )
)


### TEST12 ERROR MESSAGES - 2 id - query with bad metrics -----

test12TwoIdsBadMetrics <- gaRequestTest(
  parms=list(
    id=c(idOne, idTwo), 
    dimensions='ga:date,ga:month', 
    metrics='ga:sessaions,ga:users,ga:pageviews', 
    start=c('2015-01-01','2015-02-01'),
    end=c('2015-11-10','2015-10-31'),
    sort='-ga:sessions,ga:users',
    max='1000',
    filters='ga:browser==Chrome,ga:city==Nashville',
    segment='gaid::-3',
    allResults=FALSE,
    samplingLevel='FASTER',
    includeEmptyRows=FALSE,
    token=GAR::tokenRefresh()
  ),
  
  testResults=list(
    noError=FALSE,
    Error=TRUE,
    idsOne=FALSE,
    idsMany=TRUE
  )
)


### TEST13 ERROR MESSAGES - 2 id - query with bad date,parms,metrics ------

test13TwoIdsBadDatesParmsMetrics <- gaRequestTest(
  parms=list(
    id=c(idOne, idTwo), 
    dimensions='ga:daate,ga:month', 
    metrics='ga:sessaions,ga:users,ga:pageviews', 
    start=c('2015-1-01','2015-02-01'),
    end=c('2015-11-10','205-10-31'),
    sort='-ga:sessions,ga:users',
    max='1000',
    filters='ga:browser==Chrome,ga:city==Nashville',
    segment='gaid::-3',
    allResults=FALSE,
    samplingLevel='FASTER',
    includeEmptyRows=FALSE,
    token=GAR::tokenRefresh()
  ),
  
  testResults=list(
    noError=FALSE,
    Error=TRUE,
    idsOne=FALSE,
    idsMany=TRUE
  )
)

### TEST14 Properly Handle max=1000, all results=TRUE and totalResults>10000 ------

test14pageLogicLess10K <- gaRequestTest(
  parms=list(
    id=c(idOne, idTwo), 
    dimensions='ga:date,ga:month', 
    metrics='ga:sessions,ga:users,ga:pageviews', 
    start=c('2015-01-01','2015-02-01'),
    end=c('2015-11-10','2015-10-31'),
    sort='-ga:sessions,ga:users',
    max='10000',
    filters='ga:browser==Chrome,ga:city==Nashville',
    segment='gaid::-3',
    allResults=TRUE,
    samplingLevel='FASTER',
    includeEmptyRows=FALSE,
    token=GAR::tokenRefresh()
  ),
  
  testResults=list(
    noError=TRUE,
    Error=FALSE,
    idsOne=FALSE,
    idsMany=TRUE
  )
)


#### CONSOLIDATE RESULTS RESULTS ---- 

names <- grep('test',ls(), value=T)

overallResults <- lapply(names,
function(x) {
results <- sapply(get(x)$testResults, function(x) x)

results <- ifelse('FAILED' %in% results,'FAILED','PASSED')

y <- data.frame(testName=x,results=results)

return(y)

}
)

rm(names)

overallResults <- do.call('rbind',overallResults)





