getCode <- function(client_id=Sys.getenv('GAR_CLIENT_ID')){
  
  url <- 'https://accounts.google.com/o/oauth2/auth?scope=https://www.googleapis.com/auth/analytics&access_type=offline&approval_prompt=force&redirect_uri=http://localhost&response_type=code&client_id='
  url <- paste0(url,client_id)
  browseURL(url)
  
}