getCode <- function(client_id=Sys.getenv('GAR_CLIENT_ID'), scope = 'analytics' ){

## SCOPE OPTIONS ARE:
## analytics default
## analytics.readonly	Read-only access to the Analytics API.
## analytics.edit	Edit Google Analytics management entities.
## analytics.manage.users	View and manage user permissions for Analytics accounts.
## analytics.manage.users.readonly

baseUrl <- 'https://accounts.google.com/o/oauth2/auth'

parms <- list(
  scope = paste0('https://www.googleapis.com/auth/', scope),
  access_type = 'offline',
  approval_prompt = 'force',
  redirect_uri = 'http://localhost',
  response_type = 'code',
  client_id = client_id 
)

parms <- paste( paste0(names(parms),'=',parms), collapse = '&')

url <- paste0(baseUrl, '?', parms)
browseURL(url)

}