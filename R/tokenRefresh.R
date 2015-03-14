tokenRefresh <-
function(client_id, client_secret, token) {
                response <- POST(
                                "https://accounts.google.com/o/oauth2/token",
                                body = list(
                                       "client_id"=client_id,
                                       "client_secret"=client_secret,
                                       "refresh_token"=token,
                                       "grant_type"="refresh_token"),
                                add_headers('Host'='accounts.google.com'), 
                                encode='form'
                                )
                response <- content(response)
                accessToken <- response$access_token
                return(accessToken)
                }
