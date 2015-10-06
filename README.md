#GAR - Google Analytics R
##Instructions for use.

The package is designed to help easily retreive data from Google Analytics. 

Use of the package assumes the user has a Google Analytics accounts as well as an 'installed application' project in the Google API/Developer console. 


Set Up How To:
* Create API project in Google console
  * Log into Google API/Developer console
  * console.developers.google.com
  * Create New Project - Name whatever you'd like.
* Set up project
  * Navigate into project and select APIs & auth from left nav
  * Under API's enable Analytics API
  * Select Credentials
  * Create new client ID > select installed application
  * Copy client id and secret into R
* Authenticate
  * Use the getCode() to authenticate. This will open up a browswer window and display a code= in the URL bar. Copy this to R.
  * Use the getRefresh() function with the code from previous set to retrieve and long-lived refresh token. Save refresh token for continual use.
* Get Data
  * Always request an updated access token using the tokenRefresh() with the refresh token saved from before.
  * Use gaRequest() with the access token to retreive analytics data
* Get Core Reporting Dimensions and Metrics 
  * Use getMeta() to retrieve a list of dimensions and metrics availbe in the Core Reporting API.
