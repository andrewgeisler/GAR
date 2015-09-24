# get the latest installr package:
if (!require('devtools')) install.packages('devtools'); require('devtools')
install_github('installr', 'talgalili')
require(installr)


RStudio_CRAN_data_folder <- download_RStudio_CRAN_data(START = '2015-07-28', END = Sys.Date()-1) # around the time R 3.0.0 was released

my_RStudio_CRAN_data <- read_RStudio_CRAN_data(RStudio_CRAN_data_folder)


barplot_package_users_per_day("GAR", my_RStudio_CRAN_data)

