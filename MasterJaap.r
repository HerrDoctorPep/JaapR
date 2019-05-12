#
# Scraping Jaap.nl to get a grip on house prices in Rotterdam
#

# Load essential packages

# install.packages(c('rvest',
#                    'tidyverse',
#                    'stringr',
#                    'dplyr',
#                    'purrr',
#                    'tidyr'))

library('rvest') # scraping
library('tidyverse') # data wrangling
library('stringr') # string manipulation
library('dplyr') # more tidy stuff
library('purrr') # more tidy stuff
library('tidyr') # more tidy stuff

# Include support table

source("SupportingTables.r")
postcode_geo <- PostCodeGeo() # Create postal code geo table: all unique postcode6 with geo coordinates 



# Set-up parameters for scraping manually

mainPage <- 'https://www.jaap.nl/koophuizen/zuid+holland/groot-rijnmond/rotterdam/50+-woonopp/'
maxPage <- 47
TODAY = "20190511" # Format: YYYYMMDD

# Scrape all summary pages and write to file
source("JaapScraper.r")

for (p in 1:maxPage){
  if(p==1){
    huizen_html <- scrape_summary(mainPage,p) # function defined in JaapScraper.r
  } else{
    huizen_html <- huizen_html %>% 
      bind_rows(scrape_summary(mainPage,p)) 
  }
}

# Write/read; just in case

# write_csv(huizen_html, path=paste0("huizen_",TODAY,".csv"), na = "NA", append = FALSE, col_names = TRUE)
huizen_html <- read_csv("huizen_20190511.csv")

# Bring into analyzable format
huizen_data <- clean_summary(huizen_html)

# create train, test and validation sets

sample_id <- sample(list("train","validate","test"), size=nrow(huizen_data), prob=c(0.7,0.15,0.15), replace = TRUE)

# Time to fit some models

source("RegressionJaap.r") # fit2 wins
source("MultilevelJaap.r") # fit4 wins
# Note: other model types still have to cleaned-up 

# Check out the house types on which the model was trained
unique(huizen_train$type_complex)

# Check expected asking prices 
# Inputs: address, postal code, # rooms,  #m², house type
#

HuisCheck("Hooidrift 14","3023KP",11,300,"Tussenwoning")
HuisCheck("Klaverstraat 49","3083VB",7,410,"Villa")
HuisCheck("Noordsingel 193","3035ER",6,250,"Woning")
HuisCheck("Grote Visserijstraat 18A","3026CK",6,150,"Appartement")
HuisCheck("Delftweg 82-82","3043CH",4,306,"Woning")
HuisCheck("Voorhaven 34AB","3024RN",8,222,"Herenhuis")

