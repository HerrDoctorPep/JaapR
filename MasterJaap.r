#
# Scraping Jaap.nl to get a grip on house prices in Rotterdam
#

# Load essential packages

library('rvest') # scraping
library('tidyverse') # data wrangling
library('stringr') # string manipulation
library('dplyr') # more tidy stuff
library('purrr') # more tidy stuff
library('tidyr') # more tidy stuff
library('lubridate') # more tidy stuff
library('caret') # Machine learning wrapper


# Include support tables and load cleaning/scraping functions

source("SupportingTables.r")
source("JaapScraper.r")
source("JaapDeepScraper.r")

##
## Scrape Jaap.nl and organize data
##

# Set-up parameters for scraping manually

mainPage <- 'https://www.jaap.nl/koophuizen/zuid+holland/groot-rijnmond/rotterdam/50+-woonopp/'
maxPage <- 48 # encoded within DOM node ".page-info"onmain page; still to be automated
TODAY = "20190525" # Format: YYYYMMDD

# Scrape all summary pages and write to file and write to disc using JaapScraper.r

scrape_summary_pages(mainPage,maxPage,TODAY)

# read scraped summary pages,bring to analyzable format, and write to disc

huizen_html <- read_csv(paste0("huizen_",TODAY,".csv"))
huizen_data <- clean_summary(huizen_html)
rm(huizen_html)

# scrape underlying pageswith details, using JaapDeepScraper.r
# Note: does not yet handle if underlying page is missing... in that case restart loop with h <- h + 1

for(h in 1:nrow(huizen_data)){
  print(paste0("scraping house number...",h))
  newline_html <- scrape_detail(huizen_data$id[h])
  
  if(h==1){
    detail_html<- newline_html
  } else{
    detail_html <- detail_html %>% 
      bind_rows(newline_html) 
  }
}

write_csv(detail_html, path=paste0("detail_",TODAY,".csv"), na = "NA", append = FALSE, col_names = TRUE)

# read detailed data from file andbring to analyzable format

detail_html <- read_csv(paste0("detail_",TODAY,".csv"))
detail_data <- clean_detail(detail_html)
rm(detail_html)

# Combine summary and detail into data set for madelling using JaapDeepScraper.r

model_data <- combine_summary_detail (summary_data,detail_data)
rm(summary_data,detail_data)

##
## Modelling 
##

# split in train and test sets

id_train <- createDataPartition(model_data$prijs, p=0.7, list=FALSE) # Alternative based on Caret tutorial
model_data_train <- model_data[id_train,]
model_data_test <- model_data[-id_train,]

# Two Log-linear models

source("CaretGLMJaap.r") # Simple multiplicative andmulti-level models are equally good; MAPE = c. 14.0%

# Two ppm2 models

source("CaretGLM2Jaap.r") # Simple multiplicative andmulti-level models are equally good; MAPE =c. 14.7%

# TwoXGB models

source("CaretXGBJaap.r") # Simple multiplicative andmulti-level models are equally good; MAPE = c. 14.2%

#
# Check expected asking prices 
# Inputs: address, postal code, # rooms,  #m², house type
#

HuisCheck("Hooidrift 14","3023KP",11,300,"Woning",Bouwjaar=1909)
HuisCheck("Klaverstraat 49","3083VB",7,410,"Villa", Bouwjaar=2000,Tuin="Ja",Balkon="Ja",Garage="Ja")
HuisCheck("Noordsingel 193","3035ER",6,250,"Woning",Bouwjaar=1890,Tuin="Ja")
HuisCheck("Grote Visserijstraat 18A","3026CK",6,150,"Appartement",Bouwjaar=1952,Balkon="Ja")
HuisCheck("Voorhaven 34AB","3024RN",8,222,"Herenhuis",Bouwjaar=max(1649,1870)) # 1649 is a new level for Period variable
HuisCheck("Van der Hilstraat 123","3023PK",10,225,"Woning",Bouwjaar=2019,Balkon="Ja")

