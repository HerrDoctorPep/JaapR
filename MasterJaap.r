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
library('lme4') # simple multilevel model stuff
library('readxl') # read xlsx files

# Include support tables and load cleaning/scraping functions

source("SupportingTables.r")
source("JaapScraper.r")
source("JaapDeepScraper.r")

##
## Scrape Jaap.nl and organize data
##

# Set-up parameters for scraping manually

mainPage <- 'https://www.jaap.nl/koophuizen/zuid+holland/groot-rijnmond/rotterdam/50+-woonopp/'
trimws(html_text(html_node(read_html(mainPage),'.page-info'))) # find max number of pages

maxPage <- 46 # still to be automated
TODAY = "20190601" # Format: YYYYMMDD

# Scrape all summary pages and write to file and write to disc using JaapScraper.r

scrape_summary_pages(mainPage,maxPage,TODAY)

# read scraped summary pages,bring to analyzable format, and write to disc

huizen_html <- read_csv(paste0("huizen_",TODAY,".csv"))
huizen_data <- clean_summary(huizen_html)
rm(huizen_html)

##
## scrape underlying pageswith details, using JaapDeepScraper.r
## 

for(h in 1:nrow(huizen_data)){
  tryCatch({
    print(paste0("scraping house number...",h))
    newline_html <- scrape_detail(huizen_data$id[h])
  
    if(h==1){
      detail_html<- newline_html
    } else{
      detail_html <- detail_html %>% 
        bind_rows(newline_html) 
    }
  }, error=function(e){cat("ERROR in house number",h,"\n")})
}

write_csv(detail_html, path=paste0("detail_",TODAY,".csv"), na = "NA", append = FALSE, col_names = TRUE)

detail_html <- read_csv(paste0("detail_",TODAY,".csv"))
detail_data <- clean_detail(detail_html)
rm(detail_html)

# Combine summary and detail into data set for madelling using JaapDeepScraper.r

model_data <- combine_summary_detail (huizen_data,detail_data)

##
## Modelling 
##

# split in train and test sets

id_train <- createDataPartition(model_data$prijs, p=0.7, list=FALSE) # Alternative based on Caret tutorial
model_data_train <- model_data[id_train,]
model_data_test <- model_data[-id_train,]

# Two Log-linear models

source("CaretGLMJaap.r") # Simple multiplicative andmulti-level models are equally good c. 0.2%pt difference in MAPE

# # Two ppm2 models
# 
# source("CaretGLM2Jaap.r") # Do not perform as well as log-linear model


# # Two models with some variables modeled separately, based on the errors
# 
# source("CaretGLM+Jaap.r") # Works almost as good as the log-linear regression, but not completely

# TwoXGB models

source("CaretXGBJaap.r") # Simple multiplicative andmulti-level models are equally good; MAPE = c. 14.2%

#
# Check expected asking prices 
#

source("CheckJaap.r") # create supporting functions to get predictions for pre-specificed houses

# Create empty test set
Huis_test <- model_data_test %>%
  filter(FALSE) %>%
  select(-c("prijs","logprijs","Oorspronkelijkevraagprijs", "Huidigevraagprijs","prijspm2","Aantalkeergetoond", "Aantalkeergetoondgisteren","Geplaatstop"))

# add lines

Huis_test <- Huis_test %>%
  bind_rows(ListHuis(list(id = "Grote Visserijstraat 18A",
                          postcode6 = "3026CK",
                          Kamers = 6,
                          Woonoppervlakte = 150,
                          Type = "Appartement",
                          Bouwjaar = 1952,
                          Balkon = "Ja")))
Huis_test <- Huis_test %>%
  bind_rows(ListHuis(list(id = "Klaverstraat 49",
                          postcode6 = "3083VB",
                          Kamers = 7,
                          Slaapkamers = 4,
                          Woonoppervlakte = 410,
                          Type = "Villa",
                          Bouwjaar = 2000,
                          Balkon = "Ja",
                          Garage="Ja",
                          Tuin="Ja")))
Huis_test <- Huis_test %>%
  bind_rows(ListHuis(list(id = "Van der Hilstraat 123",
                          postcode6 = "3023PK",
                          Kamers = 10,
                          Slaapkamers =8,
                          Woonoppervlakte = 225,
                          Type = "Woning",
                          Bouwjaar = 2019,
                          Balkon = "Ja")))
Huis_test <- Huis_test %>%
  bind_rows(ListHuis(list(id = "Havenstraat 146",
                          postcode6 = "3024TL",
                          Kamers = 9,
                          Slaapkamers =5,
                          Woonoppervlakte = 291,
                          Type = "Herenhuis",
                          Bouwjaar = 1900,
                          Inhoud = 1023,
                          Perceeloppervlakte = 152,
                          Tuin="Achtertuin"
                          )))


Huis_test <- bind_cols(Huis_test,HuisCheck(Huis_test))

Huis_test%>%
  select("id", pred_GLM, "pred_MLM", "pred_XGT", "pred_XGL") %>%
  print()

HuisReference("3024TL")
