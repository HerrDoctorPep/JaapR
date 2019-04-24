# First scraping exercise
# Based on https://www.analyticsvidhya.com/blog/2017/03/beginners-guide-on-web-scraping-in-r-using-rvest-with-hands-on-knowledge/
#Starting with funda.nl/koop/Rotterdam

install.packages('rvest')
install.packages('tidyverse')
install.packages('stringr')
install.packages('purrr')
install.packages('tidyr')

#Loading the rvest package
library('rvest')

#Specifying the url for desired website to be scraped
base_url <- 'https://www.funda.nl/koop/rotterdam/'
url <- base_url

#Reading the HTML code from the website
webpage <- read_html(url)

# Reading the number of pages with homes
maxPage <- 136

#
# The manipulations for the firstpage
#

# reading house characteristics

adres_html <- html_text(html_nodes(webpage,'.search-result-title'))
postcode_html <- html_text(html_nodes(webpage,'.search-result-subtitle'))
prijs_html <- html_text(html_nodes(webpage,'.search-result-price'))
woonopp_html <- html_text(html_nodes(webpage,'.search-result-kenmerken span:nth-child(1)'))
perseelopp_html <- html_text(html_nodes(webpage,'.search-result-kenmerken span+ span'))
kamers_html <- html_text(html_nodes(webpage,'.search-result-kenmerken li+ li'))

# put everything in a tibble

library('tidyverse')
library('stringr')
library('dplyr')
library('purrr')
library('tidyr')

huizen_html <- tibble(adres = adres_html,
       postcode = postcode_html,
       prijs = prijs_html,
       woonopp = woonopp_html,
       perseelopp = perseelopp_html,
       kamers = kamers_html)

# we don't need the initial vectors anymore

rm(webpage, url, adres_html,postcode_html,prijs_html,woonopp_html,perseelopp_html,kamers_html)


#for (i in 2:maxPage){
i=3
url <- paste(base_url,"p",as.character(i),sep="")
webpage <- read_html(url)

adres_html <- html_text(html_nodes(webpage,'.search-result-title'))
postcode_html <- html_text(html_nodes(webpage,'.search-result-subtitle'))
prijs_html <- html_text(html_nodes(webpage,'.search-result-price'))
woonopp_html <- html_text(html_nodes(webpage,'.search-result-kenmerken span:nth-child(1)'))
#perseelopp_html <- html_text(html_nodes(webpage,'.search-result-kenmerken span+ span'))
kamers_html <- html_text(html_nodes(webpage,'.search-result-kenmerken li+ li'))

meerHuizen_html <- tibble(adres = adres_html,
                          postcode = postcode_html,
                          prijs = prijs_html,
                          woonopp = woonopp_html,
                          #perseelopp = perseelopp_html,
                          kamers = kamers_html)
huizen_html <- bind_rows(huizen_html,meerHuizen_html)
#}

# Getting to analyzable data

huizen <- huizen_html %>%
  rowwise() %>%
  mutate(adres = trimws(strsplit(adres,'\r\n')[[1]][2])) %>%
  mutate(postcode = substring(postcode, 23,29)) %>%
  mutate(prijs = as.integer(str_replace_all(substring(trimws(prijs), 3,nchar(prijs)-5),"\\.","")))%>%
  mutate(woonopp = as.integer(substring(trimws(woonopp), 1,nchar(woonopp)-3))) %>%
  #mutate(perseelopp = as.integer(substring(trimws(perseelopp), 1,nchar(perseelopp)-3))) %>%
  mutate(kamers = as.integer(substring(trimws(kamers), 1,nchar(kamers)-7)))

#  run through other pages to get all houses





