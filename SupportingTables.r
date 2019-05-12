library('readxl')

#include postal code long/lat coordinates
# Using table from http://www.sqlblog.nl/postcodetabel-nederland-sql-script/

PostCodeGeo <- function(){
  postcode_data <- read_xlsx("C:/Users/micro/HiDrive/RawData/postcodetabel/postcodetabel.xlsx") %>%
    select(PostCode, Latitude, Longitude) %>%
    group_by(PostCode) %>%
    summarise(latitude = mean(Latitude),
              longitude = mean(Longitude)) %>%
    filter(!is.na(latitude) & !is.na(longitude))
  return(postcode_data)
}

# Tabel om huis-types beter analyseerbaar te maken

type_tabel <- read_csv("TypeTabel.csv", col_names=TRUE)
