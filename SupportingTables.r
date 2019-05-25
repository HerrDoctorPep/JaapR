library('readxl')

#include postal code long/lat coordinates
# Using table from http://www.sqlblog.nl/postcodetabel-nederland-sql-script/

postcode_geo <- read_xlsx("C:/Users/micro/HiDrive/RawData/postcodetabel/postcodetabel.xlsx") %>%
  select(PostCode, Latitude, Longitude) %>%
  group_by(PostCode) %>%
  summarise(latitude = mean(Latitude),
            longitude = mean(Longitude)) %>%
  filter(!is.na(latitude) & !is.na(longitude))


# Tabel om huis-types beter analyseerbaar te maken

type_tabel <- read_csv("TypeTabel.csv", col_names=TRUE)
