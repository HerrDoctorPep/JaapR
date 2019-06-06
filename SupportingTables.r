library('readxl')

#include postal code long/lat coordinates
# Using table from http://www.sqlblog.nl/postcodetabel-nederland-sql-script/

# postcode_geo <- read_xlsx("D:/micro/HiDrive/RawData/postcodetabel/postcodetabel.xlsx") %>%
#   filter(substring(PostCode,1,1) == "3") %>%
#   select(PostCode, Latitude, Longitude) %>%
#   group_by(PostCode) %>%
#   summarise(latitude = mean(Latitude),
#             longitude = mean(Longitude)) %>%
#   filter(!is.na(latitude) & !is.na(longitude))
#
# write_csv(postcode_geo,"postcode_roffa_geo.csv", na = "NA", append = FALSE, col_names = TRUE)

postcode_geo <- read_csv("postcode_roffa_geo.csv", col_names = TRUE)

# Tabel om huis-types beter analyseerbaar te maken

type_tabel <- read_csv("TypeTabel.csv", col_names=TRUE)

