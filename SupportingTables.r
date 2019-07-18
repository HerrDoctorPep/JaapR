library('readxl')

HiDrive_location="D:/micro/Documents/New folder/HiDrive/RawData/"

#include postal code long/lat coordinates
# Using table from http://www.sqlblog.nl/postcodetabel-nederland-sql-script/

# postcode_geo <- read_xlsx(paste0(HiDrive_location,"postcodetabel/postcodetabel.xlsx")) %>%
#  filter(substring(PostCode,1,2) == "30") %>%
#  select(PostCode, Latitude, Longitude) %>%
#  group_by(PostCode) %>%
#  summarise(latitude = mean(Latitude),
#            longitude = mean(Longitude)) %>%
#  filter(!is.na(latitude) & !is.na(longitude))
# write_csv(postcode_geo,"postcode_roffa_geo.csv", na = "NA", append = FALSE, col_names = TRUE)

postcode_geo <- read_csv("postcode_roffa_geo.csv", col_names = TRUE)

# Tabel om huis-types beter analyseerbaar te maken

type_tabel <- read_csv("TypeTabel.csv", col_names=TRUE)

# Tabel met afstanden van postcode tot OV modaliteiten

# source("OVJaap.r")

PC_OV_distance <- read_csv("PC_OV_distance.csv",col_names = TRUE)
