#
# Scraping jaap.nllistings for Rotterdam
#
# Based on https://www.analyticsvidhya.com/blog/2017/03/beginners-guide-on-web-scraping-in-r-using-rvest-with-hands-on-knowledge/
#

#Loading the packages
library('rvest') # scraping
library('tidyverse') # data wrangling
library('stringr') #string manipulation
library('dplyr') #more tidy stuff
library('purrr')
library('tidyr')

# Check manually how many pages there are
maxPage <- 46

for (p in 1:maxPage){
  urli <- paste('https://www.jaap.nl/koophuizen/zuid+holland/groot-rijnmond/rotterdam/50+-woonopp/p',as.character(p),sep="")
  webpagei <- read_html(urli)
  
  adres_html <- html_text(html_nodes(webpagei,'.property-address-street'))
  postcode_html <- html_text(html_nodes(webpagei,'.property-address-zipcity'))
  type_html <- html_text(html_nodes(webpagei,'.property-features div:nth-child(1)')) # let op features schuiven door
  kamers_html <- html_text(html_nodes(webpagei,'.property-features div:nth-child(2)')) # let op features schuiven door
  m2_html <- html_text(html_nodes(webpagei,'.property-features div:nth-child(3)')) # let op features schuiven door
  price_html <- html_text(html_nodes(webpagei,'.property-price'))
  pricetype_html <- html_text(html_nodes(webpagei,'.pricetype'))
  # price_info <- html_text(html_nodes(webpage1,'.price-info span'))
  
  #length 
  
  print(length(type_html))
  print(length(kamers_html))
  print(length(m2_html))
  
  #correct flaws

  for(i in 1:length(type_html)){
    if(str_detect(type_html[i],"kamers") & str_detect(kamers_html[i],"m²")){
      m2_html <- append(m2_html,kamers_html[i],after = i-1)
      kamers_html[i] <- append(kamers_html,type_html[i],after = i-1)
      type_html[i] <- NA
    } else if(str_detect(type_html[i],"m²")){
      m2_html <- append(m2_html,type_html[i],after = i-1)
      kamers_html <- append(kamers_html,NA,after = i-1)
      type_html[i] <- NA
    } else if(str_detect(kamers_html[i],"m²")){
    m2_html <- append(m2_html,kamers_html[i],after = i-1)
    kamers_html[i] <- NA
    }
  }
  
  # write tibble
  
  if(p==1){
    huizen_html <- bind_rows(tibble(adres = adres_html,
                                    postcode = postcode_html,
                                    type = type_html,
                                    m2 = m2_html,
                                    kamers = kamers_html,
                                    prijs = price_html,
                                    prijstype = pricetype_html))
  } else{
    huizen_html <- huizen_html %>% bind_rows(tibble(adres = adres_html,
                                                    postcode = postcode_html,
                                                    type = type_html,
                                                    m2 = m2_html,
                                                    kamers = kamers_html,
                                                    prijs = price_html,
                                                    prijstype = pricetype_html)) 
  }
}

# write to file; just in case

write_csv(huizen_html, path="huizen_TODAY.csv", na = "NA", append = FALSE, col_names = TRUE)

# make numbers; for analysis purposes

huizen_data <- huizen_html %>%
  mutate(prijs = as.integer(str_replace_all(substring(trimws(huizen_html$prijs), 3,nchar(huizen_html$prijs)-nchar(prijstype)-1),"\\.",""))) %>%
  mutate(m2 = as.integer(substring(trimws(m2), 1,nchar(m2)-3))) %>%
  mutate(kamers = as.integer(substring(trimws(kamers), 1,nchar(kamers)-7))) %>%
  mutate(postcode4 = as.integer(substring(postcode,1,4))) %>%
  mutate(postcode6 = str_replace_all(substring(postcode,1,7)," ","")) %>%
  mutate(prijspm2 = prijs / m2)

# exclude some very big or expensive houses

huizen_html[which(huizen_data$m2>500),] %>%
  select('adres', 'prijs', 'm2', 'kamers', 'type', 'prijstype')

huizen_html[which(huizen_data$prijs>2e6),] %>%
  select('adres', 'prijs', 'm2', 'kamers', 'type', 'prijstype')

huizen_html[which(huizen_data$prijs<1e5),] %>%
  select('adres', 'prijs', 'm2', 'kamers', 'type', 'prijstype')


huizen_clean <- huizen_data %>%
  filter(m2<500 & m2 >50) %>%
  filter(prijs<2e6 & prijs>50000) %>%
  filter(!is.na(postcode4))

#include postal code long/lat coordinates
# Using table from http://www.sqlblog.nl/postcodetabel-nederland-sql-script/

library('readxl')
postcode_data <- read_xlsx("C:/Users/micro/HiDrive/RawData/postcodetabel/postcodetabel.xlsx")
postcode_data <- postcode_data %>%
  select(PostCode, Latitude, Longitude) %>%
  group_by(PostCode) %>%
  summarise(Latitude = mean(Latitude),
            Longitude = mean(Longitude))

huizen_clean <- huizen_clean %>%
  left_join(postcode_data,by=c("postcode6"="PostCode")) %>%
  mutate(prijspm2 = prijs / m2) %>%
  filter(!is.na(Longitude))


# Simplify types

type_tabel <- tibble(type_complex = unique(huizen_clean$type)) %>%
  mutate(type_simpel = ifelse(type_complex=="Bovenwoning" | type_complex=="Dubbele bovenwoning" | type_complex=="Benedenwoning","Appartement",
                              ifelse(type_complex=="Maisonette" | type_complex=="Studio" | type_complex =="Kamer" | type_complex == "Appartement","Appartement",
                                     ifelse(type_complex=="Penthouse", "Penthouse",
                                            ifelse(type_complex=="Woonboot", "Woonboot",
                                                   ifelse(type_complex=="Garage", "Garage",
                                                          ifelse(is.na(type_complex),"Onbekend","Huis")))))))
huizen_clean <- huizen_clean %>%
  mutate(logprijs = log(prijs),
         kamergrootte = m2/kamers,
         postcode3 = as.factor(floor(postcode4/10)*10)) %>%
  mutate(postcode4 = as.factor(postcode4)) %>%
  mutate(m2xm2 = m2 * m2) %>%
  right_join(type_tabel,by=c("type"="type_complex")) %>%
  filter(!is.na(kamergrootte) & !is.infinite(kamergrootte))

huizen_clean$type_simpel[which(is.na(huizen_clean$type_simpel))] <- "Onbekend"

# Write the clean data set to file

write_csv(huizen_clean,path ="huizen_clean_20190424.csv", na = "NA", append = FALSE, col_names = TRUE)

# Try mapping stuff

# Create table by postal code

huizen_bypc <- huizen_clean %>%
  group_by(postcode6) %>%
  summarise(m2_avg = mean(m2),
            prijs_avg = mean(prijs),
            kamers_avg = mean(kamers),
            Longitude = first(Longitude),
            Latitude = first(Latitude)) %>%
  mutate(prijspm2_avg = prijs_avg / m2_avg,
         m2pkamer = m2_avg / kamers_avg)


library(ggmap)
library(maps)
library(mapdata)

GoogleAPI_key <- read_file("Google_key.txt")

ggplot() + 
  geom_polygon(data = map_data("world", region="netherlands"), aes(x=long, y = lat, group = group),colour="grey",fill="lightgrey") + 
  coord_fixed(1.3) +
  geom_point(data = huizen_forplot, aes(x = Longitude, y = Latitude,colour=log(prijs_avg)), size = 1) +
  labs(title = "Huizenprijzen in Rotterdam", x="Longitude", y="Lattitude")


mean(huizen_forplot$Longitude[!is.na(huizen_forplot$Longitude)])
mean(huizen_forplot$Latitude[!is.na(huizen_forplot$Latitude)])

register_google(GoogleAPI_key)
(map_Roffa13 <- get_googlemap(c(4.48,51.915), zoom=13))

ggmap(map_Roffa13) +
  geom_point(data = huizen_forplot, aes(x = Longitude, y = Latitude,colour=log(prijs_avg)), size = 2) +
  labs(title = "Huizenprijzen in Rotterdam", x="Longitude", y="Lattitude")

ggmap(map_Roffa13) +
  geom_point(data = huizen_forplot, aes(x = Longitude, y = Latitude,colour=prijspm2_avg), size = 2) +
  labs(title = "Vierkantemeterprijzen in Rotterdam", x="Longitude", y="Lattitude")

ggmap(map_Roffa13) +
  geom_point(data = subset(huizen_forplot,m2_avg>100), aes(x = Longitude, y = Latitude,colour=prijspm2_avg), size = 2) +
  labs(title = "Vierkantemeterprijzen in Rotterdam (m²>100)", x="Longitude", y="Lattitude")

ggmap(map_Roffa13) +
  geom_point(data = huizen_forplot, aes(x = Longitude, y = Latitude,colour=m2_avg), size = 2) +
  labs(title = "Woninggrootte in Rotterdam", x="Longitude", y="Lattitude")


