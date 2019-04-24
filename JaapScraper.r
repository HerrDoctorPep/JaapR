#Loading the rvest package
library('rvest')

#Specifying the url for desired website to be scraped
url1 <- 'https://www.jaap.nl/koophuizen/zuid+holland/groot-rijnmond/rotterdam'

#Reading the HTML code from the website
webpage1 <- read_html(url1)

adres_html <- html_text(html_nodes(webpage1,'.property-address-street'))
postcode_html <- html_text(html_nodes(webpage1,'.property-address-zipcity'))
type_html <- html_text(html_nodes(webpage1,'.property-features div:nth-child(1)')) # let op features schuiven door
kamers_html <- html_text(html_nodes(webpage1,'.property-features div:nth-child(2)')) # let op features schuiven door
m2_html <- html_text(html_nodes(webpage1,'.property-features div:nth-child(3)')) # let op features schuiven door
price_html <- html_text(html_nodes(webpage1,'.property-price'))
pricetype_html <- html_text(html_nodes(webpage1,'.pricetype'))
# price_info <- html_text(html_nodes(webpage1,'.price-info span'))

# missend woningtype fixen

missingType <- which(str_detect(type_html,"kamers") & str_detect(kamers_html,"m²"))

m2_html <- append(m2_html,kamers_html[missingType],after = missingType-1)
kamers_html[missingType] <- type_html[missingType]
type_html[missingType] <- NA

# Wegschrijven naar tibble

huizen <- tibble(adres = adres_html,
                 postcode = postcode_html,
                 type = type_html,
                 m2 = m2_html,
                 kamers = kamers_html,
                 prijs = price_html,
                 prijstype = pricetype_html)


# More pages


url2 <- 'https://www.jaap.nl/koophuizen/zuid+holland/groot-rijnmond/rotterdam/p2'
webpage2 <- read_html(url2)

adres_html <- html_text(html_nodes(webpage2,'.property-address-street'))
postcode_html <- html_text(html_nodes(webpage2,'.property-address-zipcity'))
type_html <- html_text(html_nodes(webpage2,'.property-features div:nth-child(1)')) # let op features schuiven door
kamers_html <- html_text(html_nodes(webpage2,'.property-features div:nth-child(2)')) # let op features schuiven door
m2_html <- html_text(html_nodes(webpage2,'.property-features div:nth-child(3)')) # let op features schuiven door
price_html <- html_text(html_nodes(webpage2,'.property-price'))
pricetype_html <- html_text(html_nodes(webpage2,'.pricetype'))
# price_info <- html_text(html_nodes(webpage1,'.price-info span'))

length(type_html)
length(kamers_html)
length(m2_html)

missingType <- which(str_detect(type_html,"kamers") & str_detect(kamers_html,"m²"))

m2_html <- append(m2_html,kamers_html[missingType],after = missingType-1)
kamers_html[missingType] <- type_html[missingType]
type_html[missingType] <- NA

huizen <- huizen %>% bind_rows(tibble(adres = adres_html,
                postcode = postcode_html,
                type = type_html,
                m2 = m2_html,
                kamers = kamers_html,
                prijs = price_html,
                prijstype = pricetype_html))

rm(huizen_html)

maxPage <- 47

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
  
  for(i in 1:1){
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

write_csv(huizen_html, path="huizen_html.csv", na = "NA", append = FALSE, col_names = TRUE)

# make numbers; for analysis purposes

huizen_data <- huizen_html %>%
  mutate(prijs = as.integer(str_replace_all(substring(trimws(huizen_html$prijs), 3,nchar(huizen_html$prijs)-nchar(prijstype)-1),"\\.",""))) %>%
  mutate(m2 = as.integer(substring(trimws(m2), 1,nchar(m2)-3))) %>%
  mutate(kamers = as.integer(substring(trimws(kamers), 1,nchar(kamers)-7))) %>%
  mutate(postcode4 = as.integer(substring(postcode,1,4))) %>%
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

ggplot() +
  ggtitle("Prijs naar woonoppervlak") +
  geom_point(data=huizen_clean,aes(m2,prijs,col=postcode4))

ggplot() +
  ggtitle("Prijs naar postcode4") +
  geom_point(data=huizen_clean,aes(postcode4,prijs,col=postcode4))

ggplot() +
  ggtitle("Prijs per m² naar woonoppervlak") +
  geom_point(data=huizen_clean,aes(log(m2),prijspm2,col=postcode4))

ggplot() +
  ggtitle("Prijs per m2 naar postcode4") +
  geom_point(data=huizen_clean,aes(postcode4,prijspm2,col=postcode4))

huizen_bypc <- huizen_clean %>%
  group_by('postcode4') %>%
  summarise(prijs_avg = mean(prijs),
            prijspm2_avg = mean(prijspm2))


ggplot() +
  ggtitle("Avg prijs per m2 naar postcode4") +
  geom_col(data=huizen_bypc,aes(postcode4,prijspm2_avg,col=postcode4))

