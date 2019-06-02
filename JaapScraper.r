#
# Scraping jaap.nl listings for Rotterdam
#
# Based on https://www.analyticsvidhya.com/blog/2017/03/beginners-guide-on-web-scraping-in-r-using-rvest-with-hands-on-knowledge/
#

# Get summary page, scrape important fields, clean, and return in a tibble
# Broken down in three functions

# first scrape one page

scrape_summary <- function(mainPage,p){ 
  # get the page
  url <- paste0(mainPage,"p",as.character(p))
  webpage <- read_html(url)
  
  # get the fields
  adres_html <- html_nodes(webpage,'.property-address-street') %>%
    html_text()
  postcode_html <- html_nodes(webpage,'.property-address-zipcity') %>%
    html_text()
  # type_html <- html_nodes(webpage,'.property-features div:nth-child(1)') %>%
  #   html_text() # let op features schuiven door
  # kamers_html <- html_nodes(webpage,'.property-features div:nth-child(2)') %>%
  #   html_text() # let op features schuiven door
  # m2_html <- html_nodes(webpage,'.property-features div:nth-child(3)') %>%
  #   html_text() # let op features schuiven door
  price_html <- html_nodes(webpage,'.property-price') %>%
    html_text()
  pricetype_html <- html_nodes(webpage,'.pricetype') %>%
    html_text()
  link_html <- html_nodes(webpage,".property-inner") %>%
    html_attr("href")
  id_html <- html_nodes(webpage,".property") %>%
    html_attr("id") # Mind the ad placeholders and NAs
  
  # Print lengths of feature vectors as a check
  paste("+++++ Page: ",p," before fix +++++") %>% print()
  # paste("Type: ",length(type_html)) %>% print()
  # paste("Kamers: ",length(kamers_html)) %>% print()
  # paste("Oppervlak: ",length(m2_html)) %>% print()
  paste("IDs: ",length(id_html)) %>% print()
  paste("Link: ",length(link_html)) %>% print()
  
  # # Fix shifting values based on back-tracking logic
  # for(i in 1:length(type_html)){
  #   if(str_detect(type_html[i],"kamers") & str_detect(kamers_html[i],"m²")){
  #     m2_html <- append(m2_html,kamers_html[i],after = i-1)
  #     kamers_html[i] <- append(kamers_html,type_html[i],after = i-1)
  #     type_html[i] <- NA
  #   } else if(str_detect(type_html[i],"m²")){
  #     m2_html <- append(m2_html,type_html[i],after = i-1)
  #     kamers_html <- append(kamers_html,NA,after = i-1)
  #     type_html[i] <- NA
  #   } else if(str_detect(kamers_html[i],"m²")){
  #     m2_html <- append(m2_html,kamers_html[i],after = i-1)
  #     kamers_html[i] <- NA
  #   }
  # }
  
  id_clean <- str_detect(id_html,"house") & !is.na(id_html)
  link_html <- link_html[id_clean]
  id_html <- id_html[id_clean]
  
  # Print lengths of feature vectors as a check
  paste("----- Page: ",p," after fix -----") %>% print()
  # paste("Type: ",length(type_html)) %>% print()
  # paste("Kamers: ",length(kamers_html)) %>% print()
  # paste("Oppervlak: ",length(m2_html)) %>% print()
  # paste("IDs: ",length(id_html)) %>% print()
  paste("Link: ",length(link_html)) %>% print()
  
  huizen_html <- bind_rows(tibble(adres = adres_html,
                                  postcode = postcode_html,
                                  # type_complex = type_html,
                                  # m2 = m2_html,
                                  # kamers = kamers_html,
                                  prijs = price_html,
                                  prijstype = pricetype_html,
                                  link = link_html,
                                  id = id_html))
  return(huizen_html)
}

# Run through all pages and scrape those

scrape_summary_pages <- function(mainPage,maxPage,TODAY) {
  for (p in 1:maxPage){
    if(p==1){
      huizen_html <- scrape_summary(mainPage,p) # function defined in JaapScraper.r
    } else{
      huizen_html <- huizen_html %>% 
        bind_rows(scrape_summary(mainPage,p)) 
    }
  }
  # Write/read; just in case
  write_csv(huizen_html, path=paste0("huizen_",TODAY,".csv"), na = "NA", append = FALSE, col_names = TRUE)
}




# make numbers; for analysis purposes


clean_summary <- function(huizen_html){
  # mutate fields into analyzable format and exclude some extreme values
  
  print(paste0("Rows ingoing: ",nrow(huizen_html)))
  
  huizen_html <- huizen_html %>%
    mutate(prijs = as.integer(str_replace_all(substring(trimws(huizen_html$prijs), 3,nchar(huizen_html$prijs)-nchar(prijstype)-1),"\\.",""))) %>%
    mutate(prijstype = as.factor(prijstype)) %>%
    mutate(postcode4 = as.integer(substring(postcode,1,4))) %>%
    mutate(postcode6 = str_replace_all(substring(postcode,1,7)," ","")) %>%
    filter(prijs<2e6 & prijs>50000) %>%
    filter(!is.na(postcode6)) %>%
    mutate(logprijs = log(prijs)) %>%
    mutate(postcode4 = as.factor(postcode4))

  print(paste0("Rows with room count: ",nrow(huizen_html)))
  
  # join with postal code data
  huizen_html <- huizen_html %>%
    left_join(postcode_geo,by=c("postcode6"="PostCode")) %>%
    filter(!is.na(longitude) & !is.na(latitude))
  
  print(paste0("Rows with PC6 geo location: ",nrow(huizen_html)))
  
  # Create simplified type

  # huizen_html <- huizen_html %>%
  #   left_join(type_tabel,by=c("type_complex"="type_complex")) 
  
  # huizen_html$type_simpel[which(is.na(huizen_html$type_simpel))] <- "Onbekend"
  # huizen_html$type_complex[which(is.na(huizen_html$type_complex))] <- "Onbekend"
  
  print(paste0("Rows outgoing: ",nrow(huizen_html)))
  
  return(huizen_html)
}










