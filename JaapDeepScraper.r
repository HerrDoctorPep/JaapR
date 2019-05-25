#
# Scrape detailed info from the listings pagesoftheindividualhouses
#

scrape_detail <- function(house_id){
  click_through <- huizen_data$link[which(huizen_data$id == house_id)[1]]
  webpage <- read_html(click_through)
  
  # The simple characteristics
  short_html <- html_nodes(webpage,".short-description") %>%
    html_text()
  long_html <- html_nodes(webpage,"#long-description") %>%
    html_text()
  broker_html <- html_nodes(webpage,".detail-broker .broker-name") %>%
    html_text()
  
  short_html <- ifelse(length(short_html)==0,NA,short_html)
  long_html <- ifelse(length(long_html)==0,NA,long_html)
  broker_html <- ifelse(length(broker_html)==0,NA,broker_html)
  
  # The table with more detailed metrics
  kenmerk_html <- html_nodes(webpage,".no-dots")[1:26] %>%
    html_text()
  waarde_html <- html_nodes(webpage,".value")[1:26] %>%
    html_text()

  overige_html <- matrix(waarde_html,nrow=1)
  colnames(overige_html) <- kenmerk_html
  
  colnames(overige_html) <- gsub(" ","",overige_html)
  colnames(overige_html) <- gsub(")","",overige_html)
  colnames(overige_html) <- gsub("\\(","",overige_html)

  deep_html <- bind_rows(tibble(id = house_id,
                                short = short_html,
                                long = long_html,
                                broker = broker_html))%>%
    bind_cols(as.data.frame(overige_html))
}


clean_detail <- function(detail_html){
  # Intermediate step to get manageable names
  names(detail_html) <- names(detail_html) %>%
    str_replace_all(" ","") %>%
    str_replace_all("\\)","") %>%
    str_replace_all("\\(","")
  
  detail_html <- detail_html %>%
    mutate('short' = trimws(short),
           'long' = trimws(long),
           'Type' = trimws(Type),
           'Bouwjaar' = as.integer(Bouwjaar),
           'Woonoppervlakte' = as.integer(substring(trimws(Woonoppervlakte), 1,nchar(trimws(Woonoppervlakte))-3)),
           'Inhoud' = as.integer(substring(trimws(Inhoud), 1,nchar(trimws(Inhoud))-3)),
           'Perceeloppervlakte' = as.integer(substring(trimws(Perceeloppervlakte), 1,nchar(trimws(Perceeloppervlakte))-3)),
           'Bijzonderheden' = as.factor(trimws(Bijzonderheden)),
           'Isolatie' = as.factor(trimws(Isolatie)),
           'Tuin' = as.factor(trimws(Tuin)),
           'Uitzicht' = as.factor(trimws(Uitzicht)),
           'Balkon' = as.factor(trimws(Balkon)),
           'Garage' = as.factor(trimws(Garage)),
           'Keuken' = as.factor(trimws(Keuken)),
           'Kamers' = as.integer(Kamers),
           'Slaapkamers' = as.integer(Slaapkamers),
           'Verwarming' = as.factor(trimws(Verwarming)),
           'Sanitairevoorzieningen' = as.factor(trimws(Sanitairevoorzieningen)),
           'Staatonderhoud' = as.factor(trimws(Staatonderhoud)),
           'Staatonderhoud1' = as.factor(trimws(Staatonderhoud1)),
           'Energielabelgeschat' = as.factor(trimws(Energielabelgeschat)),
           'Energieverbruikgeschat' = as.factor(trimws(Energieverbruikgeschat)),
           'Staatschilderwerk' = as.factor(trimws(Staatschilderwerk)),
           'Geplaatstop' = dmy(trimws(Geplaatstop)),
           'Oorspronkelijkevraagprijs' = as.integer(str_replace_all(substring(trimws(Oorspronkelijkevraagprijs), 3,nchar(Oorspronkelijkevraagprijs)-1),"\\.","")),
           'Huidigevraagprijs' = as.integer(str_replace_all(substring(trimws(Huidigevraagprijs), 3,nchar(Huidigevraagprijs)-1),"\\.","")),
           'Aantalkeergetoond' = as.integer(trimws(Aantalkeergetoond)),
           'Aantalkeergetoondgisteren' = as.integer(trimws(Aantalkeergetoondgisteren))
           )
  
  return(detail_html)
}



#
# Functionfor combining summary and detail data sets
#

combine_summary_detail <- function(summary_data,detail_data){
  
  model_data <-  huizen_data %>%
    select(id,postcode4,longitude,latitude,prijstype,prijs,logprijs) %>%
    left_join(detail_data,by=c("id" = "id"))
  
  exclusion_lines <- which(is.na(model_data$Bouwjaar))
  exclusion_ids <- model_data$id[exclusion_lines]
  
  model_data <- model_data %>%
    filter(! id %in% exclusion_ids) %>%
    select(-long) %>%
    mutate(Period = as.factor(round((Bouwjaar-10)/20,0)*20+10)) %>%
    mutate(prijspm2 = prijs / Woonoppervlakte) %>%
    mutate(m2xm2 = Woonoppervlakte * Woonoppervlakte)
  
  return(model_data)
}

