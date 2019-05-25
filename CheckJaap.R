

# Apply models fit4 and fit6 to check how realistic asking prices are

HuisCheck <- function(adresHuis, postcode6, Kamers, Woonoppervlakte, Bouwjaar,
                      Type ="Appartement", 
                      Garage = "-",
                      Tuin = "-",
                      Balkon = "-",
                      Isolatie = "-",
                      Keuken = "-",
                      Bijzonderheden = "-"){
  Huis <- matrix(NA,nrow=1,ncol=33)
  colnames(Huis) <- predictors_GLM
  Huis[1,'postcode4'] <- substring(postcode6,1,4)
  Huis[1,'Kamers'] <- Kamers
  Huis[1,'Woonoppervlakte'] <- Woonoppervlakte
  Huis[1,'m2xm2'] <- Woonoppervlakte * Woonoppervlakte
  Huis[1,'Type'] <- Type
  Huis[1,'Bouwjaar'] <- Bouwjaar
  Huis[1,'Period'] <- round((Bouwjaar-10)/20,0)*20+10
  Huis[1,'Tuin'] <- Tuin
  Huis[1,'Balkon'] <- Balkon
  Huis[1,'Keuken'] <- Keuken
  Huis[1,'Isolatie'] <- Isolatie
  Huis[1,'Bijzonderheden'] <- Bijzonderheden
  
  # Huis[1,'longitude'] <- huizen_data %>% filter(postcode6 = postcode6) %>% summarise(first(longitude))
  # Huis[1,'latitude'] <- huizen_data %>% filter(postcode6 = postcode6) %>% summarise(first(latitude))
  
  Huis <- as.tibble(Huis)
  
  Huis <- utils::type.convert(Huis) %>%
    mutate(Period = as.factor(Period))
  
  huizen_data %>%
    filter(postcode4==Huis$postcode4) %>%
    select(adres,type_complex,postcode6, m2, kamers, prijs) %>%
    print(n=Inf)
  
  # GLM 
  
  Huis_test <- predict(normalize_model,newdata = Huis)
  Huis_GLM <- exp(predict(model_logGLM, newdata = Huis_test))  
  print(paste0(adresHuis,". Estimate 1 (GLM): ",round(Huis_GLM,-3)))
  
  Huis_MLM <- exp(predict(model_MLM, newdata = Huis_test))
  print(paste0(adresHuis,". Estimate 1 (MLM): ",round(Huis_MLM,-3)))

}
