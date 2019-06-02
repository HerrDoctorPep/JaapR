

# Apply models fit4 and fit6 to check how realistic asking prices are

ListHuis <- function(Characteristics) {
  Huis_test_line <- model_data_test %>%
    filter(FALSE) %>%
    select(-c("prijs","logprijs","Oorspronkelijkevraagprijs", "Huidigevraagprijs","prijspm2","Aantalkeergetoond", "Aantalkeergetoondgisteren","Geplaatstop")) %>%
    add_row(prijstype ="k.k.",
            short = "",
            broker = "",
            Bijzonderheden = "-",
            Isolatie = "-",
            Verwarming = "-",
            Energielabelgeschat = "-",
            Energieverbruikgeschat = "-",
            Staatonderhoud = "-",
            Sanitairevoorzieningen = "-",
            Keuken = "-",
            Staatonderhoud1 = "-",
            Staatschilderwerk = "-",
            Tuin = "-",
            Uitzicht = "-",
            Balkon = "-",
            Garage = "-",
            Perceeloppervlakte = 0
    )

  for (c in names(Characteristics)){
    if(c %in% colnames(Huis)){
      Huis_test_line[1,c] = Characteristics[c]
    }
  }
  Huis_test_line[1,'postcode4'] <- substring(Characteristics$postcode6,1,4)
  Huis_test_line[1,'longitude'] <- postcode_geo %>% filter(PostCode == Characteristics$postcode6) %>%select(longitude) %>% unlist()
  Huis_test_line[1,'latitude'] <- postcode_geo %>% filter(PostCode == Characteristics$postcode6) %>%select(latitude) %>% unlist()
  Huis_test_line[1,'Period'] <- as.factor(round((Characteristics$Bouwjaar-10)/20,0)*20+10)
  Huis_test_line[1,'m2xm2'] <- Characteristics$Woonoppervlakte^2
  Huis_test_line[1,'Plafondhoogte'] <- ifelse(is.null(Characteristics$Inhoud),2.50, Characteristics$Inhoud / Characteristics$Woonoppervlakte)
  Huis_test_line[1,'Slaapkamers'] <- ifelse(is.null(Characteristics$Slaapkamers),Characteristics$Kamers-1, Characteristics$Slaapkamers)
  Huis_test_line[1,'Inhoud'] <-   Huis_test_line[1,'Plafondhoogte'] *   Huis_test_line[1,'Woonoppervlakte']
  return(Huis_test_line)
}

HuisCheck <- function(Huis_input){
  # GLM 
  
  Huis_test <- predict(normalize_model,newdata = Huis_input)
  
  # MLM
  Y_test<- tibble(pred_GLM =exp(predict(model_logGLM, newdata = Huis_test)),
                  pred_MLM = exp(predict(model_MLM, newdata = Huis_test)))
                  
  
  #XGB
  Huis_test_mat <- as_tibble(predict(dummies_model, newdata = Huis_test)) %>%
    mutate(BijzonderhedenGemeubileerd = 0,
           BijzonderhedenLiftZwembad = 0)
  
  Y_test$pred_XGT <- predict(model_xgbT,Huis_test_mat) * Huis_input$Woonoppervlakte
  
  return(Y_test)
}
