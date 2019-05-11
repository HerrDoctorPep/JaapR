

# Apply models fit4 and fit6 to check how realistic asking prices are

HuisCheck <- function(adresHuis,postcode6Huis,kamersHuis,m2Huis,typeHuis){
  Huis <- tibble(adres = c(adresHuis))
  Huis$postcode6 <- c(as.character(postcode6Huis))
  Huis$postcode4 <- c(substring(postcode6Huis,1,4))
  Huis$postcode3 <- c(as.character(floor(as.numeric(Huis$postcode4)/10)*10))
  Huis$kamers <- c(kamersHuis)
  Huis$m2 <- c(m2Huis)
  Huis$m2xm2 <- c(m2Huis * m2Huis)
  Huis$type_simpel <- c(typeHuis)
  
  Huis7_f <- fix7['(Intercept)'] + fix7['m2'] * Huis$m2[1] + fix7['kamers'] * Huis$kamers[1] + fix7['m2:kamers'] * Huis$m2[1] * Huis$kamers[1] + fix7['m2xm2'] * Huis$m2xm2[1]
  Huis7_pc <- Beta7_pc[Huis$postcode4[1],'(Intercept)'] + (Huis$type_simpel[1] == "Huis") * Beta7_pc[Huis$postcode4[1],'type_simpelHuis'] + (Huis$type_simpel[1] == "Penthouse") * Beta7_pc[Huis$postcode4[1],'type_simpelPenthouse'] + (Huis$type_simpel[1] == "Woonboot") * Beta7_pc[Huis$postcode4[1],'type_simpelWoonboot']
    
  huizen_clean %>%
    filter(postcode4==Huis$postcode4) %>%
    print()
  
  Huis7 <- exp(Huis7_f + ifelse(!is.na(Huis7_pc),Huis7_pc,0))
  print(paste0(Huis$adres[1],". Estimate 1 (fit7): ",round(Huis7,-3)))
  
  Huis1 <- exp(predict(fit1,Huis))
  print(paste0(Huis$adres[1],". Estimate 2 (fit1): ",round(Huis1,-3)))
  
  # Add XG
  
  HuisXG <- Huis[,c("m2","m2xm2", "kamers", "postcode4")]%>%
    mutate(postcode4 = as.numeric(as.character(postcode4)))
  
  LatLon <- postcode_data %>%
    filter(PostCode == Huis$postcode6)

  HuisXG$Latitude <-LatLon$Latitude    
  HuisXG$Longitude <-LatLon$Longitude

  DummyH <- matrix(NA, nrow = 1, ncol = length(type_simp))
  
  for (i in 1:length(type_simp)){
    DummyH[,i]<- as.numeric(typeHuis == type_simp[i])
  }
  colnames(DummyH)<- type_simp

  HuisXG <- HuisXG %>%
    mutate(postcode4 = as.numeric(as.character(postcode4))) %>%
    bind_cols(as.tibble(DummyH))
  
  print(paste0(Huis$adres[1],". Estimate 3 (XG): ",round(predict(bst, as.matrix(HuisXG)),-3)))
  
}

HuisCheck("Hooidrift 14","3023KP",11,300,"Huis")
HuisCheck("Klaverstraat 49","3083VB",7,410,"Huis")
HuisCheck("Noordsingel 193","3035ER",6,250,"Huis")
HuisCheck("Grote Visserijstraat 18A","3026CK",6,150,"Appartement")
