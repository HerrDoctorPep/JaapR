

# Apply models fit4 and fit6 to check how realistic asking prices are

HuisCheck <- function(adresHuis,postcode6Huis,kamersHuis,m2Huis,typeHuis){
  Huis <- tibble(adres = c(adresHuis))
  Huis$postcode6 <- c(as.character(postcode6Huis))
  Huis$postcode4 <- c(substring(postcode6Huis,1,4))
  Huis$postcode3 <- c(as.character(floor(as.numeric(Huis$postcode4)/10)*10))
  Huis$kamers <- c(kamersHuis)
  Huis$m2 <- c(m2Huis)
  Huis$m2xm2 <- c(m2Huis * m2Huis)
  Huis$type_complex <- c(typeHuis)
  
  XH <- rep(NA,length(fix4))
  XH[1] <- 1
  XH[2] <- Huis$m2[1]
  for(j in pos_type_complex) {
    XH[j] <- paste0("type_complex",Huis$type_complex[1]) == names(fix4)[j]
  }
  XH[length(pos_type_complex)+3] <- Huis$kamers[1]
  XH[length(pos_type_complex)+4] <- Huis$m2xm2[1]
  XH[length(pos_type_complex)+5] <- Huis$m2[1] * Huis$kamers[1]

  pred4_f <- sum(XH * fix4)

  pred4_pc <- ran4$postcode4[as.character(Huis$postcode4[1]),]
  
  huizen_data %>%
    filter(postcode4==Huis$postcode4) %>%
    select(adres,type_complex,postcode6, m2, kamers, prijs) %>%
    print(n=Inf)
  
  Huis4 <- exp(pred4_f + ifelse(!is.na(pred4_pc),pred4_pc,0))
  print(paste0(Huis$adres[1],". Estimate 1 (fit4): ",round(Huis4,-3)))
  
  Huis2 <- exp(predict(fit2,Huis))
  print(paste0(Huis$adres[1],". Estimate 2 (fit2): ",round(Huis2,-3)))
  
  # # Add XG
  # 
  # HuisXG <- Huis[,c("m2","m2xm2", "kamers", "postcode4")]%>%
  #   mutate(postcode4 = as.numeric(as.character(postcode4)))
  # 
  # LatLon <- postcode_data %>%
  #   filter(PostCode == Huis$postcode6)
  # 
  # HuisXG$Latitude <-LatLon$Latitude    
  # HuisXG$Longitude <-LatLon$Longitude
  # 
  # DummyH <- matrix(NA, nrow = 1, ncol = length(type_simp))
  # 
  # for (i in 1:length(type_simp)){
  #   DummyH[,i]<- as.numeric(typeHuis == type_simp[i])
  # }
  # colnames(DummyH)<- type_simp
  # 
  # HuisXG <- HuisXG %>%
  #   mutate(postcode4 = as.numeric(as.character(postcode4))) %>%
  #   bind_cols(as.tibble(DummyH))
  # 
  # print(paste0(Huis$adres[1],". Estimate 3 (XG): ",round(predict(bst, as.matrix(HuisXG)),-3)))
  
}
