#
# Mathematical modelling on scraped Jaap data
#
# Objective: predict price_html
# Loss function: "% off"
# Multiplicative regression model
# Predictors: type of property, # m², # rooms, location
# 
# Requires: JaapScraper.r output
#


type_tabel <- tibble(type_complex = unique(huizen_clean$type)) %>%
  mutate(type_simpel = ifelse(type_complex=="Benedenwoning","Appartement",
                              ifelse(type_complex=="Bovenwoning","Appartement",
                                     ifelse(type_complex=="Dubbele bovenwoning", "Appartement",
                                            ifelse(type_complex=="Maisonnette","Appartement",
                                                   ifelse(type_complex=="Penthouse", "Penthouse",
                                                          ifelse(type_complex=="Woonboot", "Woonboot",
                                                                 ifelse(type_complex=="Garage", "Garage", "Huis"))))))))
         
huizen_m1 <- huizen_clean %>%
  select(prijs, m2,kamers,postcode4,type) %>%
  mutate(logprijs = log(prijs),
         kamergrootte = m2/kamers,
         postcode3 = as.factor(round(postcode4,-1))) %>%
  mutate(postcode4 = as.factor(postcode4)) %>%
  mutate(m2xm2 = m2 * m2) %>%
  right_join(type_tabel,by=c("type"="type_complex")) %>%
  filter(!is.na(kamergrootte) & !is.infinite(kamergrootte))



# creat train, test and validation sets

sample_id <- sample(1:3, size=nrow(huizen_m1), prob=c(0.7,0.15,0.15), replace = TRUE)

huizen_train <- huizen_m1[sample_id==1,]
huizen_valid <- huizen_m1[sample_id==2,]
huizen_test <- huizen_m1[sample_id==3,]

# fit basic linear models

fit1 <- glm(logprijs ~ m2 + type_simpel + kamers, data = huizen_train)       
summary(fit1)

fit2 <- glm(logprijs ~ m2 + type_simpel + kamers + factor(postcode3) + kamers*m2 + m2xm2, data = huizen_train)
summary(fit2)

fit3 <- glm(logprijs ~ m2 + type_simpel + kamers + factor(postcode3), data = huizen_train)       
summary(fit3)

fit4 <- glm(logprijs ~ m2 + type_simpel + kamers + factor(postcode3) + kamers*m2, data = huizen_train)       
summary(fit4)

fit5 <- glm(logprijs ~ m2 + type_simpel + kamers + factor(postcode3) + kamers*m2 + factor(postcode3)*type_simpel, data = huizen_train)       
summary(fit5)

# restrict prediction to existing postcode4  (necessary for fit2)
# 
# postcodes_viable <- huizen_train %>%
#   group_by(postcode4) %>%
#   select(postcode4)
# 
# huizen_valid <- huizen_valid %>%
#   right_join(postcodes_viable)

# create predictions

predict1 <- exp(predict(fit1,newdata=huizen_valid))
predict2 <- exp(predict(fit2,newdata=huizen_valid))
predict3 <- exp(predict(fit3,newdata=huizen_valid))
predict4 <- exp(predict(fit4,newdata=huizen_valid))
predict5 <- exp(predict(fit5,newdata=huizen_valid))

plotdata <- tibble(prijs = huizen_valid$prijs,
                   pred1 = predict1,
                   pred2 = predict2,
                   pred3 = predict3,
                   pred4 = predict4,
                   pred5 = predict5) %>%
  mutate(delta1 = pred1 - prijs,
         delta2 = pred2 - prijs,
         delta3 = pred3 - prijs,
         delta4 = pred4 - prijs,
         delta5 = pred5 - prijs,
         pct1 = pred1/prijs-1,
         pct2 = pred2/prijs-1,
         pct3 = pred3/prijs-1,
         pct4 = pred4/prijs-1,
         pct5 = pred5/prijs-1)

# Now for simple multi-level models

library (lme4) # package for linear mixed effects model
library (broom) # tidy output of std R functions - especially useful to access fitted models
library (influence.ME) # contains se.fixef function

fit6 <- lmer(logprijs ~ m2 + kamers + kamers*m2 + (type_simpel|postcode4), data = huizen_train)
summary(fit6)

fix6 <- fixef(fit6)
ran6 <- ranef(fit6)

# adding m2 x m2 as predictor
fit7 <- lmer(logprijs ~ m2 + kamers + kamers*m2 + m2xm2 + (type_simpel|postcode4), data = huizen_train)
summary(fit7)

fix7 <- fixef(fit7)
ran7 <- ranef(fit7)

# Create prediction for fit6

Beta_pc <- ran6$postcode4[as.character(huizen_valid$postcode4),]

predict6_f <- fix6['(Intercept)'] + fix6['m2'] * huizen_valid$m2 + fix6['kamers'] * huizen_valid$kamers + fix6['m2:kamers'] * huizen_valid$m2 * huizen_valid$kamers
predict6_pc <- Beta_pc$'(Intercept)' + (huizen_valid$type_simpel == "Huis") * Beta_pc$'type_simpelHuis' + (huizen_valid$type_simpel == "Penthouse") * Beta_pc$'type_simpelPenthouse' + (huizen_valid$type_simpel == "Woonboot") * Beta_pc$'type_simpelWoonboot'

predict6 <- exp(predict6_f + ifelse(!is.na(predict6_pc),predict6_pc,0))

plotdata$pred6 <- predict6
plotdata$delta6 <- predict6 - plotdata$prijs
plotdata$pct6 <- predict6 / plotdata$prijs -1

# Create prediction for fit7

Beta7_pc <- ran7$postcode4[as.character(huizen_valid$postcode4),]

predict7_f <- fix7['(Intercept)'] + fix7['m2'] * huizen_valid$m2 + fix7['kamers'] * huizen_valid$kamers + fix7['m2:kamers'] * huizen_valid$m2 * huizen_valid$kamers + fix7['m2xm2'] * huizen_valid$m2 * huizen_valid$m2
predict7_pc <- Beta7_pc$'(Intercept)' + (huizen_valid$type_simpel == "Huis") * Beta7_pc$'type_simpelHuis' + (huizen_valid$type_simpel == "Penthouse") * Beta7_pc$'type_simpelPenthouse' + (huizen_valid$type_simpel == "Woonboot") * Beta7_pc$'type_simpelWoonboot'

predict7 <- exp(predict7_f + ifelse(!is.na(predict7_pc),predict7_pc,0))

plotdata$pred7 <- predict7
plotdata$delta7 <- predict7 - plotdata$prijs
plotdata$pct7 <- predict7 / plotdata$prijs -1


plotdata %>%
  filter(!is.na(predict5)) %>%
  summarise(me1 = mean(delta1),
            me2 = mean(delta2),
            me3 = mean(delta3),
            me4 = mean(delta4),
            me5 = mean(delta5),
            me6 = mean(delta6),
            me7 = mean(delta7))

plotdata %>%
  filter(!is.na(predict5)) %>%
  summarise(sme1 = sqrt(mean(delta1^2)),
            sme2 = sqrt(mean(delta2^2)),
            sme3 = sqrt(mean(delta3^2)),
            sme4 = sqrt(mean(delta4^2)),
            sme5 = sqrt(mean(delta5^2)),
            sme6 = sqrt(mean(delta6^2)),
            sme7 = sqrt(mean(delta7^2)))

plotdata %>%
  filter(!is.na(predict5)) %>%
  summarise(pe1 = mean(pct1),
            pe2 = mean(pct2),
            pe3 = mean(pct3),
            pe4 = mean(pct4),
            pe5 = mean(pct5),
            pe6 = mean(pct6),
            pe7 = mean(pct7))

ggplot() +
  geom_point(data=plotdata,aes(x=pred7,y=prijs,col="red")) +
  geom_point(data=plotdata,aes(x=pred2,y=prijs,col="blue")) +
  labs(title = "Prediction v. Prijs", x="Prijs", y="Prediction") +
  coord_cartesian(xlim = c(0, 1200000)) 

ggplot() +
  geom_point(data=plotdata,aes(x=prijs,y=delta7,col="red")) +
  geom_point(data=plotdata,aes(x=prijs,y=delta2,col="blue")) + 
  labs(title = "Prijs v. Residual", x="Prijs", y="Residu")  +
  coord_cartesian(xlim = c(0, 1200000))

ggplot() +
  geom_point(aes(x=huizen_valid$m2,y=(predict6 - huizen_valid$prijs),col="red")) +
  geom_point(aes(x=huizen_valid$m2,y=(predict5 - huizen_valid$prijs),col="blue")) +
  labs(title = "Oppervlak v. Residual", x="m2", y="Residu")

  
# Apply models fit4 and fit6 to check how realistic asking prices are

HuisCheck <- function(adresHuis,postcode4Huis,kamersHuis,m2Huis,typeHuis){
  Huis <- tibble(adres = c(adresHuis))
  Huis$postcode4 <- c(as.character(postcode4Huis))
  Huis$postcode3 <- c(as.character(round(postcode4Huis,-1)))
  Huis$kamers <- c(kamersHuis)
  Huis$m2 <- c(m2Huis)
  Huis$m2xm2 <- c(m2Huis * m2Huis)
  Huis$type_simpel <- c(typeHuis) 
  
  Huis7_f <- fix7['(Intercept)'] + fix7['m2'] * Huis$m2 + fix7['kamers'] * Huis$kamers + fix7['m2:kamers'] * Huis$m2 * Huis$kamers + fix7['m2xm2'] * Huis$m2xm2
  Huis7_pc <- Beta7_pc[Huis$postcode4,]$'(Intercept)' + (Huis$type_simpel == "Huis") * Beta7_pc[Huis$postcode4,]$'type_simpelHuis' + (Huis$type_simpel == "Penthouse") * Beta7_pc[Huis$postcode4,]$'type_simpelPenthouse' + (Huis$type_simpel == "Woonboot") * Beta7_pc[Huis$postcode4,]$'type_simpelWoonboot'  

  huizen_data %>%
    filter(postcode4==postcode4Huis) %>%
    print()
  
  Huis7 <- exp(Huis7_f + ifelse(!is.na(Huis7_pc),Huis7_pc,0))
  print(paste0(Huis$adres[1],". Estimate 1 (fit7): ",round(Huis7,-3)))
  
  Huis2 <- exp(predict(fit2,Huis))
  print(paste0(Huis$adres[1],". Estimate 2 (fit2): ",round(Huis2,-3)))
}
  
HuisCheck("Hooidrift 14",3023,11,300,"Huis")
HuisCheck("Klaverstraat 49",3083,7,410,"Huis")

