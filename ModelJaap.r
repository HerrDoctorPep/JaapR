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

# fit2 <- glm(logprijs ~ m2 + type_simpel + kamers + factor(postcode4), data = huizen_train)       
# summary(fit2)

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
# predict2 <- exp(predict(fit2,newdata=huizen_valid))
predict3 <- exp(predict(fit3,newdata=huizen_valid))
predict4 <- exp(predict(fit4,newdata=huizen_valid))
predict5 <- exp(predict(fit5,newdata=huizen_valid))

plotdata <- tibble(prijs = huizen_valid$prijs,
                   pred1 = predict1,
                   # pred2 = predict2,
                   pred3 = predict3,
                   pred4 = predict4,
                   pred5 = predict5) %>%
  mutate(delta1 = pred1 - prijs,
         # delta2 = pred2 - prijs,
         delta3 = pred3 - prijs,
         delta4 = pred4 - prijs,
         delta5 = pred5 - prijs,
         pct1 = pred1/prijs-1,
         pct3 = pred3/prijs-1,
         pct4 = pred4/prijs-1,
         pct5 = pred5/prijs-1)

# Postal code is important - even defies over-fitting; cross between rooms and m² is a good addition; cross between pc and type doe s not add

c(mean(plotdata$delta1), mean(plotdata$delta3),mean(plotdata$delta4),mean(plotdata$delta5))
c(sqrt(mean(plotdata$delta1^2)), sqrt(mean(plotdata$delta3^2)),sqrt(mean(plotdata$delta4^2)),sqrt(mean(plotdata$delta5^2)))
c(mean(plotdata$pct1), mean(plotdata$pct3),mean(plotdata$pct4),mean(plotdata$pct5))


ggplot() +
  geom_point(data=plotdata,aes(x=pred4,y=prijs)) +
  labs(title = "Prediction v. Prijs", x="Prediction", y="Prijs")

ggplot() +
  geom_point(data=plotdata,aes(x=prijs,y=delta4)) +
  labs(title = "Prijs v. Residual", x="Prijs", y="Residu")

ggplot() +
  geom_point(aes(x=huizen_valid$m2,y=(predict4 - huizen_valid$prijs))) +
  labs(title = "Oppervlak v. Residual", x="m2", y="Residu")

# use as inspiration for multi-level model

library (lme4) # package for linear mixed effects model
library (broom) # tidy output of std R functions - especially useful to access fitted models
library (influence.ME) # contains se.fixef function

fit6 <- lmer(logprijs ~ m2 + kamers + kamers*m2 + (type_simpel|postcode4), data = huizen_train)
summary(fit6)

# adding postcode 3 does not really add something here - naturally
fit7 <- lmer(logprijs ~  postcode3 + m2 + kamers + kamers*m2 + (type_simpel|postcode4), data = huizen_train)
summary(fit7)

# Create prediction for fit6

Beta_pc <- ran6$postcode4[as.character(huizen_valid$postcode4),]

predict6_f <- fix6['(Intercept)'] + fix6['m2'] * huizen_valid$m2 + fix6['kamers'] * huizen_valid$kamers + fix6['m2:kamers'] * huizen_valid$m2 * huizen_valid$kamers
predict6_pc <- Beta_pc$'(Intercept)' + (huizen_valid$type_simpel == "Huis") * Beta_pc$'type_simpelHuis' + (huizen_valid$type_simpel == "Penthouse") * Beta_pc$'type_simpelPenthouse' + (huizen_valid$type_simpel == "Woonboot") * Beta_pc$'type_simpelWoonboot'

predict6 <- exp(predict6_f + ifelse(!is.na(predict6_pc),predict6_pc,0))

plotdata$pred6 <- predict6
plotdata$delta6 <- predict6 - plotdata$prijs
plotdata$pct6 <- predict6 / plotdata$prijs -1


plotdata %>%
  filter(!is.na(predict5)) %>%
  summarise(me1 = mean(delta1),
            me3 = mean(delta3),
            me4 = mean(delta4),
            me5 = mean(delta5),
            me6 = mean(delta6))

plotdata %>%
  filter(!is.na(predict5)) %>%
  summarise(sme1 = sqrt(mean(delta1^2)),
            sme3 = sqrt(mean(delta3^2)),
            sme4 = sqrt(mean(delta4^2)),
            sme5 = sqrt(mean(delta5^2)),
            sme6 = sqrt(mean(delta6^2)))

plotdata %>%
  filter(!is.na(predict5)) %>%
  summarise(pe1 = mean(pct1),
            pe3 = mean(pct3),
            pe4 = mean(pct4),
            pe5 = mean(pct5),
            pe6 = mean(pct6))

ggplot() +
  geom_point(data=plotdata,aes(x=pred6,y=prijs,col="red")) +
  geom_point(data=plotdata,aes(x=pred4,y=prijs,col="blue")) +
  labs(title = "Prediction v. Prijs", x="Prijs", y="Prediction") +
  coord_cartesian(xlim = c(0, 800000)) 

ggplot() +
  geom_point(data=plotdata,aes(x=prijs,y=delta6,col="red")) +
  geom_point(data=plotdata,aes(x=prijs,y=delta4,col="blue")) + 
  labs(title = "Prijs v. Residual", x="Prijs", y="Residu")

ggplot() +
  geom_point(aes(x=huizen_valid$m2,y=(predict6 - huizen_valid$prijs),col="red")) +
  geom_point(aes(x=huizen_valid$m2,y=(predict5 - huizen_valid$prijs),col="blue"))
  labs(title = "Oppervlak v. Residual", x="m2", y="Residu")
