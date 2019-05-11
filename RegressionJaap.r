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

# creat train, test and validation sets

sample_id <- sample(1:3, size=nrow(huizen_clean), prob=c(0.7,0.15,0.15), replace = TRUE)

huizen_train <- huizen_clean[sample_id==1,]
huizen_valid <- huizen_clean[sample_id==2,]
huizen_test <- huizen_clean[sample_id==3,]

# fit basic linear models

fit1 <- glm(logprijs ~ m2 + type_simpel + kamers + factor(postcode4) + kamers*m2 + m2xm2, data = huizen_train)       
summary(fit1)

fit2 <- glm(logprijs ~ m2 + type_simpel + kamers + factor(postcode3) + kamers*m2 + m2xm2, data = huizen_train)
summary(fit2)

fit3 <- glm(logprijs ~ m2 + type_simpel + kamers + factor(postcode3), data = huizen_train)       
summary(fit3)

fit4 <- glm(logprijs ~ m2 + type_simpel + factor(postcode4) + m2xm2, data = huizen_train)   
summary(fit4)

fit5 <- glm(prijs ~ m2 + type_simpel + kamers + factor(postcode4) + kamers*m2 + m2xm2, data = huizen_train)       
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
predict5 <- predict(fit5,newdata=huizen_valid)

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

plotdata %>%
  filter(!is.na(predict5)) %>%
  summarise(me1 = mean(abs(delta1)),
            me2 = mean(abs(delta2)),
            me3 = mean(abs(delta3)),
            me4 = mean(abs(delta4)),
            me5 = mean(abs(delta5)))

plotdata %>%
  filter(!is.na(predict5)) %>%
  summarise(sme1 = sqrt(mean(delta1^2)),
            sme2 = sqrt(mean(delta2^2)),
            sme3 = sqrt(mean(delta3^2)),
            sme4 = sqrt(mean(delta4^2)),
            sme5 = sqrt(mean(delta5^2)))

plotdata %>%
  filter(!is.na(predict5)) %>%
  summarise(pe1 = mean(abs(pct1)),
            pe2 = mean(abs(pct2)),
            pe3 = mean(abs(pct3)),
            pe4 = mean(abs(pct4)),
            pe5 = mean(abs(pct5)))

# The winner is... fit1

plotdata<- plotdata %>%
  select (prijs,pred1,delta1,pct1)
