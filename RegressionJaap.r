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

huizen_train <- huizen_data[sample_id=="train",]
huizen_valid <- huizen_data[sample_id=="validate",]
huizen_test <- huizen_data[sample_id=="test",]

# fit basic linear models

fit1 <- glm(logprijs ~ m2 + type_simpel + kamers + factor(postcode4) + kamers*m2 + m2xm2, data = huizen_train)       
summary(fit1)

fit2 <- glm(logprijs ~ m2 + type_complex + kamers + factor(postcode4) + kamers*m2 + m2xm2, data = huizen_train)       
summary(fit2)

fit3 <- glm(prijs ~ m2 + type_complex + kamers + factor(postcode3) + kamers*m2 + m2xm2, data = huizen_train)
summary(fit3)

regression_results <- tibble( pred1 = exp(predict(fit1,newdata=huizen_valid)),
                              pred2 = exp(predict(fit2,newdata=huizen_valid)),
                              pred3 = predict(fit3,newdata=huizen_valid),
                              prijs = huizen_valid$prijs,
                              pe1 = pred1/prijs-1,
                              pe2 = pred2/prijs-1,
                              pe3 = pred3/prijs-1)

# The multiplicative model clearly out-performs. Especially with complex type

regression_results %>%
  summarize(mape1 = mean(abs(pe1)),
            mape2 = mean(abs(pe2)),
            mape3 = mean(abs(pe3)),
            rmse1 = sqrt(mean(pe1^2)),
            rmse2 = sqrt(mean(pe2^2)),
            rmse3 = sqrt(mean(pe3^2))) %>%
  print()

