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

# Now for simple multi-level models

library (lme4) # package for linear mixed effects model
library (broom) # tidy output of std R functions - especially useful to access fitted models
library (influence.ME) # contains se.fixef function

fit6 <- lmer(logprijs ~ m2 + kamers + kamers*m2 + (type_simpel|postcode4), data = huizen_train)
summary(fit6)

fix6 <- fixef(fit6)
ran6 <- ranef(fit6)

# Create prediction for fit6

Beta6_pc <- ran6$postcode4[as.character(huizen_valid$postcode4),]

predict6_f <- fix6['(Intercept)'] + fix6['m2'] * huizen_valid$m2 + fix6['kamers'] * huizen_valid$kamers + fix6['m2:kamers'] * huizen_valid$m2 * huizen_valid$kamers
predict6_pc <- Beta6_pc$'(Intercept)' + (huizen_valid$type_simpel == "Huis") * Beta6_pc$'type_simpelHuis' + (huizen_valid$type_simpel == "Penthouse") * Beta6_pc$'type_simpelPenthouse' + (huizen_valid$type_simpel == "Woonboot") * Beta6_pc$'type_simpelWoonboot'

predict6 <- exp(predict6_f + ifelse(!is.na(predict6_pc),predict6_pc,0))

plotdata$pred6 <- predict6
plotdata$delta6 <- predict6 - plotdata$prijs
plotdata$pct6 <- predict6 / plotdata$prijs -1

# drop the log

fit8 <- lmer(prijs ~ m2 + kamers + kamers*m2 + (type_simpel|postcode4), data = huizen_train)
summary(fit8)

fix8 <- fixef(fit8)
ran8 <- ranef(fit8)

# Create prediction for fit6

Beta8_pc <- ran8$postcode4[as.character(huizen_valid$postcode4),]

predict8_f <- fix8['(Intercept)'] + fix8['m2'] * huizen_valid$m2 + fix8['kamers'] * huizen_valid$kamers + fix8['m2:kamers'] * huizen_valid$m2 * huizen_valid$kamers
predict8_pc <- Beta8_pc$'(Intercept)' + (huizen_valid$type_simpel == "Huis") * Beta8_pc$'type_simpelHuis' + (huizen_valid$type_simpel == "Penthouse") * Beta8_pc$'type_simpelPenthouse' + (huizen_valid$type_simpel == "Woonboot") * Beta8_pc$'type_simpelWoonboot'

predict8 <- predict8_f + ifelse(!is.na(predict8_pc),predict8_pc,0)

plotdata$pred8 <- predict8
plotdata$delta8 <- predict8 - plotdata$prijs
plotdata$pct8 <- predict8 / plotdata$prijs -1


# dropping the log andadding m2 x m2 as predictor
fit7 <- lmer(logprijs ~ m2 + kamers + kamers*m2 + m2xm2 + (type_simpel|postcode4), data = huizen_train)
summary(fit7)

fix7 <- fixef(fit7)
ran7 <- ranef(fit7)

# Create prediction for fit7

Beta7_pc <- ran7$postcode4[as.character(huizen_valid$postcode4),]

predict7_f <- fix7['(Intercept)'] + fix7['m2'] * huizen_valid$m2 + fix7['kamers'] * huizen_valid$kamers + fix7['m2:kamers'] * huizen_valid$m2 * huizen_valid$kamers + fix7['m2xm2'] * huizen_valid$m2xm2
predict7_pc <- Beta7_pc$'(Intercept)' + (huizen_valid$type_simpel == "Huis") * Beta7_pc$'type_simpelHuis' + (huizen_valid$type_simpel == "Penthouse") * Beta7_pc$'type_simpelPenthouse' + (huizen_valid$type_simpel == "Woonboot") * Beta7_pc$'type_simpelWoonboot'

predict7 <- exp(predict7_f + ifelse(!is.na(predict7_pc),predict7_pc,0))

plotdata$pred7 <- predict7
plotdata$delta7 <- predict7 - plotdata$prijs
plotdata$pct7 <- predict7 / plotdata$prijs -1

# Placeholder

plotdata$pred9 <- NA
plotdata$delta9 <- NA
plotdata$pct9 <- NA


plotdata %>%
  filter(!is.na(pred1)) %>%
  summarise(me1 = mean(abs(delta1)),
            me6 = mean(abs(delta6)),
            me7 = mean(abs(delta7)),
            me8 = mean(abs(delta8)),
            me9 = mean(abs(delta9)))

plotdata %>%
  filter(!is.na(pred1)) %>%
  summarise(sme1 = sqrt(mean(delta1^2)),
            sme6 = sqrt(mean(delta6^2)),
            sme7 = sqrt(mean(delta7^2)),
            sme8 = sqrt(mean(delta8^2)),
            sme9 = sqrt(mean(delta9^2)))

plotdata %>%
  filter(!is.na(pred1)) %>%
  summarise(pe1 = mean(abs(pct1)),
            pe6 = mean(abs(pct6)),
            pe7 = mean(abs(pct7)),
            pe8 = mean(abs(pct8)),
            pe9 = mean(abs(pct9)))



