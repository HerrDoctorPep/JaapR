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

# install.packages(c('lme4', 'broom', 'influence.ME'))

library (lme4) # package for linear mixed effects model
library (broom) # tidy output of std R functions - especially useful to access fitted models
library (influence.ME) # contains se.fixef function

fit4 <- lmer(logprijs ~ (1|postcode4) + m2 + type_complex + kamers + kamers*m2 + m2xm2 , data = huizen_train)
summary(fit4)

fix4 <- fixef(fit4)
ran4 <- ranef(fit4)

Beta4_pc <- ran4$postcode4[as.character(huizen_valid$postcode4),]

# Manual construction of the matrix for multiplication

pos_type_complex <- which(str_detect(names(fix4),"type_complex"))

X4 <- matrix(NA, ncol = length(fix4), nrow = nrow(huizen_valid))
X4[,1] <- 1 
X4[,2] <- huizen_valid$m2
for (i in 1:nrow(huizen_valid)){
  for(j in pos_type_complex) {
    X4[i,j] <- paste0("type_complex",huizen_valid$type_complex[i]) == names(fix4)[j]
  }
}

X4[,length(pos_type_complex)+3] <- huizen_valid$kamers
X4[,length(pos_type_complex)+4] <- huizen_valid$m2xm2
X4[,length(pos_type_complex)+5] <- huizen_valid$m2 * huizen_valid$kamers

colnames(X4) <- names(fix4)
pred4_f <- as.vector(X4 %*% fix4)

pred4_pc <- Beta4_pc

# # fit5 does not even converge!!
# 
# fit5 <- lmer(logprijs ~  (m2|postcode4) + type_complex + kamers + kamers*m2 + m2xm2 , data = huizen_train)
# summary(fit5)
# 
# fix5 <-fixef(fit5)
# ran5 <-ranef(fit5)
# 
# Beta5_pc <- ran5$postcode4[as.character(huizen_valid$postcode4),]
# 
# X5 <- matrix(NA, ncol = length(fix4), nrow = nrow(huizen_valid))
# X5[,1] <- 1
# X5[,2] <- huizen_valid$m2
# for (i in 1:nrow(huizen_valid)){
#   for(j in 3:22) {
#     X5[i,j] <- paste0("type_complex",huizen_valid$type_complex[i]) == names(fix4)[j]
#   }
# }
# 
# X5[,23] <- huizen_valid$kamers
# X5[,24] <- huizen_valid$m2xm2
# X5[,25] <- huizen_valid$m2 * huizen_valid$kamers
# 
# pred5_f <- as.vector(X5 %*% fix5)
# pred5_pc <- Beta5_pc$`(Intercept)` + Beta5_pc$m2 * huizen_valid$m2

ML_results <- tibble (pred4 = exp(pred4_f + pred4_pc),
                      prijs = huizen_valid$prijs,
                      pe4 = pred4/prijs-rep(1,nrow(huizen_valid)))

ML_results %>%
  summarise(mape4 = mean(abs(pe4)),
            rmse4 = sqrt(mean(pe4^2)))


