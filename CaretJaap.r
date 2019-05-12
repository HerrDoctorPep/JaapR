# 
# Leverage the Caret package 
# Based on https://www.machinelearningplus.com/machine-learning/caret-package/
#

# install.packages(c('caret', 'skimr', 'RANN', 'randomForest', 'fastAdaboost', 'gbm', 'xgboost', 'caretEnsemble', 'C50', 'earth'))

library(caret)
id_train <- createDataPartition(huizen_data$prijs, p=0.7, list=FALSE)

caret_train <- huizen_data[id_train,]
caret_test <- huizen_data[-id_train,]

x <- caret_train %>% 
  select(-c('adres','postcode','prijs','logprijs','prijspm2')) %>%
  mutate(postcode6 = as.factor(postcode6),
         postcode4 = as.factor(postcode4),
         postcode3 = as.factor(postcode3),
         type_complex = as.factor(type_complex),
         type_simpel = as.factor(type_simpel),
         prijstype = as.factor(prijstype))

y <- caret_train %>%
  select(prijs)

library('skimr')

skim_to_wide(caret_train)[, c(1:5, 9:11, 13, 15:16)]
