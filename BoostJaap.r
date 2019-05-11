#
# High hopes for XG boost for this type of problems
#

library(xgboost)
library(caret)

huizen_train <- huizen_clean[sample_id==1,]
huizen_valid <- huizen_clean[sample_id==2,]
huizen_test <- huizen_clean[sample_id==3,]

huizen_boost <- huizen_train

features <-c("m2","m2xm2", "kamers", "postcode4","Latitude", "Longitude")


type_simp <- unique(huizen_train$type_simpel)
Dummy <- matrix(NA, nrow = nrow(huizen_train), ncol = length(type_simp))

for (i in 1:length(type_simp)){
  Dummy[,i]<- as.numeric(huizen_train$type_simpel == type_simp[i])
}
colnames(Dummy)<- type_simp

XG_train <- huizen_train[,features] %>%
  mutate(postcode4 = as.numeric(as.character(postcode4))) %>%
  bind_cols(as.tibble(Dummy))


Dummy2 <- matrix(NA, nrow = nrow(huizen_valid), ncol = length(type_simp))

for (i in 1:length(type_simp)){
  Dummy2[,i]<- as.numeric(huizen_valid$type_simpel == type_simp[i])
}
colnames(Dummy2)<- type_simp

XG_valid <- huizen_valid[,features] %>%
  mutate(postcode4 = as.numeric(as.character(postcode4))) %>%
  bind_cols(as.tibble(Dummy2))


dtrain<- xgb.DMatrix(data = as.matrix(XG_train),
            label = huizen_train$prijs)

XG_me <- rep(NA,30)
XG_sme <- rep(NA,30)
XG_pe <- rep(NA,30)

for(i in 1:30){
  bst <- xgboost(data=dtrain,
                 max.depth=3,
                 eta=0.2,
                 nthread=2, 
                 nrounds=i,
                 objective="reg:linear",
                 verbose=2)

  XG_pred <- predict(bst, as.matrix(XG_valid))  
  XG_prijs<- huizen_valid$prijs
  XG_me[i]<- mean(abs(XG_pred - XG_prijs))
  XG_sme[i]<- sqrt(mean((XG_pred - XG_prijs)^2))
  XG_pe[i]<- mean(abs((XG_pred / XG_prijs) - 1))
}

plot(XG_sme)

bst <- xgboost(data=dtrain,
               max.depth=9,
               eta=0.2,
               nthread=4, 
               nrounds=15,
               objective="reg:linear",
               verbose=0)
  
XG_pred <- predict(bst, as.matrix(XG_valid))  
XG_prijs<- huizen_valid$prijs
print(c(mean(abs(XG_pred - XG_prijs)),
        sqrt(mean((XG_pred - XG_prijs)^2)),
        mean(abs((XG_pred / XG_prijs) - 1))))


