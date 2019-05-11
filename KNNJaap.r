#
# Rather than regression we can also do KNN approach
#


library('caret')

scale_m2 <- 1000

KNN_data <- huizen_clean %>%
  left_join(postcode_data,by=c("postcode6"="PostCode")) %>%
  mutate(prijspm2 = prijs / m2,
         hm2 = m2 / scale_m2) %>%
  select(prijs,prijspm2,Longitude,Latitude,hm2) %>%
  filter(!is.na(Longitude))

sample_id <- sample(1:3, size=nrow(KNN_data), prob=c(0.7,0.15,0.15), replace = TRUE)

KNN_train <- KNN_data[sample_id==1,]
KNN_valid <- KNN_data[sample_id==2,]
KNN_test <- KNN_data[sample_id==3,]

# KNN on prijs/m²

x <- KNN_train %>% 
  select(-prijspm2,-prijs)
y<- KNN_train$prijspm2
z <- KNN_valid %>%
  select(-prijspm2,-prijs)
k_max <- 50


KNN_result <- matrix(NA, nrow = nrow(z), ncol = k_max) 
KNN_me <- rep(NA,k_max)
KNN_sme <- rep(NA,k_max)
KNN_pe <- rep(NA,k_max)
KNN_AIC <- rep(NA,k_max)


for(i in 1:k_max){
  KNN_result[,i] <- knnregTrain(x, z, y, k = i)
  KNN_me[i]<- mean(abs(KNN_result[,i] * KNN_valid$hm2*scale_m2 - KNN_valid$prijspm2 * KNN_valid$hm2*scale_m2))
  KNN_sme[i]<- sqrt(mean((KNN_result[,i] * KNN_valid$hm2*scale_m2 - KNN_valid$prijspm2 * KNN_valid$hm2*scale_m2)^2))
  KNN_pe[i]<- mean(abs(KNN_result[,i]/KNN_valid$prijspm2 - 1))
}

plot(KNN_me)
plot(KNN_sme)
plot(KNN_pe)
plot(KNN_AIC)

k_selected <- 9

print(c(KNN_me[k_selected],KNN_sme[k_selected],KNN_pe[k_selected]))

plot_KNN <- as_tibble( list(pred = KNN_result[,k_selected] * KNN_valid$hm2*scale_m2,
                       me = abs(KNN_result[,k_selected] * KNN_valid$hm2*scale_m2 - KNN_valid$prijspm2 * KNN_valid$hm2*scale_m2),
                       sme = (KNN_result[,k_selected] * KNN_valid$hm2*scale_m2 - KNN_valid$prijspm2 * KNN_valid$hm2*scale_m2)^2,
                       pe = abs(KNN_result[,k_selected]/KNN_valid$prijspm2 - 1),
                       prijs = KNN_valid$prijspm2 * KNN_valid$hm2*scale_m2,
                       m2 = KNN_valid$hm2*scale_m2))

sqrt(mean(plot_KNN$sme))
mean(plot_KNN$me)
mean(plot_KNN$pe)

# KNN directly on price

x_2 <- KNN_train %>% 
  select(-prijspm2,-prijs)
y_2 <- KNN_train$prijs
z_2  <- KNN_valid %>%
  select(-prijspm2,-prijs)
k_max <- 50

KNN_result2 <- matrix(NA, nrow = nrow(z), ncol = k_max) 
KNN_me2 <- rep(NA,k_max)
KNN_sme2 <- rep(NA,k_max)
KNN_pe2 <- rep(NA,k_max)

for(i in 1:k_max){
  KNN_result2[,i] <- knnregTrain(x_2, z_2, y_2, k = i)
  KNN_me2[i]<- mean(abs(KNN_result[,i] * KNN_valid$hm2*scale_m2 - KNN_valid$prijspm2 * KNN_valid$hm2*scale_m2))
  KNN_sme2[i]<- sqrt(mean((KNN_result[,i] * KNN_valid$hm2*scale_m2 - KNN_valid$prijspm2 * KNN_valid$hm2*scale_m2)^2))
  KNN_pe2[i]<- mean(abs(KNN_result[,i]/KNN_valid$prijspm2 - 1))
}

plot(KNN_me2)
plot(KNN_sme2)
plot(KNN_pe2)

k_selected <- 10

print(c(KNN_me[k_selected],KNN_sme[k_selected],KNN_pe[k_selected]))



# Klaverstraat

LatLon <- postcode_data %>%
  filter(str_detect(PostCode,"3083VB"))

knnregTrain(x_2, c(LatLon$Longitude[1],LatLon$Latitude[1],410/scale_m2), y_2, k = k_selected)

# Hooidrift

LatLon <- postcode_data %>%
  filter(str_detect(PostCode,"3023KP"))

knnregTrain(x, c(LatLon$Longitude[1],LatLon$Latitude[1],300/scale_m2), y, k = k_selected) *300



