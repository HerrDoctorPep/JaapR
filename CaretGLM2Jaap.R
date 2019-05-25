# 
# GLM
# Leverage the Caret package 
# Based on https://www.machinelearningplus.com/machine-learning/caret-package/
#

# First we needtoprepare thedata set,combining the high-level and the detailed scraping

print("Linear model on prijs/m²...")

X_train <- model_data_train %>%
  select(-c("prijs","logprijs","Oorspronkelijkevraagprijs", "Huidigevraagprijs","prijspm2"))
Y_train <- model_data_train %>%
  select(c("prijs","logprijs", "prijspm2","Oorspronkelijkevraagprijs", "Huidigevraagprijs","Woonoppervlakte"))

normalize_model <- preProcess(X_train, method = c("center","scale"))

X_train <- predict(normalize_model,newdata = X_train)

modelLookup('glm')

model_GLMpm2 <- train(prijspm2 ~ Woonoppervlakte + factor(postcode4) + Type + Kamers + Kamers*Woonoppervlakte + Garage + Period, data = bind_cols(X_train,Y_train), method='glm')
print(model_GLMpm2)

print("Simple multi-level model on Log(prijs)...")

model_MLMpm2 <- lmer(prijspm2 ~ (1|postcode4) + (1| Period) + (1|Type) + (1|Garage) + Woonoppervlakte + Kamers + Kamers*Woonoppervlakte, data = bind_cols(X_train,Y_train))
print(model_MLMpm2)

# We can only predict existinglevels of factor predictors if we do not use the multi-level version

Admissible_X <- (model_data_test$Type %in% X_train$Type) & (model_data_test$postcode4 %in% X_train$postcode4)

print(paste0("There are ",nrow(model_data_test) - length(Admissible_X)," training cases that can not be predicted due to new levels in factors."))

X_test <- model_data_test[Admissible_X,] %>%
  select(-c("prijs","logprijs","Oorspronkelijkevraagprijs", "Huidigevraagprijs","prijspm2"))
Y_test <- model_data_test[Admissible_X,] %>%
  select(c("prijs","logprijs", "prijspm2","Oorspronkelijkevraagprijs", "Huidigevraagprijs", "Woonoppervlakte"))

# Normalized test set and use model to predict

X_test <- predict(normalize_model,newdata = X_test)

P_test <- tibble(Prijs = Y_test$prijs, Woonoppervlakte = Y_test$Woonoppervlakte)
  
P_test <- P_test %>%
  mutate(pred_GLMpm2 = predict(model_GLMpm2, newdata = X_test) * Woonoppervlakte,
         PE_GLMpm2 = pred_GLMpm2/Prijs-1)

P_test <- P_test %>%
  mutate(pred_MLMpm2 = predict(model_MLMpm2, newdata = X_test) * Woonoppervlakte,
         PE_MLMpm2 = pred_MLMpm2/Prijs-1)

P_test %>%
  summarise(mape_GLMpm2 = mean(abs(PE_GLMpm2)),
            smpe_GLMpm2 = sqrt(mean(PE_GLMpm2^2)),
            mape_MLMpm2 = mean(abs(PE_MLMpm2)),
            smpe_MLMpm2 = sqrt(mean(PE_MLMpm2^2))) %>%
  print(n=Inf)

# ggplot() +
#   geom_point(data=P_test,aes(x=pred_logGLM,y=pred_logGLM-Price,col="GLM")) +
#   geom_point(data=P_test,aes(x=pred_MLM,y=pred_MLM-Price,col="MLM")) +
#   labs(title = "Prijs v. Residual", x="Predicted price", y="Residue")  +
#   coord_cartesian(xlim = c(0, 900000))
  
rm(X_train,
   Y_train,
   X_test,
   Y_test,
   Admissible_X,
   P_test,
   skimmed)
