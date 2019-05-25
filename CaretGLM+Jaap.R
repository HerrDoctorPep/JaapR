# 
# GLM
# Leverage the Caret package 
# Based on https://www.machinelearningplus.com/machine-learning/caret-package/
#

# First we needtoprepare thedata set,combining the high-level and the detailed scraping

print("Linear model on Log(prijs)...")

X_train <- model_data_train %>%
  select(-c("prijs","logprijs","Oorspronkelijkevraagprijs", "Huidigevraagprijs","prijspm2"))
Y_train <- model_data_train %>%
  select(c("prijs","logprijs", "prijspm2","Oorspronkelijkevraagprijs", "Huidigevraagprijs"))

predictors_GLM <- colnames(X_train)

normalize_model <- preProcess(X_train, method = c("center","scale"))

X_train <- predict(normalize_model,newdata = X_train)

model_logGLM <- train(logprijs ~ Woonoppervlakte + factor(postcode4) + Type + Kamers + m2xm2 + Kamers*Woonoppervlakte + Period, 
                      data = bind_cols(X_train,Y_train), 
                      method='glm')
print(model_logGLM)

print("Simple multi-level model on Log(prijs)...")

model_MLM <- lmer(logprijs ~ (1|postcode4) + (1| Period) + (1|Type) + Woonoppervlakte + Kamers + m2xm2 + Kamers*Woonoppervlakte, data = bind_cols(X_train,Y_train))
print(model_MLM)

# We can only predict existinglevels of factor predictors if we do not use the multi-level version

Admissible_X <- (model_data_test$Type %in% X_train$Type) & (model_data_test$postcode4 %in% X_train$postcode4)

print(paste0("There are ",nrow(model_data_test) - length(Admissible_X)," training cases that can not be predicted due to new levels in factors."))

X_test <- model_data_test[Admissible_X,] %>%
  select(-c("prijs","logprijs","Oorspronkelijkevraagprijs", "Huidigevraagprijs","prijspm2"))
Y_test <- model_data_test[Admissible_X,] %>%
  select(c("prijs","logprijs", "prijspm2","Oorspronkelijkevraagprijs", "Huidigevraagprijs"))

X_test <- predict(normalize_model,newdata = X_test)

P_test <- tibble(Price = Y_test$prijs)
  
P_test <- P_test %>%
  mutate(pred_logGLM = exp(predict(model_logGLM, newdata = X_test)),
         PE_logGLM = pred_logGLM/Price-1)

P_test <- P_test %>%
  mutate(pred_MLM = exp(predict(model_MLM, newdata = X_test)),
         PE_MLM = pred_MLM/Price-1)

P_test %>%
  summarise(mape_logGLM = mean(abs(PE_logGLM)),
            smpe_logGLM = sqrt(mean(PE_logGLM^2)),
            mape_MLM = mean(abs(PE_MLM)),
            smpe_MLM = sqrt(mean(PE_MLM^2))) %>%
  print(n=Inf)


Y_train$pred_logGLM<- exp(predict(model_logGLM,newdata=X_train))
Y_train$pred_MLM<- exp(predict(model_MLM,newdata=X_train))
Y_train <- Y_train %>%
  mutate(res_logGLM = pred_logGLM - prijs,
         res_MLM = pred_MLM - prijs)

model_GLMontop <- train ( res_logGLM ~ Garage + Tuin + Bijzonderheden, 
                          data = bind_cols(X_train,Y_train), 
                          method='glm')
model_GLMontop

model_MLMontop <- train ( res_MLM ~ Garage + Tuin + Bijzonderheden, 
                          data = bind_cols(X_train,Y_train), 
                          method='glm')
model_MLMontop

P_test <- P_test %>%
  mutate(pred_MLMontop = pred_MLM+ predict(model_MLMontop, newdata = X_test),
         PE_MLMontop = pred_MLMontop/Price-1) %>%
  mutate(pred_logGLMontop = pred_MLM+ predict(model_logGLMontop, newdata = X_test),
         PE_logGLMontop = pred_logGLMontop/Price-1)

P_test %>%
  summarise(mape_logGLM = mean(abs(PE_logGLM)),
            smpe_logGLM = sqrt(mean(PE_logGLM^2)),
            mape_MLM = mean(abs(PE_MLM)),
            smpe_MLM = sqrt(mean(PE_MLM^2)),
            mape_MLMontop = mean(abs(PE_MLMontop)),
            smpe_MLMontop = sqrt(mean(PE_MLMontop^2)),
            mape_logGLMontop = mean(abs(PE_logGLMontop)),
            smpe_logGLMontop = sqrt(mean(PE_logGLMontop^2))) %>%
  print(n=Inf)

# Conclusion: does not help ontest set (not a surprise - considering the low R²)

rm(X_train,
   Y_train,
   X_test,
   Y_test,
   Admissible_X,
   P_test,
   skimmed)

