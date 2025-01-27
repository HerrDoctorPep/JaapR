# 
# GLM
# Leverage the Caret package 
# Based on https://www.machinelearningplus.com/machine-learning/caret-package/
#

# First we needtoprepare thedata set,combining the high-level and the detailed scraping

print("Linear model on Log(prijs)...")

X_train <- model_data_train %>%
  select(-c("prijs","logprijs","Oorspronkelijkevraagprijs", "Huidigevraagprijs","prijspm2","Aantalkeergetoond", "Aantalkeergetoondgisteren","Geplaatstop","detail_adres"))
Y_train <- model_data_train %>%
  select(c("prijs","logprijs", "prijspm2","Woonoppervlakte","Oorspronkelijkevraagprijs", "Huidigevraagprijs"))

predictors_GLM <- colnames(X_train)

normalize_model <- preProcess(X_train, method = c("center","scale"))

X_train <- predict(normalize_model,newdata = X_train)

model_logGLM <- train(logprijs ~ Woonoppervlakte + factor(postcode4) + Type + Kamers + m2xm2 + Plafondhoogte + Perceeloppervlakte + Kamers*Woonoppervlakte + Garage + Period + Bijzonderheden, data = bind_cols(X_train,Y_train), method='glm')
print(model_logGLM)

print("Simple multi-level model on Log(prijs)...")

model_MLM <- lmer(logprijs ~ (1|postcode4) + (1| Period) + (1|Type) + (1|Garage) + Woonoppervlakte + Kamers + m2xm2 + Plafondhoogte + Perceeloppervlakte + Kamers*Woonoppervlakte + + Bijzonderheden, data = bind_cols(X_train,Y_train))
print(model_MLM)

# We can only predict existinglevels of factor predictors if we do not use the multi-level version

print(paste0("There are ",nrow(model_data_test) - length(Admissible_X)," training cases that can not be predicted due to new levels in factors."))

X_test <- model_data_test[Admissible_X,] %>%
  select(-c("prijs","logprijs","Oorspronkelijkevraagprijs", "Huidigevraagprijs","prijspm2","Aantalkeergetoond", "Aantalkeergetoondgisteren","Geplaatstop","detail_adres"))
Y_test <- model_data_test[Admissible_X,] %>%
  select(c("prijs","logprijs", "prijspm2","Woonoppervlakte","Oorspronkelijkevraagprijs", "Huidigevraagprijs"))

X_test <- predict(normalize_model,newdata = X_test)

P_test <- Y_test %>%
  select(prijs,Woonoppervlakte)
  
P_test <- P_test %>%
  mutate(pred_logGLM = exp(predict(model_logGLM, newdata = X_test)),
         PE_logGLM = pred_logGLM/prijs-1)

P_test <- P_test %>%
  mutate(pred_MLM = exp(predict(model_MLM, newdata = X_test, allow.new.levels = TRUE)),
         PE_MLM = pred_MLM/prijs-1)

P_test %>%
  summarise(mape_logGLM = mean(abs(PE_logGLM)),
            smpe_logGLM = sqrt(mean(PE_logGLM^2)),
            mape_MLM = mean(abs(PE_MLM)),
            smpe_MLM = sqrt(mean(PE_MLM^2))) %>%
  print(n=Inf)

