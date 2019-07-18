# 
# XGBoost algorithm
# Use the Caret package 
# Based on https://www.machinelearningplus.com/machine-learning/caret-package/
# For tuning using https://www.analyticsvidhya.com/blog/2016/03/complete-guide-parameter-tuning-xgboost-with-codes-python/

# Build on the work done in CaretGLMJaap.r

# No action for missing values needed

# OneHot encoding

X_train <- model_data_train %>%
  select(-c("prijs","logprijs","Oorspronkelijkevraagprijs", "Huidigevraagprijs","prijspm2","Aantalkeergetoond", "Aantalkeergetoondgisteren","Geplaatstop","detail_adres"))
Y_train <- model_data_train %>%
  select(c("prijs","logprijs", "prijspm2","Woonoppervlakte","Oorspronkelijkevraagprijs", "Huidigevraagprijs"))



normalize_model <- preProcess(X_train, method = c("center","scale"))

X_train <- predict(normalize_model,newdata = X_train)

X_train <- X_train %>%
  select(-postcode4)

dummies_model <- dummyVars( ~ Garage + Balkon + Tuin + Keuken + Verwarming + Bijzonderheden + Isolatie + Staatonderhoud + longitude + latitude + prijstype + Bouwjaar + Kamers + Woonoppervlakte + m2xm2 + Plafondhoogte, data=X_train)

X_train_mat <- as_tibble(predict(dummies_model, newdata = X_train))

featurePlot(x=X_train_mat[,c('Woonoppervlakte','Plafondhoogte')],
            y=Y_train$prijs,
            plot="scatter")

# Train the XGBoost model (Tree)

fitControl <- trainControl(
  method = 'cv',                   # k-fold cross validation
  number = 5,                      # number of folds
  savePredictions = 'final'        # saves predictions for optimal tuning parameter
)

# grid_xgbT <- expand.grid(nrounds = 100 * 1:9,
#                          max_depth = c(3,4),
#                          eta = c(0.1,0.2),
#                          gamma = 0.01,
#                          colsample_bytree = 0.7,
#                          min_child_weight = c(0.8),
#                          subsample = 1
# )

grid_xgbT <- expand.grid(nrounds = c(400,500,600),
                         max_depth = c(3),
                         eta = c(0.1),
                         gamma = 0.01,
                         colsample_bytree = 0.7,
                         min_child_weight = c(0.8),
                         subsample = 1
)

model_xgbT <- train(prijspm2 ~., 
                    data = bind_cols(X_train_mat,Y_train[,'prijspm2']), 
                    method='xgbTree',
                    tuneGrid = grid_xgbT,
                    trControl = fitControl
)

summary(model_xgbT)
print(model_xgbT)
plot(model_xgbT)

# Train the XGBoost model (Linear)

grid_xgbL <- expand.grid(nrounds = c(25,50),
                         lambda = c(0.2), # 0.3 much better
                         alpha = 0.1,
                         eta = c(0.2) # not much difference
)

model_xgbL <- train(prijspm2 ~., 
                    data = bind_cols(X_train_mat,Y_train[,'prijspm2']), 
                    method='xgbLinear',
                    tuneGrid = grid_xgbL,
                    trControl = fitControl
)

print(model_xgbL)
plot(model_xgbL)

# use trained model on the test data; same prep as for MLM

X_test <- model_data_test[Admissible_X,] %>%
  select(-c("prijs","logprijs","Oorspronkelijkevraagprijs", "Huidigevraagprijs","prijspm2","Aantalkeergetoond", "Aantalkeergetoondgisteren","Geplaatstop","detail_adres"))

Y_test <- model_data_test[Admissible_X,] %>%
  select(c("prijs","logprijs", "prijspm2","Woonoppervlakte","Oorspronkelijkevraagprijs", "Huidigevraagprijs"))

X_test <- predict(normalize_model,newdata = X_test)

# use trained model on the test data; additional prep for XGBoost

X_test <- X_test %>%
  select(-postcode4)

X_test_mat <- as_tibble(predict(dummies_model, newdata = X_test))

# Outcomes

P_test$pred_XGT <- predict(model_xgbT,X_test_mat) * P_test$Woonoppervlakte
P_test$PE_XGT <- P_test$pred_XGT / Y_test$prijs - 1

P_test$pred_XGL <- predict(model_xgbL,X_test_mat) * P_test$Woonoppervlakte
P_test$PE_XGL <- P_test$pred_XGL / Y_test$prijs - 1

P_test <- P_test %>%
  mutate(pred_Ens = (pred_MLM+pred_XGL)/2) %>%
  mutate(PE_Ens = pred_Ens / prijs -1)

P_test %>%
  summarise(mape_logGLM = mean(abs(PE_logGLM)),
            smpe_logGLM = sqrt(mean(PE_logGLM^2)),
            mape_MLM = mean(abs(PE_MLM)),
            smpe_MLM = sqrt(mean(PE_MLM^2)),
            mape_XGT = mean(abs(PE_XGT)),
            smpe_XGT = sqrt(mean(PE_XGT^2)),
            mape_XGL = mean(abs(PE_XGL)),
            smpe_XGL = sqrt(mean(PE_XGL^2)),
            mape_Ens = mean(abs(PE_Ens)),
            smpe_Ens = sqrt(mean(PE_Ens^2))) %>%
  print(n=Inf)

