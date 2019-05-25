# 
# XGBoost algorithm
# Use the Caret package 
# Based on https://www.machinelearningplus.com/machine-learning/caret-package/
#

# Build on the work done in CaretGLMJaap.r

# No action for missing values needed

# OneHot encoding

X_train <- model_data_train %>%
  select(-c("prijs","logprijs","Oorspronkelijkevraagprijs", "Huidigevraagprijs","prijspm2"))
y_train <- model_data_train %>%
  select(c("prijs","logprijs", "prijspm2","Oorspronkelijkevraagprijs", "Huidigevraagprijs"))

normalize_model <- preProcess(X_train, method = c("center","scale"))

X_train <- predict(normalize_model,newdata = X_train)

dummies_model <- dummyVars( ~ Garage + Balkon + Tuin + postcode4 + Keuken + Verwarming + Bijzonderheden + Isolatie + Staatonderhoud + longitude + latitude + prijstype + Bouwjaar + Kamers + Woonoppervlakte + m2xm2, data=X_train)
dummies_model

X_train_mat <- as_tibble(predict(dummies_model, newdata = X_train))

featurePlot(x=X_train_mat[,'Woonoppervlakte'],
            y=y_train$prijs,
            plot="scatter")

# Train the XGBoost model (Tree)

modelLookup("xgbLinear")

model_xgbT <- train(prijspm2 ~., 
                    data = bind_cols(X_train_mat,y_train[,'prijspm2']), method='xgbTree')

model_xgbT
plot(model_xgbT)

# Train the XGBoost model (Linear)

model_xgbL <- train(prijspm2 ~., 
                    data = bind_cols(X_train_mat,y_train[,'prijspm2']), method='xgbLinear')

model_xgbL
plot(model_xgbL)

# use trained model on the test data

X_test_mat <- as_tibble(predict(dummies_model, newdata = X_test)) %>%
  mutate(BijzonderhedenGemeubileerd = 0,
         BijzonderhedenLiftZwembad = 0)
glimpse(X_test_mat)

P_test$pred_XGT <- predict(model_xgbT,X_test_mat)
P_test$PE_XGT <- P_test$pred_XGT / Y_test$prijspm2 - 1

P_test$pred_XGL <- predict(model_xgbL,X_test_mat)
P_test$PE_XGL <- P_test$pred_XGL / Y_test$prijspm2 - 1

P_test %>%
  summarise(mape_logGLM = mean(abs(PE_logGLM)),
            smpe_logGLM = sqrt(mean(PE_logGLM^2)),
            mape_MLM = mean(abs(PE_MLM)),
            smpe_MLM = sqrt(mean(PE_MLM^2)),
            mape_XGT = mean(abs(PE_XGT)),
            smpe_XGT = sqrt(mean(PE_XGT^2)),
            mape_XGL = mean(abs(PE_XGL)),
            smpe_XGL = sqrt(mean(PE_XGL^2)))

# XGBoost can almost match multilevel approach on MAPE (note: Long/Lat instead of postcode4)

