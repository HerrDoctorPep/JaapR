# 
# XGBoost algorithm
# Use the Caret package 
# Based on https://www.machinelearningplus.com/machine-learning/caret-package/
#

# Build on the work done in CaretGLMJaap.r

# No action for missing values needed

# OneHot encoding

X_train <- model_data_train %>%
  select(-c("prijs","logprijs","Oorspronkelijkevraagprijs", "Huidigevraagprijs","prijspm2","Aantalkeergetoond", "Aantalkeergetoondgisteren","Geplaatstop"))
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

modelLookup("xgbTree")

fitControl <- trainControl(
  method = 'cv',                   # k-fold cross validation
  number = 5,                      # number of folds
  savePredictions = 'final'        # saves predictions for optimal tuning parameter
)

grid_xgbT <- expand.grid(nrounds = c(150,200,250),
                         max_depth = 3,
                         eta = 0.3,
                         gamma = 0.01,
                         colsample_bytree = 0.7,
                         min_child_weight = 0.8,
                         subsample = 1
)



model_xgbT <- train(prijspm2 ~., 
                    data = bind_cols(X_train_mat,y_train[,'prijspm2']), 
                    method='xgbTree',
                    tuneGrid = grid_xgbT,
                    trControl = fitControl
)

summary(model_xgbT)
plot(model_xgbT)

# Train the XGBoost model (Linear)

modelLookup("xgbLinear")

grid_xgbL <- expand.grid(nrounds = c(100,150,200),
                         lambda = c(0.3,0.4),
                         alpha = 0.1,
                         eta = 0.2
)

model_xgbL <- train(prijspm2 ~., 
                    data = bind_cols(X_train_mat,y_train[,'prijspm2']), 
                    method='xgbLinear',
                    tuneGrid = grid_xgbL,
                    trControl = fitControl
)

print(model_xgbL)
plot(model_xgbL)

# use trained model on the test data

Admissible_X <- (model_data_test$Type %in% X_train$Type) & (model_data_test$postcode4 %in% X_train$postcode4)

X_test <- model_data_test[Admissible_X,] %>%
  select(-c("prijs","logprijs","Oorspronkelijkevraagprijs", "Huidigevraagprijs","prijspm2"))

Y_test <- model_data_test[Admissible_X,] %>%
  select(c("prijs","logprijs", "prijspm2","Oorspronkelijkevraagprijs", "Huidigevraagprijs"))

X_test <- predict(normalize_model,newdata = X_test)

X_test_mat <- as_tibble(predict(dummies_model, newdata = X_test)) %>%
  mutate(BijzonderhedenGemeubileerd = 0,
         BijzonderhedenLiftZwembad = 0)

# Outcomes

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
            smpe_XGL = sqrt(mean(PE_XGL^2))) %>%
  print(n=Inf)

