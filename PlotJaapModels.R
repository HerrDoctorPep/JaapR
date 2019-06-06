# Uses ModelJaap.r as well as KNNJaap.r

X_test <- model_data_test[Admissible_X,] %>%
  select(-c("prijs","logprijs","Oorspronkelijkevraagprijs", "Huidigevraagprijs","prijspm2","Aantalkeergetoond", "Aantalkeergetoondgisteren","Geplaatstop"))

Plot_test <- P_test
Plot_test$pred_XGT <-Plot_test$pred_XGT * X_test$Woonoppervlakte
Plot_test$pred_XGL <-Plot_test$pred_XGL * X_test$Woonoppervlakte


# Some plots to show how regression is doing
ggplot() +
  geom_point(data=Plot_test,aes(x=pred_logGLM,y=pred_logGLM-Price,col="GLM")) +
  geom_point(data=Plot_test,aes(x=pred_MLM,y=pred_MLM-Price,col="MLM")) +
  geom_point(data=Plot_test,aes(x=pred_XGT,y=pred_XGT-Price,col="XGT")) +
  labs(title = "Prijs v. Residual", x="Predicted price", y="Residue")  +
  coord_cartesian(xlim = c(0, 900000))

ggplot() +
  geom_point(data=Plot_test,aes(x=pred_logGLM,y=PE_logGLM,col="GLM")) +
  geom_point(data=Plot_test,aes(x=pred_MLM,y=PE_MLM,col="MLM")) +
  geom_point(data=Plot_test,aes(x=pred_XGT,y=PE_XGT,col="XGT")) +
  labs(title = "Prijs v. % error", x="Predicted price", y="% error")  +
  coord_cartesian(xlim = c(0, 900000))
