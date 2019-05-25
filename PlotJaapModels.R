# Uses ModelJaap.r as well as KNNJaap.r

X_test <- model_data_test[Admissible_X,] %>%
  select(-c("prijs","logprijs","Oorspronkelijkevraagprijs", "Huidigevraagprijs","prijspm2"))

# Some plots to show how regression is doing
ggplot() +
  geom_point(data=P_test,aes(x=pred_logGLM,y=pred_logGLM-Price,col="GLM")) +
  geom_point(data=P_test,aes(x=pred_MLM,y=pred_MLM-Price,col="MLM")) +
  labs(title = "Prijs v. Residual", x="Predicted price", y="Residue")  +
  coord_cartesian(xlim = c(0, 900000))

ggplot() +
  geom_point(data=P_test,aes(x=pred_logGLM,y=PE_logGLM,col="GLM")) +
  geom_point(data=P_test,aes(x=pred_MLM,y=PE_MLM,col="MLM")) +
  labs(title = "Prijs v. % error", x="Predicted price", y="% error")  +
  coord_cartesian(xlim = c(0, 900000))

# Might be worthwhile to add Heightof ceiling...

ggplot() +
  geom_point(data=P_test,aes(x=X_test$Inhoud/X_test$Woonoppervlakte,y=PE_logGLM,col="GLM",alpha=0.5)) +
  geom_point(data=P_test,aes(x=X_test$Inhoud/X_test$Woonoppervlakte,y=PE_MLM,col="MLM",alpha=0.5)) +
  labs(title = "Plafondhoogte v. % error", x="Gem. plafondhoogte", y="% error")

ggplot() +
  geom_histogram(data = P_test,aes(PE_logGLM))

ggplot() +
  geom_histogram(data = P_test,aes(PE_MLM))
