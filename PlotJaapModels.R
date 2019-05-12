# Uses ModelJaap.r as well as KNNJaap.r

# Some plots to show how regression is doing
ggplot() +
  geom_point(data=regression_results,aes(x=pred2,y=pred2-prijs,col="fit2")) +
  geom_point(data=ML_results,aes(x=pred4,y=pred4-prijs,col="fit4")) +
  labs(title = "Prijs v. Residual", x="Predicted price", y="Residue")  +
  coord_cartesian(xlim = c(0, 900000))

ggplot() +
  geom_point(data=regression_results,aes(x=pred2,y=pe2,col="fit2")) +
  geom_point(data=ML_results,aes(x=pred4,y=pe4,col="fit4")) +
  labs(title = "Prijs v. % error", x="Predicted price", y="% error")  +
  coord_cartesian(xlim = c(0, 900000))

ggplot() +
  geom_point(data=regression_results,aes(x=prijs,y=pe2,col="fi2")) +
  geom_point(data=ML_results,aes(x=prijs,y=pe4,col="fit4")) +
  labs(title = "Prijs v. % error", x="Price", y="% error")  +
  coord_cartesian(xlim = c(0, 900000))
