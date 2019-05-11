# Uses ModelJaap.r as well as KNNJaap.r

ggplot() +
  geom_point(data=plot_KNN,aes(x=pred,y=prijs,col="fitK")) +
  geom_point(data=plotdata,aes(x=pred7,y=prijs,col="fit1")) +
  geom_point(data=plotdata,aes(x=pred6,y=prijs,col="fit7")) +
  geom_point(aes(x=XG_pred,y=XG_prijs,col="XG")) +
  labs(title = "Prediction v. Prijs", x="Prediction", y="Price") +
  coord_cartesian(xlim = c(0, 900000),ylim=c(0,900000)) 

ggplot() +
  geom_point(data=plot_KNN,aes(x=pred,y=me,col="fitK")) +
  geom_point(data=plotdata,aes(x=prijs,y=delta7,col="fit1")) +
  geom_point(data=plotdata,aes(x=prijs,y=delta6,col="fit7")) +
  geom_point(aes(x=XG_prijs,y=(XG_pred - XG_prijs),col="XG")) +
  labs(title = "Prijs v. Residual", x="Prijs", y="Residu")  +
  coord_cartesian(xlim = c(0, 900000))

ggplot() +
  geom_point(data=plot_KNN,aes(x=m2,y=me,col="fitK")) +
  geom_point(aes(x=huizen_valid$m2,y=(predict7 - huizen_valid$prijs),col="fit1")) +
  geom_point(aes(x=huizen_valid$m2,y=(predict6 - huizen_valid$prijs),col="fit7")) +
  labs(title = "Oppervlak v. Residual", x="m2", y="Residu")
