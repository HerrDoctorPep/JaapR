
# Create table by postal code

huizen_bypc <- model_data %>%
  group_by(longitude,latitude) %>%
  summarise(m2_avg = mean(Woonoppervlakte),
            prijs_avg = mean(prijs),
            kamers_avg = mean(Kamers),
            Longitude = first(longitude),
            Latitude = first(latitude)) %>%
  mutate(prijspm2_avg = prijs_avg / m2_avg,
         m2pkamer = m2_avg / kamers_avg)


library(ggmap)
library(maps)
library(mapdata)

GoogleAPI_key <- read_file("Google_key.txt")

# ggplot() +
#   geom_polygon(data = map_data("world", region="netherlands"), aes(x=long, y = lat, group = group),colour="grey",fill="lightgrey") +
#   coord_fixed(1.3) +
#   geom_point(data = huizen_bypc, aes(x = Longitude, y = Latitude,colour=log(prijs_avg)), size = 1) +
#   labs(title = "Huizenprijzen in Rotterdam", x="Longitude", y="Lattitude")

register_google(GoogleAPI_key)
map_Roffa13 <- get_googlemap(c(4.48,51.915), zoom=13)

ggmap(map_Roffa13) +
  geom_point(data = huizen_forplot, aes(x = Longitude, y = Latitude,colour=log(prijs_avg)), size = 2) +
  labs(title = "Huizenprijzen in Rotterdam", x="Longitude", y="Lattitude")

ggmap(map_Roffa13) +
  geom_point(data = huizen_bypc, aes(x = Longitude, y = Latitude,colour=prijspm2_avg), size = 2) +
  labs(title = "Vierkantemeterprijzen in Rotterdam", x="Longitude", y="Lattitude")

ggmap(map_Roffa13) +
  geom_point(data = subset(huizen_bypc,m2_avg>100), aes(x = Longitude, y = Latitude,colour=prijspm2_avg), size = 2) +
  labs(title = "Vierkantemeterprijzen in Rotterdam (m²>100)", x="Longitude", y="Lattitude")

ggmap(map_Roffa13) +
  geom_point(data = huizen_bypc, aes(x = Longitude, y = Latitude,colour=m2_avg), size = 2) +
  labs(title = "Woninggrootte in Rotterdam", x="Longitude", y="Lattitude")


