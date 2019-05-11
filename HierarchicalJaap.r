#
# Let's createa dendrogram of how close houses are
#

# 

# Maybedistance gives a better groupingthan postal code

huizen_geo <- huizen_clean

clusters <- hclust(dist(cbind(huizen_geo$Longitude,huizen_clean$Latitude)),method='complete')
plot(clusters)
huizen_geo$clustercut <- cutree(clusters, length(unique(huizen_geo$postcode4)))

huizen_geo %>%
  group_by(postcode4) %>%
  summarise(clusters = n_distinct(clustercut))

huizen_geo %>%
  group_by(clustercut) %>%
  summarise(clusters = n_distinct(postcode4))


ggmap(map_Roffa13) +
  geom_point(data = huizen_geo, aes(x = Longitude, y = Latitude,colour=clustercut), size = 2) +
  labs(title = "Woninggrootte in Rotterdam", x="Longitude", y="Lattitude")

ggmap(map_Roffa13) +
  geom_point(data = huizen_geo, aes(x = Longitude, y = Latitude,colour=as.integer(postcode4)), size = 2) +
  labs(title = "Woninggrootte in Rotterdam", x="Longitude", y="Lattitude")
