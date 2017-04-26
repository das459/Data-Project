library(ggmap)
ecoregion = read.csv("spring_data.csv", header = TRUE)

library(dplyr)

springdata = ecoregion %>% filter(Type == "helocrene" | Type == "Helocrene" | Type ==  "rheocrene" | Type ==  "rheochrene" | Type ==  "hanging garden" | Type ==  "hillslope" | Type ==  "Rheocrene")

for(i in 1:length(springdata$Type)){
if(springdata$Type[i] == "rheochrene") springdata$Type[i] = "rheocrene"
if(springdata$Type[i] == "Rheocrene") springdata$Type[i] = "rheocrene"
if(springdata$Type[i] == "Helocrene") springdata$Type[i] = "helocrene"
}

springdata %>% group_by(Type) %>% summarise(n())

ecoregion = data.frame(Type = ecoregion$SpringType1, Lat = ecoregion$LatitudeDD, Lon = ecoregion$LongitudeDD)
box = make_bbox(lat = Lat, lon = Lon, data = ecoregion)
ecoregion.map = get_map(location = box, source = "google", maptype = "terrain")

library(zoo)
library(gtools)
ecoregion = na.replace(ecoregion, "Other")

ggmap(ecoregion.map) + geom_point(data = springdata, aes(x = Lon,y = Lat, color = Type)) + labs(title = "Grand Canyon Ecoregion Springs", color = "Sphere of Discharge", x = "Longitude", y = "Latitude", shape = 23)
