## testing a plot of coordinates
plot(datasp$lat, datasp$long )

###Maps!!

rm(map1)
map1 <-ggplot(aes(x = long, y = lat), size = 1, alpha = 0.85, data=datasp, color="coral1")+
  geom_point()+  
  #add proportional symbol at centroids
  ## scale_size(name="Total IDP Ind.",labels=format_si())+
  ggtitle("IDPs ")+theme(plot.title=element_text(size=20))
ggsave("out/map1.png", map1, width=8, height=6, units="in", dpi=300)
rm(map1)

rm(map3)
map3 <-ggplot(aes( x = long, y = lat, shape=Housing.Type_of_residence, color=Housing.Type_of_residence), alpha = 0.85, data=datasp, size = 1, color="coral1")+
  geom_point()+ 
  scale_fill_gradient(low = "#ffffcc", high = "#ff4444") +
  #add proportional symbol at centroids
  ## scale_size(name="Total IDP Ind.",labels=format_si())+
  ggtitle("IDPs ")+theme(plot.title=element_text(size=20))
ggsave("out/map3.png", map3, width=8, height=6, units="in", dpi=300)
rm(map3)

rm(map4)
map4 <-ggplot(aes( x = long, y = lat, shape=Housing.Assessor_How_do_you_evaluate_, color=Housing.Assessor_How_do_you_evaluate_), alpha = 0.85, data=datasp, size = 1, color="coral1")+
  geom_point()+ 
  scale_fill_gradient(low = "#ffffcc", high = "#ff4444") +
  #add proportional symbol at centroids
  ## scale_size(name="Total IDP Ind.",labels=format_si())+
  ggtitle("IDPs ")+theme(plot.title=element_text(size=20))
ggsave("out/map4.png", map4, width=8, height=6, units="in", dpi=300)
rm(map4)






################# Prepare map background ########
# Iraq bbox           38.782   28.833   49.433   37.358

#get_stamenmap(bbox = c(left = -95.80204, bottom = 29.38048, right = -94.92313, top = 30.14344),
#              zoom = 10,
#              maptype = c("terrain", "watercolor", "toner"),
#              crop = TRUE, messaging = FALSE, urlonly = FALSE,
#              filename = "ggmapTemp", color = c("color", "bw"))

require(ggplot2)
require(ggmap)

iraqstamentoner <-get_stamenmap(bbox = c(left = 37.782, bottom = 27.833, right = 50.433, top = 38.358),
                                zoom = 6,
                                maptype = "toner",
                                crop = TRUE, messaging = FALSE, urlonly = FALSE,
                                filename = "out/iraqstamentoner.png", color =  "bw")
toner <- ggmap(iraqstamentoner)
ggsave("out/map-toner.png", toner, width=8, height=6,units="in", dpi=300)

# KRI bbox           
kurdistamentoner <-get_stamenmap(bbox = c(left = 41.457, bottom = 34.624, right = 47.780, top = 37.518),
                                zoom = 6,
                                maptype = "toner-hybrid",
                                crop = TRUE, messaging = FALSE, urlonly = FALSE,
                                filename = "out/kurdistamentoner.png", color =  "bw")
toner <- ggmap(kurdistamentoner)

toner
ggsave("out/map-toner.png", toner, width=8, height=6,units="in", dpi=300)


kurdiosm <- get_openstreetmap(bbox = c(left = 41.457, bottom = 34.624, right = 47.780, top = 37.518), scale = 1787796,
                  format = "png", messaging = TRUE, urlonly = FALSE,
                  filename = "ggmapTemp", color = "color")
kurdiosm1 <- ggmap(kurdiosm)
kurdiosm1
#osm <- get_openstreetmap(bbox = c(left = 37.782, bottom = 27.833, right = 50.433, top = 38.358),
#                         scale = OSM_scale_lookup(6))
#osmm <- ggmap(osm)
#ggsave("plot/map/map-osm.png", osmm, width=8, height=6,units="in", dpi=300)

## Kursitan 36.3374375,44.0102224,8
 # googleterrain <- get_map(location = c(lon =44.538574, lat = 32.750323),  color = "color", source = "google",maptype = "terrain", zoom = 6)

googleterrain <- get_map(location = c(lon =44.0102224, lat = 36.3374375), zoom = 7,  color = "color", source = "google",maptype = "terrain")
erbilterrain <- get_map(location = c(lon =44.0102224, lat = 36.3374375), zoom = 7,  color = "color", source = "google",maptype = "terrain")
suliterrain <- get_map(location = c(lon =44.0102224, lat = 36.3374375), zoom = 7,  color = "color", source = "google",maptype = "terrain")
dahukterrain <- get_map(location = c(lon =44.0102224, lat = 36.3374375), zoom = 7,  color = "color", source = "google",maptype = "terrain")

googleeterrain <- ggmap(googleterrain)
#ggsave("plot/map/map-googleterrain.png", googleeterrain, width=8, height=6,units="in", dpi=300)




rm(map1)
map1 <-ggmap(googleterrain)+
  geom_point(aes(size = 1, x = long, y = lat, size = factor(Household_information.Family_Size)), shape = 21, show_guide = FALSE,
             data=datasp, color="coral1")+  
   
  # map z to area and make larger circles
  scale_size_area(max_size = 25, name="Total IDP Ind.",labels=format_si()) +
  #add proportional symbol at centroids
  #scale_size(name="Total IDP Ind.",labels=format_si())+
  coord_cartesian() +
  ggtitle("IDPs ")+
  theme(plot.title=element_text(size=20))
ggsave("out/map2.png", map1, width=8, height=6, units="in", dpi=300)
rm(map1)


str(datasp)

datasp1 <- datasp[ , c("lat", "long", "X.R_PMT_v1.Household_information.Family_Size") ]
str(datasp1)

# Let's try hexabin to view the data!
rm(maphex)
maphex <- ggmap(googleterrain)
maphex <- maphex +
  stat_summary_hex(aes(x= long, y= lat, fill = X.R_PMT_v1.Household_information.Family_Size), data=datasp1)+
  #coord_equal() +
  coord_cartesian() +
  theme_bw()+ 
  scale_fill_gradient(low = "#ffffcc", high = "#ff4444") +
  labs(x = "Longitude", y = "Latitude", fill = "#individuals") +
  ggtitle("Total Ind #")

print(maphex)
ggsave("out/maphex.png", maphex, width=8, height=6,units="in", dpi=300)



#############

irq_adm1 <- readShapePoly('geo/irq_admbnda_adm1_ocha_20140717.shp', proj4string=CRS("+proj=longlat"))
#
#plot(irq_adm1)

# Fortify them
irq_adm1@data$id = rownames(irq_adm1@data)

rm(irq_adm1_f)
irq_adm1_f <- fortify(irq_adm1, region="id")
#irq_adm1_f <- merge(irq_adm1_f, irq_adm1@data, by.x="id",by.y="row.names")
irq_adm1_f <-join(irq_adm1_f, irq_adm1@data, by="id")
irq_adm1_f <-join(x=irq_adm1_f, y=govnames, by="A1NameAlt1")

rm(maplevel1)
maplevel1 <-  ggplot(irq_adm1_f, aes(long, lat)) + coord_equal()+
  geom_polygon(data = irq_adm1_f, aes(x = long, y = lat, group = group), alpha = 0.5) +
  geom_text(aes(label = short, x = Longitude_c, y = Latitude_c, group = group)) + #add labels at centroids
  geom_path(data = irq_adm1_f, aes(x = long, y = lat, group = group), color="white")+
  ggtitle("Governorates of Iraq")


### getting level 2
irq_adm2 <- readShapePoly('geo/irq_admbnda_adm2_ocha_20140717.shp', proj4string=CRS("+proj=longlat"))
irq_adm2@data$id = rownames(irq_adm2@data)
irq_adm2_f <- fortify(irq_adm2, region="id")
irq_adm2_f <-join(irq_adm2_f, irq_adm2@data, by="id")

