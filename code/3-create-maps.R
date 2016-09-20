
################################################################
### Eliminate record without coordinates & create a specific Dataframe for maps
datasp <-data[!rowSums(is.na(data["lat"])), ]

## testing a plot of coordinates
plot(datasp$lat, datasp$long )

###Maps!!

rm(map1)
map1 <-ggplot(aes(x=long, y=lat), size=1, alpha=0.85, data=datasp, color="coral1")+
  geom_point()+  
  #add proportional symbol at centroids
  ## scale_size(name="Total IDP Ind.",labels=format_si())+
  ggtitle("IDPs ")+theme(plot.title=element_text(size=20))
ggsave("out/map1.png", map1, width=8, height=6, units="in", dpi=300)
rm(map1)

rm(map3)
map3 <-ggplot(aes( x=long, y=lat, shape=Housing.Type_of_residence, color=Housing.Type_of_residence), alpha=0.85, data=datasp, size=1, color="coral1")+
  geom_point()+ 
  scale_fill_gradient(low="#ffffcc", high="#ff4444") +
  #add proportional symbol at centroids
  ## scale_size(name="Total IDP Ind.",labels=format_si())+
  ggtitle("IDPs ")+theme(plot.title=element_text(size=20))
ggsave("out/map3.png", map3, width=8, height=6, units="in", dpi=300)
rm(map3)

rm(map4)
map4 <-ggplot(aes( x=long, y=lat, shape=Housing.Assessor_How_do_you_evaluate_, color=Housing.Assessor_How_do_you_evaluate_), alpha=0.85, data=datasp, size=1, color="coral1")+
  geom_point()+ 
  scale_fill_gradient(low="#ffffcc", high="#ff4444") +
  #add proportional symbol at centroids
  ## scale_size(name="Total IDP Ind.",labels=format_si())+
  ggtitle("IDPs ")+theme(plot.title=element_text(size=20))
ggsave("out/map4.png", map4, width=8, height=6, units="in", dpi=300)
rm(map4)






################# Prepare map background ########
# Iraq bbox           38.782   28.833   49.433   37.358

#get_stamenmap(bbox=c(left=-95.80204, bottom=29.38048, right=-94.92313, top=30.14344),
#              zoom=10,
#              maptype=c("terrain", "watercolor", "toner"),
#              crop=TRUE, messaging=FALSE, urlonly=FALSE,
#              filename="ggmapTemp", color=c("color", "bw"))

require(ggplot2)
require(ggmap)

iraqstamentoner <-get_stamenmap(bbox=c(left=37.782, bottom=27.833, right=50.433, top=38.358),
                                zoom=6,
                                maptype="toner",
                                crop=TRUE, messaging=FALSE, urlonly=FALSE,
                                filename="out/iraqstamentoner.png", color= "bw")
toner <- ggmap(iraqstamentoner)
ggsave("out/map-toner.png", toner, width=8, height=6,units="in", dpi=300)

# KRI bbox           
kurdistamentoner <-get_stamenmap(bbox=c(left=41.457, bottom=34.624, right=47.780, top=37.518),
                                zoom=6,
                                maptype="toner-hybrid",
                                crop=TRUE, messaging=FALSE, urlonly=FALSE,
                                filename="out/kurdistamentoner.png", color= "bw")
toner <- ggmap(kurdistamentoner)

toner
ggsave("out/map-toner.png", toner, width=8, height=6,units="in", dpi=300)


kurdiosm <- get_openstreetmap(bbox=c(left=41.457, bottom=34.624, right=47.780, top=37.518), scale=1787796,
                  format="png", messaging=TRUE, urlonly=FALSE,
                  filename="ggmapTemp", color="color")
#kurdiosm1 <- ggmap(kurdiosm)
#kurdiosm1

## Kurdistan 36.3374375,44.0102224,8
googleterrain <- get_map(location=c(lon =44.0102224, lat=36.3374375), zoom=7,  color="bw", source="google", maptype="terrain")

## Erbil  36.1972141,43.868571
erbilterrain <- get_map(location=c(lon =43.868571, lat=36.1972141), zoom=9,  color="bw", source="google", maptype="terrain")

## Sulaymaniyah 35.5630978,45.307443
suliterrain <- get_map(location=c(lon =45.307443, lat=35.5630978), zoom=9,  color="bw", source="google", maptype="terrain")

## Dahuk 36.8543221,42.9246974
dahukterrain <- get_map(location=c(lon =42.9246974, lat=36.8543221), zoom=9,  color="bw", source="google", maptype="terrain")

#### Testing it
# googleeterrain <- ggmap(googleterrain)
#ggsave("plot/map/map-googleterrain.png", googleeterrain, width=8, height=6,units="in", dpi=300)




rm(map1)
map1 <-ggmap(googleterrain)+
  geom_point(aes(size=1, x=long, y=lat, size=factor(Household_information.Family_Size)), shape=21, show_guide=FALSE,
             data=datasp, color="coral1")+  
   
  # map z to area and make larger circles
  scale_size_area(max_size=25, name="Total IDP Ind.",labels=format_si()) +
  #add proportional symbol at centroids
  #scale_size(name="Total IDP Ind.",labels=format_si())+
  coord_cartesian() +
  ggtitle("IDPs ")+
  theme(plot.title=element_text(size=20))
ggsave("out/map2.png", map1, width=8, height=6, units="in", dpi=300)
rm(map1)


str(datasp)


# Let's try hexabin to view the data!
rm(maphex)
maphex <- ggmap(googleterrain)
maphex <- maphex +
  stat_summary_hex(aes(x= long, y= lat, fill=Household_information.Family_Size), data=datasp)+
  #coord_equal() +
  coord_cartesian() +
  theme_bw()+ 
  scale_fill_gradient(low="#ffffcc", high="#ff4444") +
  labs(x="Longitude", y="Latitude", fill="#individuals") +
  ggtitle("Total Ind #")

print(maphex)
ggsave("out/maphex.png", maphex, width=8, height=6,units="in", dpi=300)


#########################################
### Let's map out the zscore in a loop from data frame : subgovcentroid.z

############################################################
# function to map variable
# Now we can start plotting in a loop
# the key is at the line here
# p + aes_string(x=names(mydata)[i])
# Use aes_string instead of aes, so that when you look at summary(ggplot_obj), 
# the mapping for x-values that are changing will be the actual string and not a variable i.


for (i in 4:103 ) {
  #i <- 2
  variablename <- names(subgovcentroid.z)[i]
  title <- attributes(subgovcentroid.z)$variable.labels[i]
  #title <- variablename
  plot <- ggmap(googleterrain)  + 
    geom_point(aes_string( x=names(subgovcentroid.z)[3], y=names(subgovcentroid.z)[2], 
                           colour= names(subgovcentroid.z)[i]), #, size=abs(as.numeric(names(subgovcentroid.z)[i]))
     data=subgovcentroid.z , alpha=1, size=4 ) +
    scale_colour_gradient2() +
    ## adding label box 
    geom_label_repel(aes_string(label= names(subgovcentroid.z)[1], x=names(subgovcentroid.z)[3], y=names(subgovcentroid.z)[2] ),
                     data=subgovcentroid.z ,size=2, fontface='bold', color='black',  box.padding=unit(0.25, "lines"),
                     point.padding=unit(0.5, "lines")) +
    ## Annotation to indicate z-score : Z-scores are expressed in terms of standard deviations from their means
    annotate("text", x=1, y=2.9, size=4, 
             label="Z-scores for Districts or camps are expressed in terms of standard deviations from their means. \n Blue indicates above the global mean") +
    ggtitle(title) +
    #coord_cartesian() +
    theme(
          plot.title=element_text(face="bold", size=8),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.x=element_blank(),axis.ticks=element_blank(),
          axis.text.y=element_blank(),
          plot.background=element_rect(fill="transparent",colour=NA),
          legend.title=element_blank())
  #assign(paste("plot", variablename, sep=""), plot)
  ggsave(filename=paste("out/",variablename,"_map.png",sep=""), plot=plot, width=8, height=8,units="in", dpi=300)
  rm(plot)
}

#############

irq_adm1 <- readShapePoly('geo/irq_admbnda_adm1_ocha_20140717.shp', proj4string=CRS("+proj=longlat"))
#
#plot(irq_adm1)

# Fortify them
irq_adm1@data$id=rownames(irq_adm1@data)

rm(irq_adm1_f)
irq_adm1_f <- fortify(irq_adm1, region="id")
#irq_adm1_f <- merge(irq_adm1_f, irq_adm1@data, by.x="id",by.y="row.names")
irq_adm1_f <-join(irq_adm1_f, irq_adm1@data, by="id")
irq_adm1_f <-join(x=irq_adm1_f, y=govnames, by="A1NameAlt1")

rm(maplevel1)
maplevel1 <-  ggplot(irq_adm1_f, aes(long, lat)) + coord_equal()+
  geom_polygon(data=irq_adm1_f, aes(x=long, y=lat, group=group), alpha=0.5) +
  geom_text(aes(label=short, x=Longitude_c, y=Latitude_c, group=group)) + #add labels at centroids
  geom_path(data=irq_adm1_f, aes(x=long, y=lat, group=group), color="white")+
  ggtitle("Governorates of Iraq")


### getting level 2
irq_adm2 <- readShapePoly('geo/irq_admbnda_adm2_ocha_20140717.shp', proj4string=CRS("+proj=longlat"))
irq_adm2@data$id=rownames(irq_adm2@data)
irq_adm2_f <- fortify(irq_adm2, region="id")
irq_adm2_f <-join(irq_adm2_f, irq_adm2@data, by="id")


