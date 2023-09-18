
#Let's focus on the cruise data. Perhaps just PE477.
#Let's plot the coordinates and add circular plots on these coordinates

PE477<- cruise_abundance[cruise_abundance$Location == 'PE477',]
PE477<- gather(PE477, "Total_Bacteria", "HNA", "LNA", key = "Bacteria", value = "Bacterial_Count")
PE477<- gather(PE477, "Total_Viruses", "V1", "V2", "V3", key = "Viruses", value = "Viral_Count")
PE477<- gather(PE477, "Nitrate", "Nitrite", "Phosphate", 
               "Silicate",
               key = "Nutrients", value = "Nutrient_value")
par(mfrow= c(3,3))

for (i in 1:7){
p<- ggplot(PE477[PE477$Expt_No == i,], aes(fill = as.factor(Depth), y = Nutrient_value, x= Nutrients)) +
  geom_bar(position = "stack", stat= "identity")+
  ylim(0, 65)+
  theme_minimal()+
  coord_polar(start=0)
print(p)
}

p<- ggplot(PE477[PE477$Expt_No == 3,], aes(fill = as.factor(Depth), y = Nutrient_value, x= Nutrients)) +
  geom_bar(position = "stack", stat= "identity")+
  ylim(0, 40)+
  theme_minimal()+
  coord_polar(start=0)
print(p)
ggsave("plot.png", plot=last_plot(), path = ".", width = 40, height = 40, units = "in", dpi = 300)


ggmap()

get_map(location = c(lon = -122.486328, lat = 48.862813),
        color = "color",
        maptype = "roadmap",
        zoom = 12)



map<- get_stamenmap(bbox = c(left = -1.0,
                       bottom = 52.0,
                       right = 5.0,
                       top = 56),
              maptype = "toner-lite",
              color = "bw")
map <- ggmap(map)%>%
  coord_fixed()
ggmap(map) +
  geom_point(data = PE477, aes(x = Longitude, y = Latitude, fill = "red", alpha = 0.8), size = 5, shape = 21) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE)

q<- list(annotation_custom(ggplotGrob(p), 
                           x = 2.0,
                           y = 53))

ggmap(map)+ q

PE477.Grobs<- PE477 %>%
  group_by(Expt_No, Latitude, Longitude, Depth, Nutrients, Nutrient_value) %>%
  do(subplots = ggplot(.,aes(fill = as.factor(Depth), y = Nutrient_value, x= Nutrients)) +
       geom_bar(position = "stack", stat= "identity")+
       ylim(0, 65)+
       theme_minimal()+
       coord_polar(start=0)
                       )%>%
  mutate(subgrobs = list(annotation_custom(ggplotGrob(subplots),
                                           x = Longitude - 1,
                                           y = Latitude - 1,
                                           xmax = Longitude + 1,
                                           ymax = Latitude +1)))
map + PE477.Grobs$subgrobs

#https://stackoverflow.com/questions/47116898/adding-polar-bar-plot-as-separate-object-into-ggplot-ggmap
g<- ggplotGrob(p )

ggmap(map) + annotation_custom(grob = g,
                               xmin = 2.0,
                               xmax = 4.0,
                               ymin = 54.0,
                               ymax = 55.5)



map<- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircles(lat= PE477$Latitude,
             lng = PE477$Longitude,
             icon = list(iconURL = 'plot.png', iconSize= c(40,40)))
             

map

popup


















PE477<- PE477[PE477$Expt_No == '1' & PE477$Depth == 7,]

PE477<- gather(PE477, "Total_Bacteria", "HNA" , "LNA" , 
              # "Total_Viruses" ,"V1",  "V2" , "V3"   ,
               "VBR" ,
               key = "pop", value = "count")
ggplot2::ggplot(PE477, aes(x= as.factor(pop), y=count))+
  geom_bar(stat= "identity", fill = alpha("black", 0.3)) +
  #ylim(-100,120) +
  # theme_minimal() +
  # theme(
  #   axis.text = element_blank(),
  #   axis.title = element_blank(),
  #   panel.grid = element_blank(),
  #   plot.margin = unit(rep(-2,4), "cm")     # This remove unnecessary margin around plot
  # ) +
  coord_polar(start = 0)
