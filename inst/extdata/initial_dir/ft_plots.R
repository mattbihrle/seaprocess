library(ggOceanMaps)
library(seaprocess)

#--------------------------------##SETUP##---------------------------------------

# Set Cruise ID
cruiseID <-"S314"
#Point R to the folder with the data you want to use
csv_folder <- "output/csv"

#Tell R where to output your plots
plots_output <- "output/plots"

#Search through the csv folder and read in the elg file
elg <-
  list.files(csv_folder, pattern = "elg.csv", full.names = T) |>
  readr::read_csv(show_col_types = F)

#Create a start and end date
date_start <- as.character(dplyr::first(elg$dttm) |>
                             substr(1, 10))
date_end <- as.character(dplyr::last(elg$dttm) |>
                           substr(1,10))

#Set Lat and Long limits for the plot
limits <- plot_limits(elg)

#Note: if the 'plot_limits' function isn't working you can set the limits
# manually as a string that looks like: c(lon, lon, lat, lat)


#---------------------## CREATE your plots!##------------------------------------

#First Surface Temp-------------------------------------------------------------

Map.SST <- ggOceanMaps::basemap(limits=limits, shapefiles="DecimalDegree", rotate = T, bathy.style = "rcb", legends = F) +
  labs(x = "Long", y = "Lat") +
  ggspatial::geom_spatial_point(data = elg, aes(x = lon, y = lat, color = temp), size = 1) +
  scale_color_gradient(low="blue", high="red") +
  ggtitle(paste(cruiseID, "Surface Temp (C)", sep = " - "),
          subtitle = paste(date_start, date_end, sep = " - ")) +
  xlab(expression(bold("Longitude")))+
  ylab(expression(bold("Latitude")))+
  theme(legend.position = "right",
        legend.direction="vertical",
        legend.key = element_blank(),
        legend.title = element_blank(),
        legend.key.height = unit(0.6, "cm"),
        legend.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=2),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(size=14, color="black", family="serif", face="bold"),
        legend.text = element_text(size=14, color="black", family="serif", face = "bold"),
        axis.text.x = element_text(size=14, color="black", family="serif"),
        axis.text.y = element_text(size=14, color="black", family="serif"),
        axis.title.x = element_text(size=18, color="black", family="serif"),
        axis.title.y = element_text(size=18, color="black", family="serif"))

#Show the plot
Map.SST

#save the plot
ggsave(paste(cruiseID, "surface_temp.jpg", sep = "_"), plot = Map.SST,
       scale = 4, path = plots_output)
#-------------------------------------------------------------------------------
#Run it back but this time with Salinity

Map.SSS <- ggOceanMaps::basemap(limits=limits, shapefiles="DecimalDegree",
                                rotate = T, bathy.style = "rcb", legends = F) +
  labs(x = "Long", y = "Lat") +
  ggspatial::geom_spatial_point(data = elg, aes(x = lon, y = lat, color = sal), size = 1) +
  scale_color_gradient(low="green", high="red")+
  #scale_color_viridis()+
  #scale_color_viridis_c()+
  ggtitle(paste(cruiseID, "Sea Surface Salinity (psu)", sep = "-")) +
  xlab(expression(bold("Longitude")))+
  ylab(expression(bold("Latitude")))+
  theme(legend.position = "right",
        legend.direction="vertical",
        legend.key = element_blank(),
        legend.title=element_blank(),
        legend.key.height = unit(0.6, "cm"),
        legend.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=2),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(size=14, color="black", family="serif", face="bold"),
        legend.text = element_text(size=14, color="black", family="serif", face = "bold"),
        axis.text.x = element_text(size=14, color="black", family="serif"),
        axis.text.y = element_text(size=14, color="black", family="serif"),
        axis.title.x = element_text(size=18, color="black", family="serif"),
        axis.title.y = element_text(size=18, color="black", family="serif"))

#show the plot
Map.SSS
#save the plot
ggsave(paste(cruiseID, "surface_sal.jpg", sep = "_"), plot = Map.SSS,
       scale = 4, path = plots_output)

#And again with Fluorescence----------------------------------------------------
Map.SSF <- ggOceanMaps::basemap(limits=plot_limits(elg),
                                shapefiles="DecimalDegree",
                                rotate = T, bathy.style = "rcb", legends = F) +
  labs(x = "Long", y = "Lat") +
  ggspatial::geom_spatial_point(data = elg, aes(x = lon, y = lat, color = fluor), size = 1) +
  scale_color_gradient(low="blue", high="red")+
  ggtitle(paste(cruiseID, "Surface Chla Fluorescence")) +
  xlab(expression(bold("Longitude")))+
  ylab(expression(bold("Latitude")))+
  theme(legend.position = "right",
        legend.direction="vertical",
        legend.key = element_blank(),
        legend.title=element_blank(),
        legend.key.height = unit(0.6, "cm"),
        legend.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=2),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(size=14, color="black", family="serif", face="bold"),
        legend.text = element_text(size=14, color="black", family="serif", face = "bold"),
        axis.text.x = element_text(size=14, color="black", family="serif"),
        axis.text.y = element_text(size=14, color="black", family="serif"),
        axis.title.x = element_text(size=18, color="black", family="serif"),
        axis.title.y = element_text(size=18, color="black", family="serif"))

#show the plot
Map.SSF
#save the plot
ggsave(paste(cruiseID, "surface_fluor.jpg", sep = "_"), plot = Map.SSF,
       scale = 4, path = plots_output)

