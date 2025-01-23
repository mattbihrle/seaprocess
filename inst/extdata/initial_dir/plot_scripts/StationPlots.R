# This script will create quick station plots WITH bathymetry data. This script
# will take a couple minutes to run, but creates more detailed maps. Follow the
# steps in "Setup" and run the whole script to generate plots. For specific
# plots, copy and paste a reuse as desired.

#Code for these plots adapted from "chemproperties_SK.R"

library(ggOceanMaps)
library(seaprocess)

#--------------------------------##SETUP##---------------------------------------

# Set Cruise ID
cruiseID <-""

# Point R to the folder with the data you want to use
csv_folder <- "output/csv"

# Tell R where to output your plots
plots_output <- "output/plots"

#-------------------------------------------------------------------------------
## CHECK IN WITH 1ST MT BEFORE EDITING BELOW THIS LINE
#-------------------------------------------------------------------------------

# Find all files in the csv folder to plot (excluding elg, adcp, ctd profiles)
files <- list.files(csv_folder, pattern = "datasheet.csv", full.names = T)

#Loop through to create a bunch of little plots for each deployment
for (i in 1:length(files)){

#Load in a datasheet
data <- readr::read_csv(files[i], show_col_types = F)
# Create a start and end date
date_start <- as.character(dplyr::first(data$dttm) |>
                             substr(1, 10))
date_end <- as.character(dplyr::last(data$dttm) |>
                           substr(1,10))
title <- paste(files[i]) |>
  # Extract a for the plot by searching for any character that is between "_"
  stringr::str_extract("(?<=_).*(?=_)") |>
  # Replace underscores with spaces
  stringr::str_replace_all("_", " ") |>
  # Capitalize the first letter of the words
  stringr::str_to_title()

# Set Lat and Long limits for the plot
limits <- plot_limits(data)

# Note: if the 'plot_limits' function isn't working you can set the limits
# manually as a string that looks like: c(lon, lon, lat, lat)


#---------------------## CREATE your plots!##----------------------------------


Map <- ggOceanMaps::basemap(limits=limits, shapefiles="DecimalDegree",
                               rotate = T, bathy.style = "rcb", legends = F) +
  ggspatial::geom_spatial_point(data = data, color = "blue", aes(x = lon, y = lat), size = 1) +
  ggtitle(paste(cruiseID, title, "Map", sep = " "),
          subtitle = paste(date_start, date_end, sep = " - ")) +
  xlab(expression(bold("Longitude")))+
  ylab(expression(bold("Latitude")))+
  theme(legend.position = "right",
        legend.direction="vertical",
        legend.key = element_blank(),
        legend.title = element_blank(),
        legend.key.height = unit(0.6, "cm"),
        legend.background = element_blank(),
        # # panel.grid.major = element_blank(),
        # # panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=2),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(size=14, color="black", family="serif", face="bold"),
        legend.text = element_text(size=14, color="black", family="serif", face = "bold"),
        axis.text.x = element_text(size=14, color="black", family="serif"),
        axis.text.y = element_text(size=14, color="black", family="serif"),
        axis.title.x = element_text(size=18, color="black", family="serif"),
        axis.title.y = element_text(size=18, color="black", family="serif"))

# Show the plot
print(Map)

# Save the plot
ggsave(paste(cruiseID, title, "map.jpg", sep = "_"), plot = Map,
       scale = 4, path = plots_output)
}
