#Map Construction Script
# Preamble
###############################################################################################
rm(list=ls())
install.packages("tmap")
install.packages("devtools")
library(ggspati)
install.pack
library(terra)

install.packages("terra")
install.packages("basemaps")
devtools::install_github("16EAGLE/basemaps")

library(basemaps)

library(tmap)
library(tidyverse)
library(tidycensus)
library(ggplot2)
library(gt) 
library(readr)
library(sf)
library(stringr)
library(dplyr)
library(scales)
library(patchwork)
library(cowplot)
library(viridis)
library(stargazer)
library(broom)
library(maps)
library(readxl)
library(lakes)
library(ggspatial)
library(foreign)

library(ggmap)

library(remotes)

###############################################################################################

setwd("/Users/geddylucier/Dropbox/Scuba.Tech/SdT-CT_AIS-Geddy-Winter-2024")

# Read the DBF file
dbf_data <- read.dbf("path_to_file/National_LakePoly_withMetrics_20090929.dbf")


lake_data <- st_read("shapefiles/Lake Polygon/NLA_2017_Lake_Polygon.dbf")


plot(lake_data)


gdb_path <- "shapefiles/fef0ad6f-84a0-42e6-942a-ed3172551e0c.gdb"
layers <- st_layers(gdb_path)
ct_lakes <- st_read(gdb_path)

candlewood_bbox <- st_as_sfc(st_bbox(c(
  xmin = -73.52,  # Minimum longitude (left)
  xmax = -73.42,  # Maximum longitude (right)
  ymin = 41.43,   # Minimum latitude (bottom)
  ymax = 41.53    # Maximum latitude (top)
), crs = 4326))

ct_lakes <- st_transform(data, crs = 4326)

candlewood_lake <- ct_lakes %>%
  filter(st_intersects(SHAPE, candlewood_bbox, sparse = FALSE)) #%>%
  filter(SHAPE_Length > 19000)

plot(candlewood_lake)


ggplot() +
  geom_sf(data = candlewood_lake, fill = "lightblue", color = "darkblue") +
  labs(
    title = "Map of Selected Layer",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal()

ggplot() +
  geom_sf(data = ct_lakes, fill = "lightblue", color = "darkblue") +
  labs(
    title = "Map of Selected Layer",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal()




?write_csv
write_csv(candlewood, file = "candlewood.csv")
###############################################################################################
#cleaning

sev <- sev %>%
  mutate(species_poplulated = case_when(
    
  )
    
    
    
    
  )










###############################################################################################
#2007 Plot
sev <- read_csv("transect_data/csv/2007.csv")

ggplot() +
  # Plot Candlewood Lake
  geom_sf(data = candlewood_lake, fill = "lightblue", color = "darkblue") +
  
  # Overlay points from sev dataset with smaller red dots
  geom_point(
    data = sev %>%
      filter(Lattitude < 41.8) %>%
      filter(rowSums(select(., (which(names(.) == "Weather") + 1):ncol(.)) != 0) > 0),
    aes(x = Longitude, y = Lattitude),
    color = "red",
    size = 1  # Reduce dot size
  ) +
  
  # Add labels and themes
  labs(
    title = "Candlewood Lake and Species Data",
    fill = "Lake Area (sq. km)"  # Adjust the legend title if needed
  ) +
  theme_minimal() +
  
  # Adjust legend position and spacing
  theme(
    legend.position = "right",            # Move the legend to the right
    legend.spacing = unit(2, "cm"),       # Add more space between the legend and the plot
    plot.margin = margin(10, 50, 10, 10)  # Add space on the right side for the legend (left, top, right, bottom)
  ) +
  
  # Zoom in using coord_sf
  coord_sf(
    xlim = c(-73.52, -73.40),  # Adjust longitude range for more space on the left
    ylim = c(41.43, 41.53)     # Keep the latitude range
  )


#########################################

library(dplyr)
install.packages("ggforce")
library(ggforce)


sev_long <- sev %>%
  filter(Lattitude < 41.8) %>%
  pivot_longer(
    cols = PotFol:SpiPol,  # Include all species columns
    names_to = "species",
    values_to = "value"
  ) %>%
  group_by(Lattitude, Longitude) %>%
  mutate(
    total = sum(value),  # Total value for normalization
    fraction = value / total,  # Fraction for each species
    start = cumsum(lag(fraction, default = 0)) * 2 * pi,  # Start angle
    end = cumsum(fraction) * 2 * pi                      # End angle
  ) %>%
  ungroup()
#########################################

ggplot() +
  # Plot Candlewood Lake
  geom_sf(data = candlewood_lake, fill = "lightblue", color = "darkblue") +
  
  # Overlay points from sev dataset with smaller red dots
  geom_point(
    data = sev_long%>%
      filter(Lattitude < 41.8) %>%
      filter(rowSums(select(., (which(names(.) == "Weather") + 1):ncol(.)) != 0) > 0),
    aes(x = Longitude, y = Lattitude),
    color = "species",
    size = 1  # Reduce dot size
  ) +
  
  # Add labels and themes
  labs(
    title = "Candlewood Lake and Species Data",
    fill = "Lake Area (sq. km)"  # Adjust the legend title if needed
  ) +
  theme_minimal() +
  
  # Adjust legend position and spacing
  theme(
    legend.position = "right",            # Move the legend to the right
    legend.spacing = unit(2, "cm"),       # Add more space between the legend and the plot
    plot.margin = margin(10, 50, 10, 10)  # Add space on the right side for the legend (left, top, right, bottom)
  ) +
  
  # Zoom in using coord_sf
  coord_sf(
    xlim = c(-73.52, -73.40),  # Adjust longitude range for more space on the left
    ylim = c(41.43, 41.53)     # Keep the latitude range
  )

###
ggplot() +
  # Plot Candlewood Lake
  geom_sf(data = candlewood_lake, fill = "lightblue", color = "darkblue") +
  
  # Overlay points with size based on total species count and color by species
  geom_point(
    data = sev_long %>%
      filter(Lattitude < 41.8),  # Filter for the desired area
    aes(
      x = Longitude,
      y = Lattitude,       # Use the total column for size
      color = species     # Use the species column for color
    )
  ) +
  
  # Add labels and themes
  labs(
    title = "Candlewood Lake 2007 Species Data",
    size = "Total Species",      # Legend title for size
    color = "Species Category"   # Legend title for color
  ) +
  theme_minimal() +
  
  # Adjust legend position and spacing
  theme(
    legend.position = "right",            # Move the legend to the right
    legend.spacing = unit(2, "cm"),       # Add more space between the legend and the plot
    plot.margin = margin(10, 50, 10, 10)  # Add space on the right side for the legend (left, top, right, bottom)
  ) +
  
  # Zoom in using coord_sf
  coord_sf(
    xlim = c(-73.52, -73.40),  # Adjust longitude range for more space on the left
    ylim = c(41.43, 41.53)     # Keep the latitude range
  )+
  theme(
    axis.text = element_blank(),          # Remove axis text
    axis.ticks = element_blank(),         # Remove axis ticks
    axis.title = element_blank(),         # Remove axis titles
    panel.grid = element_blank()          # Remove grid lines
  )



##########
sev_long <- sev %>%
  filter(Lattitude < 41.8) %>%
  pivot_longer(
    cols = PotFol:SpiPol,  # Replace with your species column names
    names_to = "species",
    values_to = "value"
  ) %>%
  group_by(Lattitude, Longitude) %>%
  mutate(
    total = sum(value),                   # Calculate total species value for normalization
    fraction = value / total,             # Compute the fraction for each species
    start = cumsum(lag(fraction, default = 0)) * 2 * pi,  # Start angle in radians
    end = cumsum(fraction) * 2 * pi                        # End angle in radians
  ) %>%
  ungroup() %>%
  filter(total > 0)


ggplot() +
  # Plot Candlewood Lake geometry
  geom_sf(data = candlewood_lake, fill = "lightblue", color = "darkblue") +
  
  # Add jittered points with size proportional to total and color by species
  geom_point(
    data = sev_long,
    aes(
      x = Longitude + runif(n(), -0.001, 0.001),  # Add jitter to Longitude
      y = Lattitude + runif(n(), -0.001, 0.001),  # Add jitter to Latitude
      size = sqrt(total),                         # Scale size by total
      color = species                             # Color by species
    ),
    inherit.aes = FALSE
  ) +
  
  # Add labels and themes
  labs(
    title = "Candlewood Lake with Jittered Points",
    size = "Total Species",      # Legend title for size
    color = "Species Category"   # Legend title for color
  ) +
  
  # Remove unused species from the legend
  scale_color_discrete(drop = TRUE) +
  scale_size_continuous(range = c(2, 8)) +  # Control point size range
  
  # Minimal theme
  theme_minimal() +
  
  # Remove axis labels and grid
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "right"
  ) +
  
  # Set plot limits
  coord_sf(
    xlim = c(-73.50, -73.42),  # Narrow longitude range for north side
    ylim = c(41.49, 41.53)     # Focus on higher latitude values
  )
###########

# Add jittered coordinates to the dataset
set.seed(123)  # For reproducibility
sev_long_jittered <- sev_long %>%
  mutate(
    jittered_Longitude = Longitude + runif(nrow(sev_long), -0.001, 0.001),  # Add jitter to Longitude
    jittered_Lattitude = Lattitude + runif(nrow(sev_long), -0.001, 0.001)   # Add jitter to Latitude
  )

# Plot with jittered points
ggplot() +
  # Plot Candlewood Lake geometry
  geom_sf(data = candlewood_lake, fill = "lightblue", color = "black") +
  
  # Add jittered points with size proportional to total and color by species
  geom_point(
    data = sev_long_jittered,
    aes(
      x = jittered_Longitude,
      y = jittered_Lattitude,
      size = sqrt(total),      # Scale size by total
      color = species
    ),
    inherit.aes = FALSE
  ) +
  
  # Add labels and themes
  labs(
    title = "Candlewood Lake with Jittered Points",
    size = "Total Species",      # Legend title for size
    color = "Species Category"   # Legend title for color
  ) +
  
  # Remove unused species from the legend
  scale_color_discrete(drop = TRUE) +
  scale_size_continuous(range = c(2, 8)) +  # Control point size range
  
  # Minimal theme
  theme_minimal() +
  
  # Remove axis labels and grid
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "right"
  ) +
  
  # Set plot limits
  coord_sf(
    xlim = c(-73.50, -73.42),  # Narrow longitude range for north side
    ylim = c(41.49, 41.53)     # Focus on higher latitude values
  )
###########
library(sf)
library(dplyr)
library(ggplot2)
library(dbscan)

library(sf)
library(dplyr)
library(dbscan)
library(tidyr)

# 1. Perform clustering on sev_long_sf
sev_long_sf <- sev %>%
  st_as_sf(coords = c("Longitude", "Lattitude"), crs = st_crs(candlewood_lake))

dbscan_result <- dbscan(st_coordinates(sev_long_sf), eps = 0.0005, minPts = 1)

sev_long_sf <- sev_long_sf %>%
  mutate(cluster = as.factor(dbscan_result$cluster))

# 2. Aggregate species totals for each cluster using geometry
cluster_totals <- sev_long_sf %>%
  group_by(cluster) %>%
  summarize(
    geometry = st_centroid(st_union(geometry)),  # Create a centroid for each cluster
    across(PotFol:SpiPol, sum, na.rm = TRUE),    # Sum species data by cluster
    .groups = "drop"
  ) %>%
  mutate(total = rowSums(across(PotFol:SpiPol)))  # Calculate total species

# 3. Prepare data for pie charts
cluster_pies <- cluster_totals %>%
  st_drop_geometry() %>%
  pivot_longer(cols = PotFol:SpiPol, names_to = "species", values_to = "value") %>%
  mutate(
    fraction = value / total,                           # Proportion of each species
    start = cumsum(lag(fraction, default = 0)) * 2 * pi,  # Start angle in radians
    end = cumsum(fraction) * 2 * pi                     # End angle in radians
  ) %>%
  left_join(cluster_totals %>% select(cluster, geometry), by = "cluster")  # Add geometry back

# 4. Plot the pie charts
ggplot() +
  # Plot Candlewood Lake geometry
  geom_sf(data = candlewood_lake, fill = "lightblue", color = "black") +
  
  # Add pie charts at cluster centroids
  geom_arc_bar(
    data = cluster_pies,
    aes(
      x0 = st_coordinates(geometry)[, 1],  # Extract x from geometry
      y0 = st_coordinates(geometry)[, 2],  # Extract y from geometry
      r0 = 0, 
      r = sqrt(total) * 0.005,  # Scale radius by total species
      start = start, end = end, fill = species
    ),
    inherit.aes = FALSE
  ) +
  
  # Add labels and themes
  labs(
    title = "Candlewood Lake with Clustered Pie Charts",
    fill = "Species"
  ) +
  
  # Remove unused species from the legend
  scale_fill_discrete(drop = TRUE) +
  
  # Minimal theme
  theme_minimal() +
  
  # Remove axis labels and grid
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "right"
  ) +
  
  # Set plot limits
  coord_sf(
    xlim = c(-73.52, -73.40),  # Adjust longitude range for more space on the left
    ylim = c(41.43, 41.53)     # Keep the latitude range
  )

#########
# Exclude unclustered points
sev_long_sf <- sev_long_sf %>%
  filter(cluster != "-1")

# Aggregate species totals for each cluster
cluster_totals <- sev_long_sf %>%
  group_by(cluster) %>%
  summarize(
    geometry = st_centroid(st_union(geometry)),  # Create a centroid for each cluster
    across(PotFol:SpiPol, sum, na.rm = TRUE),    # Sum species data by cluster
    .groups = "drop"
  ) %>%
  mutate(total = rowSums(across(PotFol:SpiPol))) %>%
  filter(total > 0)  # Remove clusters with zero total species

# Prepare data for pie charts
cluster_pies <- cluster_totals %>%
  st_drop_geometry() %>%
  pivot_longer(cols = PotFol:SpiPol, names_to = "species", values_to = "value") %>%
  mutate(
    fraction = value / total,                           # Proportion of each species
    start = cumsum(lag(fraction, default = 0)) * 2 * pi,  # Start angle in radians
    end = cumsum(fraction) * 2 * pi                     # End angle in radians
  ) %>%
  filter(!is.na(fraction) & fraction > 0) %>%  # Remove invalid fractions
  left_join(cluster_totals %>% select(cluster, geometry), by = "cluster")

# Plot
sev_north <- ggplot() +
  # Plot Candlewood Lake geometry
  geom_sf(data = candlewood_lake, fill = "lightblue", color = "black") +
  
  # Add pie charts at cluster centroids
  geom_arc_bar(
    data = cluster_pies,
    aes(
      x0 = st_coordinates(geometry)[, 1],  # Extract x from geometry
      y0 = st_coordinates(geometry)[, 2],  # Extract y from geometry
      r0 = 0, 
      r = sqrt(total) * 0.0006,  # Scale radius by total species
      start = start, end = end, fill = species
    ),
    inherit.aes = FALSE
  ) +
  
  # Add labels and themes
  labs(
    title = "Candlewood Lake with Clustered Pie Charts",
    fill = "Species"
  ) +
  
  # Remove unused species from the legend
  scale_fill_discrete(drop = TRUE) +
  
  # Minimal theme
  theme_minimal() +
  
  # Remove axis labels and grid
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "right"
  ) +
  
  # Set plot limits
  coord_sf(
    xlim = c(-73.52, -73.40),  # Wide longitude range
    ylim = c(41.50, 41.57)     # Higher latitudes
  )


sev_south <- ggplot() +
  # Plot Candlewood Lake geometry
  geom_sf(data = candlewood_lake, fill = "lightblue", color = "black") +
  
  # Add pie charts at cluster centroids
  geom_arc_bar(
    data = cluster_pies %>%
      filter(
        st_coordinates(geometry)[, 1] >= -73.52 & st_coordinates(geometry)[, 1] <= -73.40,  # Longitude
        st_coordinates(geometry)[, 2] >= 41.43 & st_coordinates(geometry)[, 2] <= 41.48    # Latitude
      ),
    aes(
      x0 = st_coordinates(geometry)[, 1],  # Extract x from geometry
      y0 = st_coordinates(geometry)[, 2],  # Extract y from geometry
      r0 = 0, 
      r = sqrt(total) * 0.0006,  # Scale radius by total species
      start = start, end = end, fill = species
    ),
    inherit.aes = FALSE
  ) +
  
  # Add labels and themes
  labs(
    title = "Candlewood Lake with Clustered Pie Charts (South View)",
    fill = "Species"
  ) +
  
  # Remove unused species from the legend
  scale_fill_discrete(drop = TRUE) +
  
  # Minimal theme
  theme_minimal() +
  
  # Remove axis labels and grid
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "right"
  ) +
  
  # Set plot limits
  coord_sf(
    xlim = c(-73.52, -73.40),  # Wide longitude range
    ylim = c(41.42, 41.50)     # Lower latitudes
  )



plot(sev_south)
plot(sev_north)

sev_south <- sev_south +
  theme(legend.position = "none",
        plot.title = element_blank()
        )

combined_plot <- sev_north / sev_south

plot(combined_plot)

ggsave("2007_combined.png", combined_plot, width = 12, height = 15)
############
sev_total <- ggplot() +
  # Plot Candlewood Lake geometry
  geom_sf(data = candlewood_lake, fill = "lightblue", color = "black") +
  
  # Add pie charts at cluster centroids
  geom_arc_bar(
    data = cluster_pies %>%
      filter(
        st_coordinates(geometry)[, 1] >= -73.52 & st_coordinates(geometry)[, 1] <= -73.40,  # Longitude
        st_coordinates(geometry)[, 2] >= 41.43 & st_coordinates(geometry)[, 2] <= 41.48    # Latitude
      ),
    aes(
      x0 = st_coordinates(geometry)[, 1],  # Extract x from geometry
      y0 = st_coordinates(geometry)[, 2],  # Extract y from geometry
      r0 = 0, 
      r = sqrt(total) * 0.0006,  # Scale radius by total species
      start = start, end = end, fill = species
    ),
    inherit.aes = FALSE
  ) +
  
  # Add labels and themes
  labs(
    title = "Candlewood Lake with Clustered Pie Charts (South View)",
    fill = "Species"
  ) +
  
  # Remove unused species from the legend
  scale_fill_discrete(drop = TRUE) +
  
  # Minimal theme
  theme_minimal() +
  
  # Remove axis labels and grid
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "right"
  ) +
  
  # Set plot limits
  coord_sf(
    xlim = c(-73.52, -73.40),  # Wide longitude range
    ylim = c(41.42, 41.57)     # Lower latitudes
  )

print(sev_north)

ggsave("2007_total.png", sev_total, width = 12, height = 15)

##############
library(sf)
library(dplyr)
library(dbscan)
library(ggforce)

# 1. Perform clustering with a slightly larger eps
eps_value <- 0.0025  # Increase from the previous value
sev_long_sf <- sev %>%
  st_as_sf(coords = c("Longitude", "Lattitude"), crs = st_crs(candlewood_lake))

dbscan_result <- dbscan(st_coordinates(sev_long_sf), eps = eps_value, minPts = 3)

sev_long_sf <- sev_long_sf %>%
  mutate(cluster = as.factor(dbscan_result$cluster))

# 2. Aggregate species totals for each cluster
cluster_totals <- sev_long_sf %>%
  group_by(cluster) %>%
  summarize(
    geometry = st_centroid(st_union(geometry)),  # Create a centroid for each cluster
    across(PotFol:SpiPol, sum, na.rm = TRUE),    # Sum species data by cluster
    .groups = "drop"
  ) %>%
  mutate(total = rowSums(across(PotFol:SpiPol))) %>%
  filter(total > 0)  # Remove clusters with zero total species

# 3. Prepare data for pie charts
cluster_pies <- cluster_totals %>%
  st_drop_geometry() %>%
  pivot_longer(cols = PotFol:SpiPol, names_to = "species", values_to = "value") %>%
  mutate(
    fraction = value / total,                           # Proportion of each species
    start = cumsum(lag(fraction, default = 0)) * 2 * pi,  # Start angle in radians
    end = cumsum(fraction) * 2 * pi                     # End angle in radians
  ) %>%
  filter(!is.na(fraction) & fraction > 0) %>%  # Remove invalid fractions
  left_join(cluster_totals %>% select(cluster, geometry), by = "cluster")

# 4. Plot with larger pie charts
pie_chart_scale <- 0.001  # Larger scale factor for pie chart radius

sev_north <- ggplot() +
  geom_sf(data = candlewood_lake, fill = "lightblue", color = "black") +
  geom_arc_bar(
    data = cluster_pies,
    aes(
      x0 = st_coordinates(geometry)[, 1],
      y0 = st_coordinates(geometry)[, 2],
      r0 = 0,
      r = sqrt(total) * pie_chart_scale,  # Larger pie chart radius
      start = start, end = end, fill = species
    ),
    inherit.aes = FALSE
  ) +
  labs(title = "Candlewood Lake (North)", fill = "Species") +
  scale_fill_discrete(drop = TRUE) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "right"
  ) +
  coord_sf(xlim = c(-73.52, -73.40), ylim = c(41.50, 41.57))

sev_south <- ggplot() +
  geom_sf(data = candlewood_lake, fill = "lightblue", color = "black") +
  geom_arc_bar(
    data = cluster_pies %>%
      filter(
        st_coordinates(geometry)[, 1] >= -73.52 & st_coordinates(geometry)[, 1] <= -73.40,
        st_coordinates(geometry)[, 2] >= 41.43 & st_coordinates(geometry)[, 2] <= 41.48
      ),
    aes(
      x0 = st_coordinates(geometry)[, 1],
      y0 = st_coordinates(geometry)[, 2],
      r0 = 0,
      r = sqrt(total) * pie_chart_scale,  # Larger pie chart radius
      start = start, end = end, fill = species
    ),
    inherit.aes = FALSE
  ) +
  labs(title = "Candlewood Lake (South)", fill = "Species") +
  scale_fill_discrete(drop = TRUE) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "right"
  ) +
  coord_sf(xlim = c(-73.52, -73.40), ylim = c(41.43, 41.48))

plot(sev_south)
plot(sev_north)


########################
sev_total <- ggplot() +
  # Plot Candlewood Lake geometry
  geom_sf(data = candlewood_lake, fill = "lightblue", color = "black") +
  
  # Add pie charts at cluster centroids
  geom_arc_bar(
    data = cluster_pies,
    aes(
      x0 = st_coordinates(geometry)[, 1],  # Extract x from geometry
      y0 = st_coordinates(geometry)[, 2],  # Extract y from geometry
      r0 = 0, 
      r = sqrt(total) * 0.0006,  # Scale radius by total species
      start = start, end = end, fill = species
    ),
    inherit.aes = FALSE
  ) +
  
  # Add labels and themes
  labs(
    title = "Candlewood Lake 2007 Invasive Species Reports",
    fill = "Species"
  ) +
  
  # Remove unused species from the legend
  scale_fill_discrete(drop = TRUE) +
  
  # Minimal theme
  theme_minimal() +
  
  # Remove axis labels and grid
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "right"
  ) +
  
  # Set plot limits for full view
  coord_sf(
    xlim = c(-73.52, -73.40),  # Full longitude range
    ylim = c(41.42, 41.57)     # Full latitude range
  ) + 
  theme(
    plot.background = element_rect(fill = "white", color = NA),  # Set white background
    legend.key.size = unit(2, "cm"),                          # Increase legend key size
    legend.text = element_text(size = 14),                      # Increase legend text size
    legend.title = element_text(size = 16),                     # Increase legend title size
    plot.margin = margin(15, 10, 10, -50), # Shift plot to the left (adjust as needed)
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5)
  )


#contour line enhancement
#



ggsave("2007_total.png", sev_total, width = 12, height = 15, bg = "white")

unique(sev$CalSp)

unique(sev$MyrSpi)

