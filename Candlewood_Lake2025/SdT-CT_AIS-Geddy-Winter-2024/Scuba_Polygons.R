#preamble

rm(list=ls())
library(tidyverse)
library(tidycensus)
library(ggplot2)
library(gt) 
library(readr)
library(stringr)
library(dplyr)
library(scales)
library(patchwork)
library(cowplot)
library(viridis)
library(stargazer)
library(broom)
setwd("~/Dropbox/Scuba.Tech/SdT-CT_AIS-Geddy-Winter-2024")


pop <- read_csv("candlewoodagg_scuba.tech.csv")


test_plot <- ggplot() +
  geom_polygon(data = pop, aes(x = Longitude, y = Latitude, fill = `Depth (m)`)) + 
  geom_polygon(data = pop, aes(x = Longitude, y = Latitude, group = Transect), 
               fill = NA, color = "black", size = 0.1)

?geom_polygon

test_plot


 
  
   geom_polygon(data = counties, aes(x = long, y = lat, group = group), 
               fill = NA, color = "black", size = 0.1)
