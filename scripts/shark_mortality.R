## ---------------------------
## Script name: shark_mortality.R
## Author: Echelle Burns, emLab, UC Santa Barbara
## Date: 2024-07-19
## Purpose:
## Use the publicly available shark mortality data from Worm et al., (2024)
## to build maps of mean shark mortality in the FSM for purse seine and 
## longline vessels
## ---------------------------
## Notes:
## 
## ---------------------------

###
# Setup 
###

# Load libraries
library(tidyverse)
library(sf)
library(terra)
library(cowplot)

# Load source files
source(here::here("scripts", "setup.R"))

# Load data from Worm et al., 2024
coastal <- read.csv(file.path(sys_path, "emlab", "projects", "archived-projects", "finmap", "data", "Dryad", "coastal_shark_mortality_1x1.csv"))
rfmo <- read.csv(file.path(sys_path, "emlab", "projects", "archived-projects", "finmap", "data", "Dryad", "rfmo_shark_mortality_1x1.csv"))
high_seas <- read.csv(file.path(sys_path, "emlab", "projects", "archived-projects", "finmap", "data", "Dryad", "high_seas_shark_mortality_1x1.csv"))

# Update fsm crs to match better with the mortality data
fsm <- fsm %>% 
  st_transform(., crs = 4326)

# Rasterize
coastal_raster <- terra::rast(coastal %>% group_by(longitude, latitude) %>% 
                                summarize(mortality = sum(mortality)), 
                              type = "xyz", 
                              crs = "epsg:4326") 

rfmo_raster <- terra::rast(rfmo %>% group_by(longitude, latitude) %>% 
                             summarize(mortality = sum(mortality)), 
                           type = "xyz", 
                           crs = "epsg:4326") 

extent_rfmo <- ext(rfmo_raster)

high_seas_raster <- terra::rast(high_seas %>% group_by(longitude, latitude) %>% 
                                  summarize(mortality = sum(mortality)), 
                                type = "xyz", 
                                crs = "epsg:4326") 

# Looks like the rfmo raster is the most useful here...
coastal_raster %>% 
  crop(., fsm) %>% 
  mask(., fsm) %>% 
  plot()

rfmo_raster %>% 
  crop(., fsm) %>% 
  mask(., fsm) %>% 
  plot()

high_seas_raster %>% 
  crop(., fsm) %>% 
  mask(., fsm) %>% 
  plot()

# Create a function to plot relevant maps... 
plot_shark_mortality <- function(data, gear) { 
  # First, calculate the mean annual mortality by species
  result <- purrr::map(.x = data %>% 
                         filter(gear_class == gear) %>% 
                         distinct(scientific_name) %>% 
                         deframe(), 
                       .f = ~{ 
                         t <- data %>% 
                           filter(gear_class == gear & scientific_name == .x) %>% 
                           group_by(longitude, latitude, year) %>% 
                           summarise(mortality = sum(mortality, na.rm = TRUE)) %>% 
                           ungroup() %>% 
                           group_by(longitude, latitude) %>% 
                           summarise(mean_mortality = ceiling(mean(mortality, na.rm = T))) %>% 
                           ungroup() %>% 
                           terra::rast(., type = "xyz", crs = "epsg:4326", 
                                       extent = extent_rfmo) %>% 
                           crop(., st_buffer(fsm, 10)) 
                         
                         t[is.na(t)] <- 0
                         
                         t
                       })
  
  # Stack the results
  result <- terra::rast(result)
  
  # Update the names to include the common and scientific name (where applicable)
  names(result) <- data %>% 
    filter(gear_class == gear) %>% 
    distinct(common_name, scientific_name) %>% 
    mutate(common_name = ifelse(is.na(common_name), scientific_name, common_name), 
           full_name = paste0(common_name, ifelse(common_name != scientific_name, 
                                                  paste0("<br />(*", scientific_name, "*)"), 
                                                  "<br />"))) %>% 
    select(full_name) %>% 
    deframe()
  
  # Create dataframe for ggplotting; only keep species with some mortality estimates
  result_df <- as.data.frame(result, xy = TRUE) %>% 
    pivot_longer(-c(x, y)) %>% 
    group_by(name) %>% 
    mutate(colsums = sum(value)) %>% 
    ungroup() %>% 
    filter(colsums>0) %>% 
    select(-colsums)
  
  # Create a plot for each species
  plots <- purrr::map(.x = unique(result_df$name), 
                      .f = ~{ 
                        ggplot(data = result_df %>% filter(name == .x)) + 
                          geom_raster(mapping = aes(x = x, y = y, fill = value)) + 
                          geom_sf(data = fsm, fill = NA, color = "white") + 
                          facet_wrap(vars(name)) + 
                          scale_fill_viridis_c(name = NULL, limits = c(0, NA)) + 
                          coord_sf() + 
                          theme_void() + 
                          theme(strip.text = ggtext::element_markdown(), 
                                legend.position = "bottom", 
                                plot.margin = margin(10, 0, 0, 0))
                      })
  
  # Combine the plots and add a title
  plot_grid(plotlist = plots, ncol = 4, align = "v", axis = "l") %>% 
    ggdraw() +
    draw_plot_label(label = paste0("Mean shark mortality (individuals) for ", gear , " vessels in 2012-2019"), 
                    x = 0.5, y = 0.97, hjust = 0.5, vjust = 0, size = 14)
  
}

# Run for the purse seine and longline RFMO data
purse_seine <- plot_shark_mortality(data = rfmo, gear = "purse seine") 
longline <- plot_shark_mortality(data = rfmo, gear = "longline")

# Save outputs
# ggsave(file.path(project_figure_path, "shark_mortality_purse_seine.png"), purse_seine,
#        width = 10, height = 8, bg = "white", dpi = 600)
# ggsave(file.path(project_figure_path, "shark_mortality_longline.png"), longline,
#        width = 10, height = 8, bg = "white", dpi = 600)