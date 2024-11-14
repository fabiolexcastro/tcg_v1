


# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, fs, usdm, RColorBrewer, raster, spocc, sf, outliers, glue, CoordinateCleaner, tidyverse, rgbif, readxl, xlsx, openxlsx, rnaturalearthdata, rnaturalearth, geodata)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------

## Ghana 
gha0 <- gadm(country = 'GHA', level = 0, path = './tmpr') %>% st_as_sf()
gha1 <- gadm(country = 'GHA', level = 1, path = './tmpr') %>% st_as_sf()

## List the results 
dirs <- dir_ls('./rf/output', type = 'directory') %>% dir_ls() %>% dir_ls() %>% grep('/results', ., value = T) %>% as.character()

## Species 
spcs <- basename(as.character(dir_ls('./rData')))
lbls <- tibble(specie = c('Theobroma cacao', 'Anacardium occidentale', 'Cocos nucifera', 'Elaeis guineensis', 'Hevea brasiliensis', 'Vitellaria paradoxa', 'Mangifera indica'), common = c('Cocoa', 'Cashew', 'Coconut', 'Oil palm', 'Rubber', 'Shea', 'Mango'))

## Colors 
clrs <- read_csv('./tbl/Types clusters TCG Three Crops Ghana.csv', show_col_types = FALSE)

# Function to make the map  -----------------------------------------------

make.map <- function(spce){
  
  spce <- 'Cashew'
  
  
  ## Read the raster
  cat('To process: ', spce, '\n')
  rstr <- as.character(dir_ls(grep('v1', as.character(dir_ls(grep(spce, dirs, value = T))), value = T)))
  rstr <- terra::rast(rstr)
  
  ## Raster to table 
  tble <- terra::as.data.frame(rstr, xy = T) %>% as_tibble() 
  clor <- filter(clrs, Crop == spce)
  
  ## Join 
  tble <- full_join(tble, clor, by = c('final' = 'final'))
  tble <- mutate(tble, Type = factor(Type, levels = unique(clor$Type)))
  
  ## Colors 
  clrs <- unique(clor$Color)
  names(clrs) <- unique(clor$Type)

  ## To make the map
  g.map <- ggplot() +
    geom_tile(data = tble, aes(x = x, y = y, fill = Type)) +
    scale_fill_manual(values = clrs) +
    geom_sf(data = gha0, fill = NA, col = 'grey30') + 
    geom_sf(data = gha1, fill = NA, col = 'grey30') + 
    labs(x = 'Lon', y = 'Lat', fill = 'AEZ') +
    ggtitle(label = glue('{spce}')) +
    coord_sf() + 
    theme_minimal() +
    theme(
      strip.text = element_text(face = 'bold', hjust = 0.5),
      axis.text.x = element_text(size = 6), 
      axis.text.y = element_text(size = 6, angle = 90),
      plot.title = element_text(size = 12, face = 'bold', hjust = 0.5),
      legend.title = element_text(size = 9, face = 'bold'),
      axis.title = element_text(size = 7, face = 'bold')
    ) 
  
  out <- glue('./png/maps/run_1/aez')
  ggsave(plot = g.map, filename = glue('{out}/map_{spce}.jpg'), units = 'in', width = 5, height = 6, dpi = 300)
  
    
}








