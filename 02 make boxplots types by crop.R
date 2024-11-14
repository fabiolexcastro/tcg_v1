


## Fabio Alexander Castro Llanos 
## Alliance Bioversity - CIAT 
## November 12th 2024

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, fs, usdm, raster, RColorBrewer, factoextra, spocc, ggdendro, dendextend, sf, pvclust, outliers, glue, CoordinateCleaner, tidyverse, rgbif, readxl, xlsx, openxlsx, rnaturalearthdata, rnaturalearth, geodata)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------

dirs <- dir_ls('./rf/output', type = 'directory')
dirs <- as.character(dirs)

# Function make boxplot ---------------------------------------------------
make.boxp <- function(dir){
  
  dir <- dirs[1]
  
  ## To list the files
  cat('To start the process: ', dir, '\n')
  drs <- as.character(dir_ls(dir_ls(dir)))
  nme <- basename(dir)
  fls <- dir_ls(grep('results', drs, value = T))
  fls <- as.character(fls)
  
  ## Clustering 
  cls <- rast(grep('Clust', fls, value = T))
  cls.tbl <- terra::as.data.frame(cls, xy = T) %>% as_tibble() %>% setNames(c('x', 'y', 'class')) 
  cls.tbl <- inner_join(cls.tbl, tibble(class = 1:7, type = c('Unsuitable', 'Unsuitable', paste0('Type ', 1:5))))

  cll <- unique(cls.tbl$type)
  cll <- sort(cll)
  
  ## To read the table
  tbl <- read_csv(grep('pnts_prob-clust', fls, value = T), show_col_types = FALSE)
  tbl <- tbl %>% dplyr::select(pb, nombre, Longitude, Latitude, starts_with('bioc'), clst.pnts)
  tbl <- inner_join(tbl, tibble(clst.pnts = 1:7, cluster = c('Unsuitable', 'Unsuitable', paste0('Type ', 1:5))))
  
  tps <- sort(unique(tbl$clst.pnts))
  
  ## Tidy the table 
  vles <- tbl %>% gather(var, value, -c(pb,  nombre, Longitude, Latitude, clst.pnts, cluster)) 
  vars <- unique(vles$var)
  vles <- mutate(vles, var = factor(var, levels = vars))
  vles <- mutate(vles, clst.pnts = factor(clst.pnts, levels = tps))
  vles <- filter(vles, clst.pnts %in% tps)
  
  vles <- vles %>% filter(cluster %in% unique(grep('Type', tbl$cluster, value = T)))
  
  ## To make the boxplot
  gbox <- ggplot(data = vles, aes(x = cluster, y = value)) + 
    geom_boxplot() + 
    facet_wrap(.~var, scales = 'free_y') +
    labs(x = '', y = 'Value') +
    ggtitle(label = nme) +
    theme_minimal() + 
    theme(
      strip.text = element_text(face = 'bold', hjust = 0.5), 
      axis.text.x = element_text(angle = 90, hjust = 0.5), 
      plot.title = element_text(face = 'bold', hjust = 0.5),
      axis.text.y = element_text(angle = 90, hjust = 0.5)
    )
  
  ## To save the graph
  ggsave(plot = gbox, filename = glue('./png/graphs/boxplot/{nme}.jpg'), units = 'in', create.dir = TRUE, width = 9, height = 7, dpi = 300)
  cat('Done!\n')
  
  ## Raw map for Ghana
  gha1 <- gadm(country = 'GHA', level = 1, path = './tmpr')
  gha1 <- st_as_sf(gha1)
  
  clrs <- c('white', brewer.pal(n = 5, name = 'Set2'))
  names(clrs) <- c('Unsuitable', paste0('Type ', 1:5))
  
  g.cl <- ggplot() + 
    geom_tile(data = cls.tbl, aes(x = x, y = y, fill = type)) + 
    scale_fill_manual(values = clrs) +
    geom_sf(data = gha1, fill = NA, col = 'grey30') +
    coord_sf() + 
    labs(fill = '') +
    theme_void() + 
    theme(
      legend.position = 'bottom', 
      legend.text = element_text(size = 12)
    )
  
  ggsave(plot = g.cl, filename = glue('./png/maps/run_1/clust_raw/{nme}.jpg'), units = 'in', width = 4, height = 4, dpi = 300)
  

}








