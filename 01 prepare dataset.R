


## Fabio Alexander Castro Llanos 
## Alliance Bioversity - CIAT 
## November 12th 2024

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, fs, usdm, raster, RColorBrewer, factoextra, spocc, ggdendro, dendextend, sf, pvclust, outliers, glue, CoordinateCleaner, tidyverse, rgbif, readxl, xlsx, openxlsx, rnaturalearthdata, rnaturalearth, geodata)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

## Functions
source('FunctionsRFclustering.R')

# Load data ---------------------------------------------------------------

## Raster dataset 
bioc <- terra::rast('../v2_allCrops/tif/wc_5m/bioc-all_wrld.tif')

## Points dataset
pnts <- read_csv('../v2_allCrops/tbl/input/points/points rmve.csv')
pnts <- dplyr::select(pnts, pb:bioc_29)

## Labels
lbls <- tibble(specie = c('Theobroma cacao', 'Anacardium occidentale', 'Cocos nucifera', 'Elaeis guineensis', 'Hevea brasiliensis', 'Vitellaria paradoxa', 'Mangifera indica'), common = c('Cocoa', 'Cashew', 'Coconut', 'Oil palm', 'Rubber', 'Shea', 'Mango'))

# VIFs --------------------------------------------------------------------

## Function ----------
make.vifs <- function(tble, spce){
  
  # tble <- pnts
  # spce <- unique(pnts$nombre)[1]
  
  ## To filter the specie
  cat('To process: ', spce, '\n')
  pnt <- tble
  occ <- filter(pnt, pb == 1)
  occ <- filter(occ, nombre == spce)
  
  ## To make the VIF 
  vif <- usdm::vifstep(x = as.data.frame(occ[,5:32]), th = 10)
  vrs <- vif@results$Variables
  
  ## To select the variables 
  rsl <- dplyr::select(occ, pb:Latitude, all_of(vrs))
  
  ## To write the table
  out <- glue('./tbl/input/points')
  dir_create(out)
  nme <- lbls %>% filter(specie == spce) %>% pull(2)
  write.csv(rsl, glue('{out}/points_vifs_{nme}.csv'), row.names = FALSE)
  
  ## Finish
  cat('Done!\n')
  return(rsl)
  
}

## To apply ----------
spcs <- unique(tble$nombre)
pnts.vifs <- map(1:length(spcs), function(i){make.vifs(tble = pnts, spce = spcs[i])})

# Make clustering ---------------------------------------------------------

## Function ----------
make.clst.occr <- function(tble, nmbr){
  
  ## Proof 
  # tble <- pnts.vifs[[1]]
  # nmbr <- 3
  
  ## To start the process
  spce <- tble$nombre %>% unique()
  cat('To star the process: ', spce, '\n')
  nme <- lbls %>% filter(specie == spce) %>% pull(2)
  
  ## No Forest / No Trees
  no.forest <- 25
  no.trees <- 100
  nVars <- 8
  
  ## To remove the NAs 
  tble <- drop_na(tble)
  
  ## Random forest distance
  occ <- tble
  occ.mtx <- occ[,5:ncol(occ)]
  occ.dst <- RFdist(occ.mtx, mtry1 = nVars, no.trees, no.forest, addcl1 = T, addcl2 = F, imp = T, oob.prox1 = T)
  
  ## Clustering
  occ.cls <- hclust(as.dist(occ.dst$cl1), method = 'ward.D2')
  
  ## Drawing the dendogram
  ddd <- fviz_dend(occ.cls, k = nmbr, k_color = brewer.pal(n = nmbr, name = 'Dark2'), cex = 0.5, main = glue('Dendogram {nme}'), xlab = 'Objects', ylab = 'Distance')
  ggsave(plot = ddd, filename = glue('./png/graphs/dendogram/dendo_{nme}_{nmbr}.jpg'), units = 'in', width = 9, height = 7, dpi = 300, create.dir = TRUE)
  
  ## Adjust the clustering
  occ.ncl <- nmbr
  occ.lrf <- pamNew(occ.dst$cl1, occ.ncl)
  occ.cld <- cbind(pb = as.factor(occ.lrf), occ[2:ncol(occ)])
  # occ <- dplyr::select(occ, -cluster)
  occ.clp <- cbind(occ, cluster = occ.lrf) %>% na.omit() %>% as_tibble()

  ## To save the results
  dir <- glue('./rData/{nme}/run_1')
  dir_create(dir)
  save(occ.mtx, file = glue('{dir}/datRF.rData'))
  save(occ.cls, file = glue('{dir}/clusterdata.rData'))
  save(occ, occ.clp, occ.ncl, occ.lrf, file = glue('{dir}/clustereddata.rData'))
  save(occ.cld, file = glue('{dir}/occ_cld.rData'))
  
  cat('Done!\n')
  # return(occ.clp)
  
}

## To apply ----------
map2(.x = pnts.vifs, .y = c(3, 3, 3, 4, 5, 6, 5), .f = make.clst.occr)
# Mapping clustering points -----------------------------------------------

##
make.maps.pnts.clst <- function(spce){
  
  # spce <- 'Cashew'
  
  ## To add the columns
  fles <- dir_ls(glue('./rData/{spce}/run_1'), regexp = '.rData')
  load(as.character(grep('clustereddata.rData', fles, value = T)))
  
  ## Check the table
  tble <- occ.clp
  tble <- mutate(tble, cluster = paste0('Type ', cluster))
  maxn <- unique(tble$cluster) %>% parse_number()
  tble <- mutate(tble, cluster = factor(cluster, levels = paste0('Type ', maxn)))
  
  ## Vector data 
  wrld <- ne_countries(returnclass = 'sf', scale = 50)
  
  ## To make the map
  gocc <- ggplot() + 
    geom_sf(data = wrld, fill = NA, col = 'grey30') + 
    geom_point(data = tble, aes(x = Longitude, y = Latitude, col = cluster), size = 0.3) +
    scale_color_manual(values = brewer.pal(n = max(maxn), name = 'Set2')) +
    coord_sf() + 
    labs(col = '') +
    ggtitle(label = spce) +
    theme_void() + 
    theme(
      plot.title = element_text(face = 'bold', hjust = 0.5), 
      legend.position = 'bottom'
    ) +
    guides(colour = guide_legend(override.aes = list(size=10)))
  
  ## To save the map
  ggsave(plot = gocc, filename = glue('./png/maps/run_1/{spce}.jpg'), units = 'in', width = 9, height = 7, dpi = 300, create.dir = TRUE)
  return(gocc)
  
}

##
map(.x = lbls$common, .f = make.maps.pnts.clst)

# Random forest predict ---------------------------------------------------

##
make.rfrs.mdel <- function(crop){
  
  # Proof
  # crop <- 'Cashew'
  
  ## To start the process
  cat('To start the process: ', crop, '\n')
  spce <- filter(lbls, common == crop) %>% pull(2)
  nme <- crop
  
  ## No Forest / No Trees
  no.forest <- 25
  no.trees <- 100
  nVars <- 8
  
  ## To list the results 
  fles <- dir_ls(glue('./rData/{spce}/run_1'))
  load(grep('clustereddata.rData', fles, value = T))
  
  tble <- occ.clp
  
  ## To generate the background
  mskr <- bioc[[1]] * 0 + 1
  mask <- mskr
  cll.occ <- terra::extract(mskr, tble[,c('Longitude', 'Latitude')], cell = T)[,3]
  mskr[cll.occ] <- NA
  mskr.tble <- terra::as.data.frame(mskr, xy = T) %>% as_tibble()
  bck <- sample_n(tbl = mskr.tble, size = nrow(tble), replace = FALSE)[,-3]
  bck <- as_tibble(cbind(bck, terra::extract(bioc, bck[,1:2])))
  bck <- mutate(bck, pb = 0)
  bck <- rename(bck, Longitude = x, Latitude = y)
  bck <- dplyr::select(bck, -ID)
  bck <- mutate(bck, nombre = crop)
  
  ## Select the variables remained from the VIF 
  bck <- dplyr::select(bck, Longitude, Latitude, pb, nombre, tble %>% dplyr::select(starts_with('bioc')) %>% colnames())
  
  colnames(bck)
  colnames(occ)
  
  ## Clustering pseudo-absences 
  bck.mtx <- bck[,5:ncol(bck)]
  bck.mtx <- drop_na(bck.mtx)
  bck.dst <- RFdist(bck.mtx, mtry1 = nVars, no.trees, no.forest, addcl1 = T, addcl2 = F, imp = T, oob.prox1 = T)
  bck.ncl <- 2
  bck.lrf <- pamNew(bck.dst$cl1, bck.ncl)
  bck.cls <- hclust(as.dist(bck.dst$cl1), method = 'ward.D2')
  bck <- bck %>% dplyr::select(-pb)
  bck.cld <- cbind(pb = as.factor(bck.lrf), drop_na(bck[1:ncol(bck)]))
  bck.clp <- cbind(drop_na(bck), cluster = bck.lrf) %>% na.omit() %>% as_tibble()
  
  ## Read the results of presences 
  fls.occ <- as.character(dir_ls(glue('./rData/{nme}/run_1'), regexp = '.rData'))
  load(grep('/occ_cld', fls.occ, value = T)) # load(grep('/occ_cls', fls.occ, value = T))
  load(grep('/clustereddata.rData', fls.occ, value = T))
  
  clusteredpresdata <- occ.clp
  no.absenceclasses <- 2
  
  presvalue_swd  <- clusteredpresdata[,3:ncol(clusteredpresdata)] %>% 
    cbind(pb = (clusteredpresdata$cluster + no.absenceclasses), .) %>%
    na.omit() %>% 
    as.data.frame() %>%
    mutate(cluster = cluster + no.absenceclasses)
  presvalue_swd <- dplyr::select(presvalue_swd, pb, starts_with('bioc'))
  presvalue_swd <- mutate(presvalue_swd, pb = as.factor(pb))
  classdata <- bck.cld
  
  classdata_2 <- cbind(pb = as.data.frame(classdata)$pb, classdata[,2:ncol(classdata)]) # Background
  dim(classdata_2); dim(presvalue_swd)
  classdata_2 <- dplyr::select(classdata_2, -Longitude, -Latitude, -nombre)
  
  allclasses_swd <- rbind(classdata_2, presvalue_swd[,1:ncol(classdata_2)])
  unique(allclasses_swd$pb)
  
  ## To save the results
  dir <- glue('./rData/{nme}/run_1'); dir_create(dir)
  save(bck.mtx, file = glue('{dir}/back_datRF.rData'))
  save(bck.cls, file = glue('{dir}/back_clusterdata.rData'))
  save(bck, bck.clp, bck.ncl, bck.lrf, file = glue('{dir}/back_clustereddata.rData'))
  save(allclasses_swd, file = glue('{dir}/allclasses_swd.rData'))
  
  ## To make the random forest
  
  # To make the random forest analysis --------------------------------------
  vrs <- colnames(allclasses_swd)[2:ncol(allclasses_swd)]
  model1 <- as.formula(paste('factor(pb) ~', paste(paste(vrs), collapse = '+', sep =' ')))
  rflist <- vector('list', 50) 
  auc <- vector('list', 50)
  NumberOfClusters <- max(as.character(allclasses_swd$pb)) %>% as.numeric()
  NumberOfClusters <- NumberOfClusters - 2
  
  clusteredpresdata
  allclasses_swd
  
  samplesize <- round(min(summary(as.factor(allclasses_swd$pb))) / 2, 0) 
  
  for(repe in 1:50){ # 50 bosques
    
    # library(pROC)
    
    print(repe)
    pressample <- list()
    
    for (i in 1:(NumberOfClusters+no.absenceclasses)){
      
      if(any(i==c(1:no.absenceclasses))) { 
        
        rows <- sample(rownames(allclasses_swd[allclasses_swd$pb==i,]), 
                       size = samplesize*NumberOfClusters/2/no.absenceclasses)
      } else {
        rows <- sample(rownames(allclasses_swd[allclasses_swd$pb==i,]), size=samplesize)
      }
      pressample[[i]] <- allclasses_swd[rows,] 
    }
    
    species <- na.omit(do.call(rbind, pressample)) 
    head(species)
    Samplesplit <- sample(rownames(species)) 
    
    envtrain <- species[Samplesplit[1:(0.8*nrow(species))],] 
    envtest <- species[Samplesplit[(0.8*nrow(species)):nrow(species)],] 
    
    rfmodel <- randomForest(model1, data = envtrain, ntree = 500, na.action = na.omit, nodesize = 2) 
    
    dir_create(glue('./rf/output/{nme}/run_1/models'))
    save(rfmodel, file = glue('./rf/output/{nme}/run_1/models/RF_', NumberOfClusters, 'Prob_' , 'rep_' ,repe, '.rdata' ,sep=''))
    rflist[[repe]] <- rfmodel
    
    # AUC 
    predicted <- as.numeric(predict(rfmodel, envtest))
    observed <- as.vector(envtest[,'pb'])
    auc[[repe]] <- auc(observed, predicted) 
    rm(rfmodel)
    
    cat(auc[[repe]] ,'\n')
    
  }
  
  allclasses_swd
  
  ## Auc analys
  auc <- unlist(auc)
  
  ## To make the some graphs
  gauc <- ggplot(data = tibble(value = auc), aes(y = auc)) + 
    geom_boxplot() + 
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2), labels = seq(0, 1, 0.2)) +
    labs(y = 'AUC') +
    ggtitle(label = 'AUC for the random forest model') +
    theme_minimal() + 
    theme(
      axis.text.y = element_text(angle = 90, hjust = 0.5, size = 12), 
      axis.title = element_text(face = 'bold', size = 16), 
      axis.text.x = element_blank(), 
      plot.title = element_text(face = 'bold')
    )
  
  ggsave(plot = gauc, filename = glue('./png/graphs/run_1/{nme}_boxplot_auc.jpg'), units = 'in', width = 6, height = 6, dpi = 300, create.dir = TRUE)
  
  ## Random forest reducing
  rff <- do.call(randomForest::combine, rflist)
  importance <- as.data.frame(rff$importance)
  
  dir.out <- glue('./rData/{nme}/run_1')
  dir_create(dir.out)
  
  save(allclasses_swd, file = paste0(dir.out, '/allclasses_swd.rData'))
  save(rflist, file = paste(dir.out, '/rflist_', NumberOfClusters, '.rdata', sep = ''))
  save(importance, file = paste0(dir.out, '/importanceRF.rData'))
  save(auc, file = paste0(dir.out, '/aucRF_dist.rData'))
  save(rff, file = paste0(dir.out, '/rff_dist.rData'))
  
  ## To save
  save(occ.cld, file = paste0(dir.out, '/', 'occ_cld.rData'))
  save(occ.clp, file = paste0(dir.out, '/', 'occ_clp.rData'))
  # save(occ.cls, file = paste0(dir.out, '/', 'occ_cls.rData'))
  save(occ, file = paste0(dir.out, '/', 'occ.rData'))
  
  # To extract by mask 
  limt <- gadm(country = 'GHA', level = 0, path = './tmpr')
  lyr <- rast('../v2_allCrops/tif/wc_5m/bioc-all_wrld.tif')
  lyr <- lyr[[grep(paste0(vrs, collapse = '|'), names(lyr))]]
  lyr <- terra::crop(lyr, limt) %>% terra::mask(., limt)
  
  # Predict modell
  climatevalues <- as.data.frame(lyr, xy = T, na.rm = F)
  NumberOfClusters 
  
  ## To make the predict for the matrix
  rasterProbs <- predict(rff, climatevalues[,3:ncol(climatevalues)], type = 'prob') # proximity = T
  
  # Map --------------------------------------------------------------------
  tbl.prob <- rasterProbs
  tbl.prob <- as_tibble(tbl.prob) %>% setNames(c('Unsuit_1', 'Unsuit_2', paste0('Type_', 1:5)))
  library(hablar)
  tbl.prob <- retype(tbl.prob)
  tbl.prob <- cbind(climatevalues[,1:2], tbl.prob)
  tbl.prob <- tbl.prob %>% drop_na()
  tbl.prob <- as_tibble(tbl.prob)
  rst.prob <- rast(tbl.prob, type = 'xyz')
  dfr <- tbl.prob %>% mutate(gid = 1:nrow(.)) %>% gather(var, value, -c(gid, x, y))
  dfr <- mutate(dfr, var = factor(var, levels = c('Unsuit_1', 'Unsuit_2', paste0('Type_', 1:5))))
  
  ## To make the map 
  gtps <- ggplot() + 
    geom_tile(data = dfr, aes(x = x, y = y, fill = value)) +
    facet_wrap(~var, ncol = 4, nrow = 2) +
    scale_fill_gradientn(colors = brewer.pal(n =  9, name = 'RdYlGn')) +
    geom_sf(data = st_as_sf(limt), fill = NA, col = 'grey30') +
    coord_sf() +
    labs(fill = 'Prob\nvalue') +
    theme_void() + 
    theme(
      legend.position = 'bottom', 
      legend.key.width = unit(3, 'line')
    )
  
  gtps
  ggsave(plot = gtps, filename = glue('./png/maps/run_1/types_probs_{nme}.jpg'), units = 'in', width = 9, height = 6.5, dpi = 300, create.dir = TRUE)
  
  ## Raster building
  rasterProbs_na <- na.omit(rasterProbs)
  sum_rasterProbs_na <- apply(rasterProbs_na, 1, sum)
  
  rasterRF <- rowSums(rasterProbs[,c(3:(NumberOfClusters+2))])
  uncertainty <- apply(rasterProbs, 1, max)  
  
  rasterRFprob <- lyr[[1]]
  values(rasterRFprob) <- rasterRF 
  
  rasterRFuncertainty <- lyr[[1]]
  values(rasterRFuncertainty) <- uncertainty 
  
  rasterRF <- max.col(rasterProbs, 'first')
  rasterRFclass <- lyr[[1]]
  values(rasterRFclass) <- rasterRF
  
  ## To write the raster
  plot(rasterRFclass)
  dir.out <- glue('./rf/output/{nme}/run_1/results')
  dir_create(dir.out)
  terra::writeRaster(rasterRFclass, paste0(dir.out, '/RF_5Clust_current.tif'), overwrite = T)
  terra::writeRaster(rasterRFprob, paste0(dir.out, '/RF_5Prob_current.tif'), overwrite = T)
  terra::writeRaster(rasterRFuncertainty, paste0(dir.out, '/RF_5Unc_current.tif'), overwrite = T)
  cat('Done!\n')
  
  ## Mapping clustered points 
  
  ## For the points
  pnts.df <- tble %>% dplyr::select(starts_with('bioc'))
  rasterProbs_points <- predict(rff, pnts.df, type = 'prob')
  prob.pnts <- rowSums(rasterProbs_points[,3:(NumberOfClusters+2)])
  clst.pnts <- max.col(rasterProbs_points, 'first')
  uncr.pnts <- apply(rasterProbs_points, 1, max)
  prob.pnts <- cbind(prob.pnts, uncr.pnts, clst.pnts)
  prob.pnts <- as.data.frame(prob.pnts)
  rslt <- cbind(tble, prob.pnts)
  rslt <- as_tibble(rslt)
  rslt <- as_tibble(rslt)
  
  write.csv(rslt, paste0(dir.out, '/pnts_prob-clust.csv'), row.names = FALSE)
  
  # read_csv('./rf/output/Cocoa/run_1/results/pnts_prob-clust.csv')
  
  ## To make the map 
  rslt <- rslt %>% mutate(clase = clst.pnts - 2, clase = paste0('Type ', clase)) %>% dplyr::select(clase, everything())
  rslt <- mutate(rslt, clase = factor(clase, levels = c('Type 1', 'Type 2', 'Type 3', 'Type 4', 'Type 5')))
  wrld <- ne_countries(returnclass = 'sf', scale = 50)
  
  gpnt.cls <- ggplot() + 
    geom_point(data = rslt %>% drop_na(), aes(x = Longitude, y = Latitude, col = clase), size = 0.5) +
    geom_sf(data = wrld, fill = NA, col = 'grey30') +
    labs(col = '') +
    theme_void() + 
    theme(
      legend.position = 'bottom'
    ) + 
    guides(
      colour = guide_legend(override.aes = list(size=10))
    )
  
  ## Final and to sdave the map
  ggsave(gpnt.cls, filename = glue('./png/maps/points/run_1/{nme}_points.jpg'), units = 'in', width = 9, height = 6, dpi = 300, create.dir = TRUE)
  cat('Finish!\n')
  
}

## 
nmes.crps <- lbls$common
make.rfrs.mdel('Coconut')
make.rfrs.mdel('Oil palm')
make.rfrs.mdel('Cocoa')
map(.x = 1:length(clst.spcs), .f = function(i){make.rfrs.mdel(tble = clst.spcs[[i]], crop = crps[i])})


