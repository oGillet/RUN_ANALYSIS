#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
####                                R script                                     ####
####                               RUNTASTIC                                     ####
####                                STRAVA                                       ####
####                               TRACTIVE                                      ####
####                               14/12/2018                                    ####
####                             Olivier Gillet                                  ####
#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################

#####################################################################################
#####################################################################################
##                                 Librairies                                      ##
#####################################################################################
#####################################################################################

# Nettoyer l'environnement R
rm(list=ls())
gc()

# Chargement des librairies
libs <- c("rgdal","plotKML","rayshader","raster","XML","lubridate","sf","raster","ggmap", "gganimate","ggspatial","ggplot2","tidyverse","httr","scales","gridExtra","sp","rjson")
lapply(libs, require, character.only = TRUE)

# Definir les Systemes de Coordonnes de Reference
rgf93 <- "+proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
wgs84 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

#####################################################################################
#####################################################################################
##                                 Fonctions                                       ##
#####################################################################################
#####################################################################################

shift.vec <- function (vec, shift) {
  if(length(vec) <= abs(shift)) {
    rep(NA ,length(vec))
  }else{
    if (shift >= 0) {
      c(rep(NA, shift), vec[1:(length(vec)-shift)]) 
    } else {
      c(vec[(abs(shift)+1):length(vec)], rep(NA, abs(shift))) 
    } 
  } 
}

updatGPX <- function(gpxFile, f.ele=0.1, f.speed=0.1,f.hr=0.01, dog=F){
  run <- do.call("rbind", unlist(gpxFile$tracks, recursive=F))
  rownames(run) <- 1:nrow(run)
  if(dog){
    run <- run[,c(1:4)]
    colnames(run) <- c("lon","lat","ele","datetime")
  } else {
    colnames(run) <- c("lon","lat","ele","datetime", 'hr')
    run$hr <- as.character(run$hr)
    run$hr = substr(run$hr,1,nchar(run$hr)-2)
    for(i in 1:length(run$hr)){
      if(nchar(run$hr[i])>3){
        run$hr[i] = substr(run$hr[i],3,nchar(run$hr[i]))
      }
      if(nchar(run$hr[i])==4){
        run$hr[i] = substr(run$hr[i],1,nchar(run$hr)-1)
      }
    }
    run$hr <- as.numeric(run$hr)
  }
  run[,"datetime"] <- gsub("T", " ", run[,"datetime"]); run[,"datetime"] <- gsub(".000Z", " ", run[,"datetime"])
  run[,"datetime"] <- as.POSIXct(run$datetime,format="%Y-%m-%d %H:%M:%S", tz="GMT")
  run[,"datetime"] <- as.POSIXct(run$datetime,format="%Y-%m-%d %H:%M:%S", tz="Europe/Paris")
  attributes(run[,"datetime"])$tzone <- "Europe/Paris" 
  run[,"date"] <- as.Date(run$datetime)
  run[,"time"] <- format(run$datetime,"%H:%M:%S")
  run_shp <- run
  coordinates(run_shp) <- ~ lon + lat
  proj4string(run_shp) <- CRS(wgs84)
  run_shp <- spTransform(run_shp,CRS(rgf93))
  dist.to.prev <- sapply(seq_along(run_shp[-1, ]), function(i)
    spDistsN1(pts = run_shp[i, ], pt = run_shp[i+1, ], longlat = F))
  run_shp <- as.data.frame(run_shp)
  run[["lon"]] <- run_shp$lon
  run[["lat"]] <- run_shp$lat
  dist.to.prev <- c(dist.to.prev, NA)
  run$dist.to.prev <- dist.to.prev
  run$cumul.dist <- cumsum(run[,"dist.to.prev"])
  run$cumul.dist <- run$cumul.dist/1000
  run$ele <- as.numeric(run$ele)
  run$time.p1 <- shift.vec(run$datetime, -1)
  run$time.diff.to.prev <- as.numeric(difftime(run$time.p1, run$datetime))
  run$speed.m.per.sec <- run$dist.to.prev / run$time.diff.to.prev
  run$speed.km.per.h <- run$speed.m.per.sec * 3.6
  run$speed.km.per.h <- ifelse(is.na(run$speed.km.per.h), 0, run$speed.km.per.h)
  run$lowess.speed <- lowess(run$speed.km.per.h, f = f.speed)$y
  run$lowess.ele <- lowess(run$ele, f = f.ele)$y
  if(!dog){
    run$lowess.hr <- lowess(run$hr, f = f.hr)$y
  }
  print(head(run))
  return(run[-nrow(run),])
}


#####################################################################################
#####################################################################################
##                                 Script                                          ##
#####################################################################################
#####################################################################################

# Ouvrir les shapefiles
# route <- st_read("C:/Users/gilleol2/Desktop/TMP/ROUTES_METROPOLE_ROUEN_LINES_RGF93.shp")
# vegetation <- st_read("C:/Users/gilleol2/Desktop/TMP/VEGETATION_METROPOLE_ROUEN_POLYGONS_RGF93.shp")
# eau <- st_read("C:/Users/gilleol2/Desktop/TMP/HYDRO_METROPOLE_ROUEN_POLYGONS_RGF93.shp")

# Ouvrir les gpx
nameGPX_h <- "strava_20200411_0822_Course.gpx"; 
nameGPX_d <- "export.gpx"
gpx_h <- readGPX(paste0("C:\\Users\\gilleol2\\Downloads\\",nameGPX_h))
run_h <- updatGPX(gpxFile = gpx_h, f.ele = 0.1, f.hr = 0.01)
gpx_d <- readGPX(paste0("C:\\Users\\gilleol2\\Downloads\\",nameGPX_d))
run_d <- updatGPX(gpxFile = gpx_d, f.ele = 0.1, f.hr = 0.01, dog=T)
dateRun <- str_split(nameGPX_h,'_')[[1]][2]

# Joindre les deux dataframes
x <- merge(run_h, run_d, by='datetime')
run_h<- x[,c(1:16)]
colnames(run_h) <- gsub('.x', '', colnames(run_h))
run_d<-x[,c(17:29)]
colnames(run_d) <- gsub('.y', '', colnames(run_d))
x <- x[,c("lon.x","lat.x","lon.y","lat.y",'lowess.ele.x','lowess.hr',"cumul.dist.x","datetime")]
x$distance <- sqrt(((x[,"lon.x"]-x[,"lon.y"])^2)+((x[,"lat.x"]-x[,"lat.y"])^2)) 
x$lowess.distance <- lowess(x$distance, f = 0.005)$y
tmp <- rbind(data.frame(lon=x$lon.x,lat=x$lat.x, datetime=x$datetime, who=rep('Me',nrow(x))),
             data.frame(lon=x$lon.y,lat=x$lat.y, datetime=x$datetime, who=rep('My dog (Oslo)',nrow(x))))

# Convertir df en sp
run_d_sf = st_as_sf(run_d,coords = c("lon","lat"),crs=2154)
run_d_sf_4326 <- st_transform(run_d_sf,4326)
run_h_sf = st_as_sf(run_h, coords = c("lon", "lat"), crs=2154)
run_h_sf_4326 <- st_transform(run_d_sf,4326)

longextent <- c(min(st_coordinates(run_d_sf_4326)[,1]),max(st_coordinates(run_d_sf_4326)[,1]))
latextent <- c(min(st_coordinates(run_d_sf_4326)[,2]),max(st_coordinates(run_d_sf_4326)[,2]))

# Obtenir l'extension spatiale du shapefile
bboxMap <- st_bbox(run_d_sf_4326)

# Decouper les shapefiles
# vegetation <- st_crop(vegetation, c(xmin=min(run_d$lon-1000), xmax=max(run_d$lon+1000), ymin=min(run_d$lat-1000), ymax=max(run_d$lat+1000)))
# route <- st_crop(route, c(xmin=min(run_d$lon-1000), xmax=max(run_d$lon+1000), ymin=min(run_d$lat-1000), ymax=max(run_d$lat+1000)))

# 1ere carte
c1 <- ggplot() +
  annotation_scale(location = "bl", width_hint = 0.4) +
  coord_sf(xlim = c(min(run_d$lon), max(run_d$lon)), ylim = c(min(run_d$lat), max(run_d$lat)))+
  labs(title = paste0("Run, Rouen, ", as.Date(dateRun, format = '%Y%m%d')), 
       subtitle = "Strava, 2020 - Olivier Gillet")+ 
  theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", size = 0.5),
      panel.background = element_blank(),        
      legend.title=element_blank(),
      legend.position = 'bottom',
      legend.key=element_blank()) 

c1_a <- c1 +
  geom_point(data=tmp, aes(x = lon, y=lat, colour = who), size=2.5, show.legend = TRUE, alpha = 0.7) +
  transition_time(datetime) +
  scale_color_manual(values = c("red", "blue"))+
  shadow_wake(wake_length = 0.05, alpha = FALSE)+
  view_follow(fixed_y = TRUE)+
  enter_fade() + 
  exit_shrink() +
  ease_aes('sine-in-out')
a_gif <- animate(c1_a,duration=8, width = 400, height = 500)
# a_gif


tmp = gather(x[,c("lowess.ele.x","cumul.dist.x","datetime","lowess.distance")], Variables, Values, -c('cumul.dist.x','datetime'))

c1_b <- ggplot()+
  geom_line(data=tmp, aes(x=cumul.dist.x, y=Values, colour=Variables), size=0.8)  +
  annotate("rect", xmin = 0, xmax = max(x$cumul.dist.x), ymin = 0, ymax = 10, fill = "blue", alpha = .2)+
  annotate("text", x = 0.5, y = 11, size=3, label = "10 meters", fontface = 'bold')+
  # geom_hline(yintercept=10, size=1, linetype="dashed", color = "blue")+
  annotate("rect", xmin = 0, xmax = max(x$cumul.dist.x), ymin = 10, ymax = 20, fill = "green", alpha = .2)+
  annotate("text", x = 0.5, y = 21, size=3, label = "20 meters", fontface = 'bold')+
  # geom_hline(yintercept=20, size=1, linetype="dashed", color = "green")+
  annotate("rect", xmin = 0, xmax = max(x$cumul.dist.x), ymin = 20, ymax = 30, fill = "yellow", alpha = .2)+
  annotate("text", x = 0.5, y = 31, size=3, label = "30 meters", fontface = 'bold')+
  # geom_hline(yintercept=30, size=1, linetype="dashed", color = "yellow")+
  annotate("rect", xmin = 0, xmax = max(x$cumul.dist.x), ymin = 30, ymax = 40, fill = "orange", alpha = .2)+
  annotate("text", x = 0.5, y = 41, size=3, label = "40 meters", fontface = 'bold')+
  # geom_hline(yintercept=40, size=1, linetype="dashed", color = "orange")+
  annotate("rect", xmin = 0, xmax = max(x$cumul.dist.x), ymin = 40, ymax = 50, fill = "red", alpha = .2)+
  annotate("text", x = 0.5, y = 51, size=3,label = "50 meters", fontface = 'bold')+
  # geom_hline(yintercept=50, size=1, linetype="dashed", color = "red")+
  xlab("Kilometers") + 
  scale_y_continuous( "Distance between us (meters)", sec.axis = sec_axis(~ ., name = "Elevation (meters)"))+
  scale_color_discrete(labels = c("Distance between us", "Elevation"))+
  theme_light()+
  theme(legend.title=element_blank(),
        legend.position = 'bottom',
        legend.key=element_blank(),
        axis.title.y = element_text(size=10, face="bold"),
        axis.title.x = element_text(size=10, face="bold")) 

c1_b <-  c1_b + transition_reveal(datetime) + 
  enter_fade() +
  exit_shrink() +
  ease_aes('sine-in-out')
b_gif <- animate(c1_b,duration=8, width = 500, height = 500)

new_gif <- magick::image_append(c(a_gif[1], b_gif[1]))
for(i in 2:100){
  combined <- magick::image_append(c(a_gif[i], b_gif[i]))
  new_gif <- c(new_gif, combined)
}
anim_save("C:\\Users\\gilleol2\\CHART_2.gif",new_gif)



bbox <- data.frame(min = c(6.022108, 45.012030),
                   max = c(6.179801, 45.11066))
el_mat <- get_elevdata_from_bbox()
elmat_rayshade <- el_mat %>% unlabel_elevdata()
raw_data <- raster::getData("SRTM", lon = bbox[1, 1], lat = bbox[2, 2])
data_cropped <- raster::crop(raw_data, raster::extent(bbox))


bboxMap






























































































# Library
library(ggmap)
library(gridExtra)

map <- get_stamenmap(bbox = c(left = latextent[1], bottom = longextent[1], right = latextent[2], top = longextent[2]), 
                     maptype = "roadmap")
ggmap(map) + 
  theme_void() + 
  theme(
    plot.title = element_text(colour = "orange"), 
    panel.border = element_rect(colour = "grey", fill=NA, size=2)
  )



tmp <- sf::st_read("C:\\ESCAPE_VOLCANO\\DONNEES\\DONNEES_VECTORIELLES\\TRAITEES\\SAINT-CLAUDE\\ZONAGE_SAINT-CLAUDE_POLYGON_5490.shp")
tmp$zone <- c('2a','2b','3')
tmp <- tmp[,c("zone","geometry")]
tmp <- tmp[c(3,2,1),]
sf::st_write(tmp, "C:\\ZONAGE_(2)_SAINT-CLAUDE_POLYGON_5490.shp")



# Telecharger les donnees OSM
# library(osmdata)
# q <- bboxOsm %>%
#   opq() %>%
#   add_osm_feature(key = "highway")
# roadssf <- osmdata_sf(q)
# roadssf <- roadssf$osm_lines
# roadssf <- st_transform(roadssf$osm_lines, 4326)
# 
# q <- bboxOsm %>%
#   opq() %>%
#   add_osm_feature(key = "landuse", value = c('forest'))
# vegetationsf <- osmdata_sf(q)
# vegetationsf <- vegetationsf$osm_polygons
# 
# ggplot()+
#   geom_sf(roadssf[,c("osm_id","geometry")], colour="grey")

# Realiser la carte
c1 <- ggplot() +
  geom_sf(data = run_d, shape = 16, color = "blue") +
  geom_sf(data = run_h, shape = 16, color ='red') +
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.5, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  labs(title = paste0("Run, Normandie, ", as.Date(dateRun, format = '%Y%m%d')), 
       subtitle = "Strava, 2018 - Olivier Gillet")+ 
  xlab("Longitude") + ylab("Latitude") +
  theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", size = 0.5),
        panel.background = element_blank(),
        legend.title=element_blank(),
        legend.position = 'bottom',
        legend.key=element_blank()) 

c1 <- c1 + transition_time(datetime) +
  shadow_wake(wake_length = 0.1, alpha = FALSE)

c1_gif <- animate(c1, width = 240, height = 240)

anim_save("C:\\Users\\gilleol2\\CHART.gif",c1)


# Altitude
ggplot() +
  geom_line(data=run_h, aes(x=cumul.dist, y=ele),color="grey")+
  labs(title = paste0("Run, Normandie, ", as.Date(dateRun, format = '%Y%m%d')), 
       subtitle = "Strava, 2018 - Olivier Gillet", x = "Distance (Kilometers)", y = "Elevation (Meters)")+     
  theme_bw() + theme(
    panel.background = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none") 