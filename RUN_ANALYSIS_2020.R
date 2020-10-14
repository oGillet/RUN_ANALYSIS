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

# Fixer le repertoire courant
setwd('C:\\Users\\gilleol2\\Downloads\\ALL_GPX_RUN')
filesList <- list.files(pattern = '.gpx')

# Charger lespackages
libs <- c("rgdal","plotKML","rayshader","raster","XML","lubridate","sf","raster","ggmap", "gganimate","ggspatial","ggplot2","tidyverse","httr","scales","gridExtra","sp","rjson")
lapply(libs, require, character.only = TRUE)

# Definir deux SCRs
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

updatGPX <- function(gpxFile, f.ele=0.1, f.speed=0.1,f.hr=0.01, dog=F, printDf=T){
  run <- do.call("rbind", unlist(gpxFile$tracks, recursive=F))
  rownames(run) <- 1:nrow(run)
  hr <- ifelse(dog|length(colnames(run))==4,FALSE, TRUE)
  if(!hr){
    run <- run[,c(1:4)]
    colnames(run) <- c("lon","lat","ele","datetime")
    run$hr <- NA
  } else if ('extensions' %in% colnames(run)){ 
    colnames(run) <- c("lon","lat","ele","datetime", 'hr')    
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
  # run <- run[which(run$dist.to.prev < 50),]
  run$cumul.dist <- cumsum(run[,"dist.to.prev"])
  run$cumul.dist <- run$cumul.dist/1000
  run$ele <- as.numeric(run$ele)
  run$time.p1 <- shift.vec(run$datetime, -1)
  run$time.diff.to.prev <- as.numeric(difftime(run$time.p1, run$datetime))
  run$speed.m.per.sec <- run$dist.to.prev / run$time.diff.to.prev
  run$speed.km.per.h <- run$speed.m.per.sec * 3.6
  run$speed.km.per.h <- ifelse(is.na(run$speed.km.per.h), 0, run$speed.km.per.h)
  run <- run[is.finite(run$speed.km.per.h),]
  run <- run
  run$lowess.speed <- lowess(run$speed.km.per.h, f = f.speed)$y
  run$lowess.ele <- lowess(run$ele, f = f.ele)$y
  if(hr){run$lowess.hr <- lowess(run$hr, f = f.hr)$y}else{run$lowess.hr <- NA}
  if(printDf) print(head(run))
  return(run[-nrow(run),])
}

#####################################################################################
#####################################################################################
##                                 Script                                          ##
#####################################################################################
#####################################################################################

runDF <- data.frame()
for (f in 1:length(filesList)){
  nameGPX <- filesList[f]
  dateRun <- str_split(nameGPX,'_')[[1]][1]
  cat('++++',dateRun,'-',f,'/', length(filesList),'++++\n')
  gpx <- readGPX(nameGPX)
  run <- updatGPX(gpxFile = gpx, f.ele = 0.1, f.hr = 0.01, printDf=F)
  run$ID <- f
  runDF <- rbind(runDF, run)
}
save(runDF, file = 'ALL_GPX_RUN.RData')
# load('ALL_GPX_RUN.RData')

#####################################################################################
#####################################################################################
##                                 Strava                                          ##
#####################################################################################
#####################################################################################
# library(strava)
# data <- process_data('E:\\SPORT\\ALL_GPX_RUN', old_gpx_format = TRUE)
# p1 <- plot_facets(data)
# ggsave("facets001.png", p1, width = 20, height = 20, units = "cm")
# p2 <- plot_map(data, lon_min = 144.9, lon_max = 145.73, lat_min = -38.1, lat_max = -37.475)
# ggsave("map001.png", p2, width = 20, height = 15, units = "cm", dpi = 600))
# p3 <- plot_elevations(data)
# ggsave("elevations001.png", p3, width = 20, height = 20, units = "cm")

#####################################################################################
#####################################################################################
##                                 TrackeR                                         ##
#####################################################################################
#####################################################################################
library(trackeR)

runList <- list()
for (f in 1:length(filesList)){
  nameGPX <- filesList[f]
  dateRun <- str_split(nameGPX,'_')[[1]][1]
  dateRun <- as.Date(dateRun, format = "%Y%m%d")
  cat('++++',dateRun,'-',f,'/', length(filesList),'++++\n')
  runDF <- readGPX(file = filesList[f], speedunit = "km_per_h", distanceunit = "km")
  runTr0 <- trackeRdata(runDF, sport = "running")
  runList[[f]] <- runTr0
}
save(runList, file = 'ALL_GPX_RUN_LIST.RData')
load('ALL_GPX_RUN_LIST.RData')

tmpDf <- data.frame(date=NULL, elevation=NULL, distance=NULL, pace=NULL, duration=NULL, speed=NULL)
for (f in 1:length(runList)){
  tmp<- runList[[f]]
  tmp <- change_units(tmp,
                        variable = c("speed", "altitude"),
                        unit = c("km_per_h", "m"),
                        sport = c("running", "running"))
  runSummary <- summary(tmp, session = 1)
  dateRun <- as.Date(runSummary$sessionStart)
  cat('++++',dateRun,'-',f,'/', length(filesList),'++++\n')
  # if(year(dateRun)<2019) next()
  if(runSummary$avgPaceMoving>8) next()
  tmpDf <- rbind(tmpDf, data.frame(date=as.Date(runSummary$sessionStart),
                                   elevation=runSummary$total_elevation_gain, 
                                   distance=runSummary$distance,
                                   pace=runSummary$avgPaceMoving,
                                   duration=runSummary$duration,
                                   speed=runSummary$avgSpeedMoving))
}

tmpDf$lowess.elevation <- lowess(tmpDf$elevation, f = .1)$y
tmpDf$lowess.distance <- lowess(tmpDf$distance, f = .1)$y
tmpDf$lowess.pace <- lowess(tmpDf$pace, f = .1)$y
tmpDf$lowess.duration <- lowess(tmpDf$duration, f = .1)$y
tmpDf$lowess.speed <- lowess(tmpDf$speed, f = .1)$y

pE <- ggplot() + 
  geom_line(data=tmpDf, aes(x=date, y=lowess.elevation),color = "steelblue", size=.65) +
  labs(title = 'Elevation (m)',
       subtitle = paste0(min(tmpDf$date),' to ',max(tmpDf$date)),
       caption="",
       # caption="Source: strava, Runtastic, Suunto",
       y="Elevation (m)", x="")+
  annotate("rect", xmin = as.Date('2019-08-17'), xmax = max(tmpDf$date), ymin = min(tmpDf$lowess.elevation), ymax = max(tmpDf$lowess.elevation), fill = "grey50", alpha = .2)+
  annotate("text", x = as.Date('2019-09-30'), y = max(tmpDf$lowess.elevation), size=3,label = "Canicross", fontface = 'bold')+
  scale_x_date(labels=date_format("%d-%m-%y"), breaks="4 months") +
  theme_bw() + theme(axis.text.x = element_text(angle = 90, size=7, vjust=0.5),
                     panel.grid.minor = element_blank(),
                     plot.title=element_text(size=12, face="bold"),
                     legend.title=element_blank())
pE
ggsave("elevation.png", pE, width = 7, height = 6, dpi = 300, units = "in", device='png')


pDuration <- ggplot() + 
  geom_line(data=tmpDf, aes(x=date, y=lowess.duration),color = "steelblue", size=.65) +
  labs(title = 'Duration (min)',
       subtitle = paste0(min(tmpDf$date),' to ',max(tmpDf$date)),
       caption="Source: strava, Runtastic, Suunto",
       y="Duration (min)", x="")+
  annotate("rect", xmin = as.Date('2019-08-17'), xmax = max(tmpDf$date), ymin = min(tmpDf$lowess.duration), ymax = max(tmpDf$lowess.duration), fill = "grey50", alpha = .2)+
  annotate("text", x = as.Date('2019-09-30'), y = max(tmpDf$lowess.duration), size=3,label = "Canicross", fontface = 'bold')+
  scale_x_date(labels=date_format("%d-%m-%y"), breaks="4 months") +
  theme_bw() + theme(axis.text.x = element_text(angle = 90, size = 7, face = 'bold', vjust=0.5),
                     panel.grid.minor = element_blank(),
                     plot.title=element_text(size=12, face="bold"),
                     legend.title=element_blank())
pDuration
ggsave("duration.png", pDuration, width = 7, height = 6, dpi = 300, units = "in", device='png')

ps <- ggplot() + 
  geom_line(data=tmpDf, aes(x=date, y=lowess.speed),color = "steelblue", size=.65) +
  labs(title = 'Speed (km per h)',
       subtitle = paste0(min(tmpDf$date),' to ',max(tmpDf$date)),
       caption="",
       # caption="Source: strava, Runtastic, Suunto",
       y="Speed (km/h)", x="")+
  annotate("rect", xmin = as.Date('2019-08-17'), xmax = max(tmpDf$date), ymin = min(tmpDf$lowess.speed), ymax = max(tmpDf$lowess.speed), fill = "grey50", alpha = .2)+
  annotate("text", x = as.Date('2019-09-30'), y = max(tmpDf$lowess.speed), size=3,label = "Canicross", fontface = 'bold')+
  scale_x_date(labels=date_format("%d-%m-%y"), breaks="4 months") +
  theme_bw() + theme(axis.text.x = element_text(angle = 90, size = 7, face = 'bold', vjust=0.5),
                     panel.grid.minor = element_blank(),
                     plot.title=element_text(size=12, face="bold"),
                     legend.title=element_blank())
ps
ggsave("speed.png", ps, width = 7, height = 6, dpi = 300, units = "in", device='png')

pDistance <- ggplot() + 
  geom_line(data=tmpDf, aes(x=date, y=lowess.distance),color = "steelblue", size=.65) +
  labs(title = 'Distance (km)',
       subtitle = paste0(min(tmpDf$date),' to ',max(tmpDf$date)),
       caption="",
       # caption="Source: strava, Runtastic, Suunto",
       y="Distance (km)", x="")+
  annotate("rect", xmin = as.Date('2019-08-17'), xmax = max(tmpDf$date), ymin = min(tmpDf$lowess.distance), ymax = max(tmpDf$lowess.distance), fill = "grey50", alpha = .2)+
  annotate("text", x = as.Date('2019-09-30'), y = max(tmpDf$lowess.distance), size=3,label = "Canicross", fontface = 'bold')+
  scale_x_date(labels=date_format("%d-%m-%y"), breaks="4 months") +
  theme_bw() + theme(axis.text.x = element_text(angle = 90, size = 7, face = 'bold', vjust=0.5),
                     panel.grid.minor = element_blank(),
                     plot.title=element_text(size=12, face="bold"),
                     legend.title=element_blank())
pDistance
ggsave("distance.png", pDistance, width = 7, height = 6, dpi = 300, units = "in", device='png')

pDistance1 <- ggplot() + 
  geom_line(data=tmpDf[tmpDf$distance<20000,], aes(x=date, y=distance),color = "steelblue", size=.2) +
  geom_line(data=tmpDf, aes(x=date, y=lowess.distance),color = "red", size=.65) +
  labs(title = 'Distance (km)',
       subtitle = paste0(min(tmpDf$date),' to ',max(tmpDf$date)),
       caption="",
       # caption="Source: strava, Runtastic, Suunto",
       y="Distance (km)", x="")+
  annotate("rect", xmin = as.Date('2019-08-17'), xmax = max(tmpDf$date), ymin = min(tmpDf$distance), ymax = max(tmpDf[tmpDf$distance<20000,'distance']), fill = "grey50", alpha = .2)+
  annotate("text", x = as.Date('2019-09-30'), y = max(tmpDf[tmpDf$distance<20000,'distance']), size=3,label = "Canicross", fontface = 'bold')+
  scale_x_date(labels=date_format("%d-%m-%y"), breaks="4 months") +
  theme_bw() + theme(axis.text.x = element_text(angle = 90, size = 7, face = 'bold', vjust=0.5),
                     panel.grid.minor = element_blank(),
                     plot.title=element_text(size=12, face="bold"),
                     legend.title=element_blank())
pDistance1
ggsave("distance1.png", pDistance1, width = 7, height = 6, dpi = 300, units = "in", device='png')

pa <- ggplot() + 
  geom_line(data=tmpDf, aes(x=date, y=lowess.pace),color = "steelblue", size=.65) +
  labs(title = 'Average pace (min per km)',
       subtitle = paste0(min(tmpDf$date),' to ',max(tmpDf$date)),
       # caption="Source: strava, Runtastic, Suunto",
       caption="",
       y="Pace (min/km)", x="")+
  annotate("rect", xmin = as.Date('2019-08-17'), xmax = max(tmpDf$date), ymin = min(tmpDf$lowess.pace), ymax = max(tmpDf$lowess.pace), fill = "grey50", alpha = .2)+
  annotate("text", x = as.Date('2019-09-30'), y = max(tmpDf$lowess.pace), size=3,label = "Canicross", fontface = 'bold')+
  scale_x_date(labels=date_format("%d-%m-%y"), breaks="4 months") +
  theme_bw() + theme(axis.text.x = element_text(angle = 90, size = 7, face = 'bold', vjust=0.5),
                     panel.grid.minor = element_blank(),
                     plot.title=element_text(size=12, face="bold"),
                     legend.title=element_blank())
pa
ggsave("average.png", pa, width = 7, height = 6, dpi = 300, units = "in", device='png')


pa1 <- ggplot() + 
  geom_line(data=tmpDf, aes(x=date, y=pace),color = "steelblue", size=.2) +
  geom_line(data=tmpDf, aes(x=date, y=lowess.pace),color = "red", size=.65) +
  labs(title = 'Average pace (min per 1 km)',
       subtitle = paste0(min(tmpDf$date),' to ',max(tmpDf$date)),
       caption="",
       # caption="Source: strava, Runtastic, Suunto",
       y="Pace (min/km)", x="")+
  annotate("rect", xmin = as.Date('2019-08-17'), xmax = max(tmpDf$date), ymin = min(tmpDf$pace), ymax = max(tmpDf$pace), fill = "grey50", alpha = .2)+
  annotate("text", x = as.Date('2019-09-30'), y = max(tmpDf$pace), size=3,label = "Canicross", fontface = 'bold')+
  scale_x_date(labels=date_format("%d-%m-%y"), breaks="4 months") +
  theme_bw() + theme(axis.text.x = element_text(angle = 90, size = 7, face = 'bold', vjust=0.5),
                     panel.grid.minor = element_blank(),
                     plot.title=element_text(size=12, face="bold"),
                     legend.title=element_blank())
pa1
ggsave("average.png", pa1, width = 7, height = 6, dpi = 300, units = "in", device='png')

g <- grid.arrange(pa1, ps, pDistance1, pDuration, ncol=2, nrow = 2)
ggsave(file="stats.png", g, width = 9, height = 7, dpi = 300, units = "in", device='png')














x <- tmpDf[,c("date","distance")] %>% group_by(week = cut(date, "week")) %>% summarise(value = n())
x$week <- as.Date(x$week)

p <- ggplot(x,aes(x = week, y=value)) +
  geom_bar(stat="identity")+
  labs(title = '',
       caption="Source: strava, Runtastic, Suunto",
       y="Elevation", x="Date")+
  scale_x_date(labels=date_format("%Y%M"), breaks="4 months") +
  theme_bw() + theme(axis.text.x = element_text(angle = 90, vjust=0.5),
                     panel.grid.minor = element_blank(),
                     plot.title=element_text(size=12, face="bold"),
                     legend.title=element_blank())
p