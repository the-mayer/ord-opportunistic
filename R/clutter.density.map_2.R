clutter.density.map_2 <- function(){
  
  #Required Packages
  library(raster)
  library(rgdal)
  library(MASS)
  library(ggmap)
  library(geosphere)
  library(maptools)
  library(ggthemes)
  library(data.table)
  library(bit64)
  
  #Where are we doing the work  
  working.directory<-file.path(path.expand("~"),"ORD_Opportunistic")
  
  #Load Accipiter data (all of it)
  #ohare1
  #setwd(file.path(working.directory,"CSV_to_Process","ARTI","ohare1"))
  #ohare.1.filelist<-list.files(pattern="\\.csv$")
  #datalist<-lapply(ohare.1.filelist,fread)
  #ohare1<-rbindlist(datalist)
  #ohare1$Radar<-"ohare1"
  #assign("ohare.1.filelist",ohare.1.filelist,.GlobalEnv)
  #ohare2
  #setwd(file.path(working.directory,"CSV_to_Process","ARTI","ohare2"))
  #ohare.2.filelist<-list.files(pattern="\\.csv$")
  #datalist<-lapply(ohare.2.filelist,fread)
  #ohare2<-rbindlist(datalist)
  #ohare2$Radar<-"ohare2"
  #assign("ohare.2.filelist",ohare.2.filelist,.GlobalEnv)
  #ohare3
  #setwd(file.path(working.directory,"CSV_to_Process","ARTI","ohare3"))
  #ohare.3.filelist<-list.files(pattern="\\.csv$")
  #datalist<-lapply(ohare.3.filelist,fread)
  #ohare3<-rbindlist(datalist)
  #ohare3$Radar<-"ohare3"
  #assign("ohare.3.filelist",ohare.3.filelist,.GlobalEnv)
  #Cleanup a bit  
  #data<-rbind(ohare1,ohare2,ohare3)
  #rm(datalist,ohare1,ohare2,ohare3)
  #arti.header<-unlist(read.csv(ohare.3.filelist[1],header=F,nrows=1,stringsAsFactors = F))
  #setnames(data, c(arti.header),c("Update.Time", "Track.ID", "Start.Time..UTC.", "Latitude", "Longitude", "Speed..m.s.", "Heading..deg.N.", "Height...m.", "RCS..dBsm.", "Range.from.radar...m.", "Azimuth.from.radar..deg..", "Intensity", "UTM.Zone", "UTM.Northing", "UTM.Easting"))
  
  #Observation Data
  obs<-read.csv(file.path(working.directory,"CSV_to_Process","Observations", "observation.workup.csv"),header = T,stringsAsFactors = F)
  ord.1.obs<-subset(obs, Radar == "ohare1" | Radar == "both")
  ord.2.obs<-subset(obs, Radar == "ohare2")
  ord.3.obs<-subset(obs, Radar == "ohare3" | Radar == "both")
  
  #georectify ohare 1 clutter image
  ord.1.clutter.map<-raster(file.path(working.directory,"R_Geo_Clutter","ord1_Tracker_Display_16.31.21_Tue_24Apr2018.bmp"))
  xmin(ord.1.clutter.map) <- -88.024399
  xmax(ord.1.clutter.map) <- -87.843471
  ymin(ord.1.clutter.map) <-  41.925481
  ymax(ord.1.clutter.map) <-  42.059985
  crs(ord.1.clutter.map) <- "+proj=longlat +datum=WGS84"
  
  #georectify ohare 2 clutter image
  ord.2.clutter.map<-raster(file.path(working.directory,"R_Geo_Clutter","ord2_Tracker_Display_16.32.36_Tue_24Apr2018.bmp"))
  xmin(ord.2.clutter.map) <- -88.024399
  xmax(ord.2.clutter.map) <- -87.843471
  ymin(ord.2.clutter.map) <-  41.925481
  ymax(ord.2.clutter.map) <-  42.059985
  crs(ord.2.clutter.map) <- "+proj=longlat +datum=WGS84"
  
  #georectify ohare 3 clutter image
  ord.3.clutter.map<-raster(file.path(working.directory,"R_Geo_Clutter","ord3_Tracker_Display_16.33.11_Tue_24Apr2018.bmp"))
  xmin(ord.3.clutter.map) <- -88.024399
  xmax(ord.3.clutter.map) <- -87.843471
  ymin(ord.3.clutter.map) <-  41.925481
  ymax(ord.3.clutter.map) <-  42.059985
  crs(ord.3.clutter.map) <- "+proj=longlat +datum=WGS84"
  
  
  #subset data to clutter map extents
  #sub.data <- subset(data, Longitude >= -88.024399 & Longitude <= -87.843471)
  #sub.data <- subset(sub.data, Latitude >= 41.925481 & Latitude <= 42.059985)
  #rm(data)
  
  #Filter out extremely low clutter values
  sub.ord.1.clutter.map<-ord.1.clutter.map
  #sub.ord.1.clutter.map[sub.ord.1.clutter.map<=15]<-NA
  sub.ord.2.clutter.map<-ord.2.clutter.map
  #sub.ord.2.clutter.map[sub.ord.2.clutter.map<=15]<-NA
  sub.ord.3.clutter.map<-ord.3.clutter.map
  #sub.ord.3.clutter.map[sub.ord.3.clutter.map<=15]<-NA
  
  #Density calculation
  #density<-kde2d(x = sub.data$Longitude, y = sub.data$Latitude, n = c(128,128),lims = c(-122.365123,-122.266430,47.418647,47.485388))
  sub.ord.1.clutter.map <- aggregate(sub.ord.1.clutter.map, fact = 4) #For testing, consolidate clutter image
  sub.ord.2.clutter.map <- aggregate(sub.ord.2.clutter.map, fact = 4)
  sub.ord.3.clutter.map <- aggregate(sub.ord.3.clutter.map, fact = 4)
  
  #Convert clutter raster to polygons for use in ggmap
  ord.1.poly <- rasterToPolygons(sub.ord.1.clutter.map)
  ord.1.poly@data$id<-1:nrow(ord.1.poly@data)
  ord.1.polyFort <- fortify(ord.1.poly, data=ord.1.poly@data)
  ord.1.polyFortMer <- merge(ord.1.polyFort, ord.1.poly@data, by.x='id',by.y='id')
  
  ord.2.poly <- rasterToPolygons(sub.ord.2.clutter.map)
  ord.2.poly@data$id<-1:nrow(ord.2.poly@data)
  ord.2.polyFort <- fortify(ord.2.poly, data=ord.2.poly@data)
  ord.2.polyFortMer <- merge(ord.2.polyFort, ord.2.poly@data, by.x='id',by.y='id')
  
  ord.3.poly <- rasterToPolygons(sub.ord.3.clutter.map)
  ord.3.poly@data$id<-1:nrow(ord.3.poly@data)
  ord.3.polyFort <- fortify(ord.3.poly, data=ord.3.poly@data)
  ord.3.polyFortMer <- merge(ord.3.polyFort, ord.3.poly@data, by.x='id',by.y='id')
  
  #Get a map
  background<-get_map(location = c(-87.934003, 41.993222),zoom = 12,maptype = "satellite", color="bw")
  
  #ord1  
  #ord.1<-subset(sub.data, Radar == "ohare1")
  #p<-ggmap(background, extent = "normal", maprange = FALSE) %+% ord.1 + aes(x = Longitude, y = Latitude)
  p  <- ggmap(background)
  p1 <- p+geom_polygon(data = ord.1.polyFortMer,aes(x=long,y=lat,group=group,fill=ord1_Tracker_Display_16.31.21_Tue_24Apr2018),color=NA, alpha=0.7) + scale_fill_gradientn(colours = topo.colors(255))
  #p2 <- p1 + geom_density2d(data = ord.1, aes(x = Longitude, y = Latitude),n=c(100,100),size=0.4, na.rm=T) #Plot the density contours
  #p3 <- p2 + stat_density2d(data = ord.1, aes(x = Longitude, y = Latitude, fill = ..level.., alpha = ..level..),n=c(100,100), size = 0.01, bins = 16, geom = 'polygon',na.rm=T) #Add Transparent fill to density contours
  #p4 <- p3 + scale_fill_gradient(low = "yellow", high = "purple4") + scale_alpha(range = c(0.05, 0.20), guide = FALSE) #Color range and alpha levels to use for fill
  p5 <- p1 + geom_point(data = ord.1.obs, aes(x=bird.Longitude, y=bird.Latitude),color="dark red")
  p6 <- p5  + labs(title = "Chicago O'Hare International Airport",subtitle = "AR2-1 Clutter Map with Estimated Observed Bird Position", caption="AR2-1 Clutter Map: September 24, 2014\nEstimated Observed Bird Position within AR2-1 Beam (red)")
  p7 <- p6 + theme_map() + guides(fill=guide_legend(title="Clutter Level"))
  ggsave(filename = file.path(working.directory,"ohare.1.clutter.png"),plot = p7,width = 20,height = 20,units = "in")
  
  #ord2
  #ord.2<-subset(sub.data, Radar == "ohare2")
  #q<-ggmap(background, extent = "normal", maprange = FALSE) %+% ord.2 + aes(x = Longitude, y = Latitude)
  q  <- ggmap(background)
  q1 <- q+geom_polygon(data = ord.2.polyFortMer,aes(x=long,y=lat,group=group,fill=ord2_Tracker_Display_16.32.36_Tue_24Apr2018),color=NA, alpha=0.7) + scale_fill_gradientn(colours = topo.colors(255))
  #q2 <- q1 + geom_density2d(data = ord.2, aes(x = Longitude, y = Latitude),n=c(100,100),size=0.4, na.rm=T) #Plot the density contours
  #q3 <- q2 + stat_density2d(data = ord.2, aes(x = Longitude, y = Latitude, fill = ..level.., alpha = ..level..),n=c(100,100), size = 0.01, bins = 16, geom = 'polygon',na.rm=T) #Add Transparent fill to density contours
  #q4 <- q3 + scale_fill_gradient(low = "yellow", high = "purple4") + scale_alpha(range = c(0.05, 0.20), guide = FALSE) #Color range and alpha levels to use for fill
  q5 <- q1 + geom_point(data = ord.2.obs, aes(x=bird.Longitude, y=bird.Latitude),color="dark red")
  q6 <- q5  + labs(title = "Chicago O'Hare International Airport",subtitle = "AR2-2 Clutter Map with Estimated Observed Bird Position", caption="AR2-2 Clutter Map: September 24, 2014\nEstimated Observed Bird Position within AR2-2 Beam (red)")
  q7 <- q6 + theme_map() + guides(fill=guide_legend(title="Clutter Level"))
  ggsave(filename = file.path(working.directory,"ohare.2.clutter.png"),plot = q7,width = 20,height = 20,units = "in")
  
  #ord3
  #ord.3<-subset(sub.data, Radar == "ohare3")
  #r<-ggmap(background, extent = "normal", maprange = FALSE) %+% ord.3 + aes(x = Longitude, y = Latitude)
  r  <- ggmap(background)
  r1 <- r+geom_polygon(data = ord.3.polyFortMer,aes(x=long,y=lat,group=group,fill=ord3_Tracker_Display_16.33.11_Tue_24Apr2018),color=NA, alpha=0.7) + scale_fill_gradientn(colours = topo.colors(255))
  #r2 <- r1 + geom_density2d(data = ord.3, aes(x = Longitude, y = Latitude),n=c(100,100),size=0.4, na.rm=T) #Plot the density contours
  #r3 <- r2 + stat_density2d(data = ord.3, aes(x = Longitude, y = Latitude, fill = ..level.., alpha = ..level..),n=c(100,100), size = 0.01, bins = 16, geom = 'polygon',na.rm=T) #Add Transparent fill to density contours
  #r4 <- r3 + scale_fill_gradient(low = "yellow", high = "purple4") + scale_alpha(range = c(0.05, 0.20), guide = FALSE) #Color range and alpha levels to use for fill
  r5 <- r1 + geom_point(data = ord.3.obs, aes(x=bird.Longitude, y=bird.Latitude),color="dark red")
  r6 <- r5  + labs(title = "Chicago O'Hare International Airport",subtitle = "AR1 Clutter Map with Estimated Observed Bird Position", caption="AR1 Clutter Map: March 9, 2011\nEstimated Observed Bird Position within AR1 Beam (red)")
  r7 <- r6 + theme_map() + guides(fill=guide_legend(title="Clutter Level"))
  ggsave(filename = file.path(working.directory,"ohare.3.clutter.png"),plot = r7,width = 20,height = 20,units = "in")
  
  print("All done!")
}