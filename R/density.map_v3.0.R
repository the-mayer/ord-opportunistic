density.map_v3.0<-function(ULC, LRC,title="Your Title Here", n=c(100,100), p=c(5),local.time.zone,obs.csv){
  
  #Some reference for function inputs
  # ULC = c(-122.3264, 47.4711), LRC = c(-122.2975, 47.4228),title="SEA_Fil+0.5"
  # ULC = c(-122.3342, 47.4756), LRC = c(-122.2904, 47.4186),title="SEA_Fil+1.0"
  # n = c(100,100),p = c(3,4,5,8,10,15)
  # Modified for ORD - OHARE IS UTM ZONE 16T
  #FOR USE WITH ORD OPPORTUNISTIC OBSERVATIONS
  
  #Add some filtering rules for input raw dataset, and then to generate the density maps
  
  #Check that the required packages are installed.
  if("data.table" %in% rownames(installed.packages()) == FALSE) {install.packages("data.table")}
  if("bit64" %in% rownames(installed.packages()) == FALSE) {install.packages("bit64")}
  if("ggmap" %in% rownames(installed.packages()) == FALSE) {install.packages("ggmap")}
  if("ggthemes" %in% rownames(installed.packages()) == FALSE) {install.packages("ggthemes")}
  if("geosphere" %in% rownames(installed.packages()) == FALSE) {install.packages("geosphere")}
  if("ggsn" %in% rownames(installed.packages()) == FALSE) {install.packages("ggsn")}
  if("grid" %in% rownames(installed.packages()) == FALSE) {install.packages("grid")}
  if("circular" %in% rownames(installed.packages()) == FALSE) {install.packages("circular")}
  if("scales" %in% rownames(installed.packages()) == FALSE) {install.packages("scales")}
  if("MASS" %in% rownames(installed.packages()) == FALSE) {install.packages("MASS")}
  if("reshape2" %in% rownames(installed.packages()) == FALSE) {install.packages("reshape2")}
  if("sp" %in% rownames(installed.packages()) == FALSE) {install.packages("sp")}
  if("maptools" %in% rownames(installed.packages()) == FALSE) {install.packages("maptools")}
  if("rgeos" %in% rownames(installed.packages()) == FALSE) {install.packages("rgeos")}
  if("rgdal" %in% rownames(installed.packages()) == FALSE) {install.packages("rgdal")}
  if("plotKML" %in% rownames(installed.packages()) == FALSE) {install.packages("plotKML")}
  
  #Required Packages
  require(data.table) 
  require(bit64) 
  require(ggmap) 
  require(ggthemes)
  require(geosphere)
  require(ggsn)
  require(grid)
  require(circular)
  require(scales)
  require(MASS)
  require(reshape2)
  require(sp)
  require(maptools)
  require(rgeos)
  require(rgdal)
  require(plotKML)
  
  #Define some variables.
  working.directory<-file.path(path.expand("~"),"ORD_Opportunistic")
  
  #Load observation data
  setwd(file.path(working.directory,"CSV_to_Process","Observations"))  
  obs.raw<-read.csv(obs.csv,header=T)
  #Remove incomplete observations
  obs<-na.omit(obs.raw)
  obs.incomplete<-obs.raw[!complete.cases(obs.raw),] #Document which ones were removed.
  
  #Make R understand observation data
  obs$Bearing<-circular(obs$Bearing,type="angles",units="degrees",template="none",modulo="2pi",rotation="clock") ##Bearings are circular data
  obs$Heading<-as.character(obs$Heading)
  obs$Comments<-as.character(obs$Comments) ##Comments are characters, not factors
  obs$Start.Date.Time.GMT<- as.POSIXct(obs$Start.Date.Time.GMT,tz="GMT") ##Time as time object with correct time zone
  obs$End.Date.Time.GMT<- as.POSIXct(obs$End.Date.Time.GMT,tz="GMT") ##Time as time object with correct time zone
  obs$Start.Date.Time.local<-as.POSIXct(format(obs$Start.Date.Time.GMT,tz = local.time.zone),tz = local.time.zone)
  obs$End.Date.Time.local<-as.POSIXct(format(obs$End.Date.Time.GMT,tz = local.time.zone),tz = local.time.zone)
  utmcoor<- SpatialPoints(cbind(obs$Easting,obs$Northing), proj4string=CRS("+proj=utm +zone=16T")) ##Classify as Spatial Data. Mind the UTM Zone
  longlatcoor<- spTransform(utmcoor,CRS("+proj=longlat +datum=WGS84")) ##Convert to Latitue/Longitude
  obs$obs.Longitude<- longlatcoor$coords.x1 ##Save for later
  obs$obs.Latitude<- longlatcoor$coords.x2 ##Save for later
  obs$round<-as.POSIXct(round(as.numeric(obs$Start.Date.Time.local)/(60*60))*(60*60),origin=(as.POSIXlt('1970-01-01'))) ##will need this to match with weather data from ORD, rounds timestamp to nearest hour to match metar interval
  #Which Radar
  obs$Radar<-ifelse(obs$bird.angle.from.radar >=0 & obs$bird.angle.from.radar < 2, "ohare3", ifelse(obs$bird.angle.from.radar >= 2 & obs$bird.angle.from.radar < 4, "both", ifelse(obs$bird.angle.from.radar >=4 & obs$bird.angle.from.radar < 6,"ohare1", ifelse(obs$bird.angle.from.radar >=6 & obs$bird.angle.from.radar < 10, "ohare2", "none") ) ) )
  obs.ohare1<-subset(obs, Radar == "ohare1")
  obs.ohare2<- subset(obs, Radar == "ohare2")
  ohare3<- subset(obs, Radar == "ohare3")
  obs.both<- subset(obs, Radar == "both")
  obs.none<- subset(obs, Radar == "none")
  #ohare1 and 2 overlap, add the observations from both to each
  obs.both.1<-obs.both
  obs.both.1$Radar<-"ohare1"
  obs.both.2<-obs.both
  obs.both.2$Radar<-"ohare2"
  ohare1<-rbind(obs.ohare1,obs.both.1)
  ohare2<-rbind(obs.ohare2,obs.both.2)
  obs<-rbind(ohare1,ohare2,ohare3)
  assign("obs",obs,.GlobalEnv)
  
  #Load the radar data
  ##ARTI
  #Load Accipiter data (all of it)
  #ohare1
  setwd(file.path(working.directory,"CSV_to_Process","ARTI","ohare1"))
  filelist<-list.files(pattern="\\.csv$")
  filelist<-sample(x = filelist,size = round(length(filelist)*.5),replace = F)
  datalist<-lapply(filelist,fread)
  ohare1<-rbindlist(datalist)
  ohare1$Radar<-"ohare1"
  #ohare2
  setwd(file.path(working.directory,"CSV_to_Process","ARTI","ohare2"))
  filelist<-list.files(pattern="\\.csv$")
  filelist<-sample(x = filelist,size = round(length(filelist)*.5),replace = F)
  datalist<-lapply(filelist,fread)
  ohare2<-rbindlist(datalist)
  ohare2$Radar<-"ohare2"
  #ohare3
  setwd(file.path(working.directory,"CSV_to_Process","ARTI","ohare3"))
  filelist<-list.files(pattern="\\.csv$")
  filelist<-sample(x = filelist,size = round(length(filelist)*.5),replace = F)
  datalist<-lapply(filelist,fread)
  ohare3<-rbindlist(datalist)
  ohare3$Radar<-"ohare3"
  #Cleanup
  data<-rbind(ohare1,ohare2,ohare3)
  rm(datalist,ohare1,ohare2,ohare3)
  arti.header<-unlist(read.csv(filelist[1],header=F,nrows=1,stringsAsFactors = F))
  setnames(data, c(arti.header),c("Update.Time", "Track.ID", "Start.Time..UTC.", "Latitude", "Longitude", "Speed..m.s.", "Heading..deg.N.", "Height...m.", "RCS..dBsm.", "Range.from.radar...m.", "Azimuth.from.radar..deg..", "Intensity", "UTM.Zone", "UTM.Northing", "UTM.Easting"))
  
  #Add a unique ID
  data$long.ID<-paste0(data$Track.ID,data$Start.Time..UTC.)  #may be important for counting unique tracks
  assign("data",data,.GlobalEnv)
  
  #Use this section to do some data filtering
  
  ##Spatial filtering, remove all tracks in airport certain areas
  #setwd(file.path(working.directory,"KML"))
  #sea.class <- readOGR("SEA_Runway and Service Road_Polygon.kml","SEA_Runway and Service Road_Polygon")
  #sea.class <- spTransform(sea.class, CRS("+proj=longlat +datum=WGS84"))
  #Define Spatial Objects
  #arti.lonlat<- SpatialPoints(cbind(data$Longitude,data$Latitude), proj4string=CRS("+proj=longlat +datum=WGS84"))
  #Classify Accipiter positions
  #data.classification<-over(arti.lonlat,sea.class) #Use the KML polygon layer to determine where lon lat pairs are
  #data$Location.Classification<-data.classification$Name  #add polygon names to Accipiter data.frame
  #Subset the ARTI data by location
  #sub.data<-subset(data,is.na(Location.Classification)==T) #Only include data that is NOT within a polygon.
  
  ## Define some rules for filter out non-bird tracks
  #sub.data <- subset(sub.data, RCS..dBsm. >= -35 & RCS..dBsm. <= -10)
  #sub.data <- subset(sub.data, Speed..m.s. >= 20*0.44704 & Speed..m.s. <= 60*0.44704)
  #sub.data <- subset(sub.data, (Heading..deg.N. >= 60 & Heading..deg.N. <= 120)|(Heading..deg.N. >= 150 & Heading..deg.N. <= 210)|(Heading..deg.N. >= 240 & Heading..deg.N. <= 330))
  #sub.data <- subset(sub.data, Range.from.radar...m. >= 0.25*1609.344) 
  
  #data <- sub.data
  
  #Perform the 2d density calculation for the extents as defined by ULC (upper left corner lonlat), LRC (lower right corner lonlat)    
  dens <-kde2d(x = data$Longitude,y = data$Latitude,n = n,lims=c(ULC[1], LRC[1], LRC[2],ULC[2]))
  dens.data.frame<-data.frame(dens$z)
  colnames(dens.data.frame)<-dens$y
  rownames(dens.data.frame)<-dens$x
  assign("dens.data.frame",dens.data.frame,.GlobalEnv)
  dens.data<-melt(data.matrix(dens.data.frame))
  colnames(dens.data) <-c("Longitude","Latitude","DensityValue")
  assign("dens.data",dens.data,.GlobalEnv)
  ifelse(length(n) ==2,csv.filename<-paste0(title,"_n",n[1],".",n[2],".csv"),csv.filename<-paste0(title,"_n",n,".csv"))
  write.csv(dens.data,file=file.path(working.directory,"Results",csv.filename),row.names=F)
  
  #Subset the radar data based on the desired extents for the map as defined by ULC, LRC
  sub.data<-subset(data, Longitude >= ULC[1] & Longitude <= LRC[1])
  sub.data<-subset(sub.data, Latitude >= LRC[2] & Latitude <= ULC[2])
  assign("sub.data",sub.data,.GlobalEnv)
  #Get a black and white map of SEA centered on the Olympic Radar
  map<-get_map(location = c(-87.934061,41.993163),zoom = 12,maptype = "satellite",color = "bw")
  #Get map attirbutes (corners)  
  bb<-attr(map,"bb")
  assign("bb",bb,.GlobalEnv)  
  #Use map attributes to create a scale bar
  sbar <- data.frame(lon.start = c(bb$ll.lon + 0.1*(bb$ur.lon - bb$ll.lon)),
                     lon.end = c(bb$ll.lon + 0.25*(bb$ur.lon - bb$ll.lon)),
                     lat.start = c(bb$ll.lat + 0.1*(bb$ur.lat - bb$ll.lat)),
                     lat.end = c(bb$ll.lat + 0.1*(bb$ur.lat - bb$ll.lat)))
  sbar$distance = distGeo(p1 = c(sbar$lon.start,sbar$lat.start),p2 = c(sbar$lon.end,sbar$lat.end)) #Distance across downloaded map in meters
  ptspermm <- 2.83464567  # need this because geom_text uses mm, and themes use pts. Urgh.
  
  #Plot Map Density Map
  m <- ggmap(map, extent = "normal", maprange=FALSE) %+% sub.data + aes(x = Longitude, y = Latitude) #Plot the map
  m2 <- m + geom_density2d(data = sub.data, aes(x = Longitude, y = Latitude),n=n,size=0.2, na.rm=T) #Plot the density contours, n=n and default bandwidth
  m3 <- m2 + stat_density2d(data = sub.data, aes(x = Longitude, y = Latitude, fill = ..level.., alpha = ..level..),n=n, size = 0.01, bins = 16, geom = 'polygon',na.rm=T) #Add Transparent fill to density contours, n=n and default bandwidth
  m4 <- m3 + scale_fill_gradient(low = "green", high = "red") + scale_alpha(range = c(0.25, 0.75), guide = FALSE) #Color range and alpha levels to use for fill
  m5 <- m4 + geom_segment(data=sbar,aes(x=lon.start,xend=lon.end,y=lat.start,yend=lat.end),color="white")+geom_text(data = sbar, aes(x = (lon.start + lon.end)/2, y = lat.start + 0.025*(bb$ur.lat - bb$ll.lat),label = paste(format(distance*.000621371, digits = 4,nsmall = 2),'mi')),hjust = 0.5, vjust = 0, size = 8/ptspermm,color="white")  + coord_map(projection="mercator",xlim=c(bb$ll.lon, bb$ur.lon),ylim=c(bb$ll.lat, bb$ur.lat))+theme(plot.title = element_text(lineheight=.8, face="bold"))+theme(axis.text = element_text(size=8))#Add Scale Bar
  ifelse(length(n)==2,subtitle.heatmap<-paste0("2-D Kernel Density Estimate Heatmap n=",n[1],", ",n[2]),subtitle.heatmap<-paste0("2-D Kernel Density Estimate Heatmap n=",n))
  final <- m5 + ggtitle(label = title,subtitle = subtitle.heatmap) + theme_map() + guides(fill=F) + facet_wrap(~Radar,strip.position = "left") + geom_point(data = obs,aes(x=bird.lon,y=bird.lat),color="blue")
  ifelse(length(n)==2,filename<-paste0(title,"_n",n[1],".",n[2],"_Heatmap.png"),filename<-paste0(title,"_n",n,"_Heatmap.png"))
  png(filename = file.path(working.directory,"Results",filename),width = 30,height = 10.20,units ="in",res=300)
  north2(ggp = final,x = 0.95, y = 0.135,scale = .1)
  dev.off()
  #Plot Map with point Centroids
  q <- m + geom_point(data = dens.data, aes(x=Longitude,y = Latitude),size=0.0001,color="yellow")
  q2 <- q + geom_segment(data=sbar,aes(x=lon.start,xend=lon.end,y=lat.start,yend=lat.end),color="white")+geom_text(data = sbar, aes(x = (lon.start + lon.end)/2, y = lat.start + 0.025*(bb$ur.lat - bb$ll.lat),label = paste(format(distance*.000621371, digits = 4,nsmall = 2),'mi')),hjust = 0.5, vjust = 0, size = 8/ptspermm,color="white")  + coord_map(projection="mercator",xlim=c(bb$ll.lon, bb$ur.lon),ylim=c(bb$ll.lat, bb$ur.lat))+theme(plot.title = element_text(lineheight=.8, face="bold"))+theme(axis.text = element_text(size=8))#Add Scale Bar
  ifelse(length(n)==2,subtitle.centroid<-paste0("2-D Kernel Density Estimate Point Centroid n=",n[1],", ",n[2]),subtitle.centroid<-paste0("2-D Kernel Density Estimate Point Centroid n=",n))
  q.final <- q2 + ggtitle(label = title,subtitle = subtitle.centroid) + theme_map() + guides(fill=F)
  ifelse(length(n)==2,filename2 <- paste0(title,"_n",n[1],".",n[2],"_PointCentroid.png"),filename2 <- paste0(title,"_n",n,"_PointCentroid.png"))
  png(filename = file.path(working.directory,"Results",filename2),width = 10,height = 10.20,units ="in",res=300)
  north2(ggp = q.final,x = 0.95, y = 0.135,scale = .1)
  dev.off()
  #%p stuff
  for(i in 1:length(p)){
    #Find the top p percent density values and save as CSV
    top<-subset(dens.data, DensityValue > quantile(DensityValue, prob = 1 - p[i]/100))
    ifelse(length(n)==2,filename.top<-paste0(title,"_top_",p[i],"_n",n[1],".",n[2],".csv"),filename.top<-paste0(title,"_top_",p[i],"_n",n,".csv"))
    write.csv(x = top,file = file.path(working.directory,"Results",filename.top),row.names = F)
    #Create outline and spatial filter (KML) for top %p
    #Find the distance between grid point centroids for the lower left of the defined extent
    point.1<-c(as.numeric(rownames(dens.data.frame)[2]),as.numeric(colnames(dens.data.frame)[1]))
    point.2<-c(as.numeric(rownames(dens.data.frame)[1]),as.numeric(colnames(dens.data.frame)[1]))
    point.3<-c(as.numeric(rownames(dens.data.frame)[1]),as.numeric(colnames(dens.data.frame)[2]))
    ns.dist<-distGeo(p1 = point.2,p2 = point.3) ##Calculate the distance between adjacent grid point centroids in North South direction
    assign("ns.dist",ns.dist,.GlobalEnv)
    ew.dist<-distGeo(p1 = point.2,p2 = point.1) ##Caltulate the distance between adjacent grid point centroids in East West direction
    assign("ew.dist",ew.dist,.GlobalEnv)
    ifelse(ns.dist >= ew.dist, width<-ns.dist,width<-ew.dist) ##Determine if NS or EW distance is greater, use the larger of the two.
    #Calculate appropriate buffer around each point and merge into polygons. Save as KML  
    width.1<-(width/2) + 1 #Half of the distance plus 1 meter to ensure overlap between buffers
    spdf<-SpatialPointsDataFrame(coords = cbind(top$Longitude,top$Latitude),data = top,proj4string = CRS("+proj=longlat +datum=WGS84")) ##Add top %p density values to a SpatialPointsDataFrame
    utmspdf<-spTransform(x = spdf,CRSobj = CRS("+proj=utm +zone=16T")) ##Transform to UTM so that buffer size can be specified in meters CHANGE THE UTM ZONE BASED ON RADAR LOCATION
    buf1 <- gBuffer(spgeom = utmspdf,width=width.1,byid = T,capStyle = "SQUARE") ##Put a square with "radius" width.1 around each point centroid
    spatial.filter <- gUnaryUnion(buf1) ## join the squares into one or more polygons depending on overlap
    buffer <- spTransform(spatial.filter, CRS("+proj=longlat +datum=WGS84")) #change the projection to longlat for ggmap
    ifelse(length(n)==2,filename.kml<-paste0(title,"_top_",p[i],"_n",n[1],".",n[2],".kml"),filename.kml<-paste0(title,"_top_",p[i],"_n",n,".kml"))
    kml(obj = spatial.filter,file.name = file.path(working.directory,"Results",filename.kml))
    
    
    #Map the top p percent density values  
    r <- m + geom_point(data = top, aes(x=Longitude,y = Latitude),size=0.0001,color="dark red")
    r2 <- r + geom_segment(data=sbar,aes(x=lon.start,xend=lon.end,y=lat.start,yend=lat.end),color="white")+geom_text(data = sbar, aes(x = (lon.start + lon.end)/2, y = lat.start + 0.025*(bb$ur.lat - bb$ll.lat),label = paste(format(distance*.000621371, digits = 4,nsmall = 2),'mi')),hjust = 0.5, vjust = 0, size = 8/ptspermm,color="white")  + coord_map(projection="mercator",xlim=c(bb$ll.lon, bb$ur.lon),ylim=c(bb$ll.lat, bb$ur.lat))+theme(plot.title = element_text(lineheight=.8, face="bold"))+theme(axis.text = element_text(size=8))#Add Scale Bar
    subtitle.top<-paste0("Top ",p[i],"%", " Grid Points (Point Centroid)")
    r.final <- r2 + ggtitle(label = title,subtitle = subtitle.top) + geom_polygon(data=buffer,aes(x=long,y=lat,group=group,fill=hole),alpha=0.2) + theme_map() + guides(fill=F)
    ifelse(length(n)==2,filename.top.map<-paste0(title,"_top_",p[i],"_n",n[1],".",n[2],"_map.png"),filename.top.map<-paste0(title,"_top_",p[i],"_n",n,"_map.png"))
    png(filename = file.path(working.directory,"Results",filename.top.map),width = 10,height = 10.20,units ="in",res=300)
    north2(ggp = r.final,x = 0.95, y = 0.135,scale = .1)
    dev.off()
  }
  
}