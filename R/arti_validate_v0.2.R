validate <- function(start.time, end.time, fov=95, target.heading, obs.bearing, obs.UTM.Northing, obs.UTM.Easting, obs.dist.estimate.yards, obs.alt.estimate.ft, weather, radar, csv,oh.factor,USDA.grid.loc) {
  #Load Required Packages and Data
    #Libraries (for stuff)
      library(sp) ##scale bar
      library(raster) ##scale bar
      library(GISTools) ##North Arrow
      library(circular) ##Bearings
      #library(ggplot2) ##Plots
      library(maptools) ##Airport shapefile
      library(rgdal) ##Coordinate Conversion
      library(calibrate) ##Labeling Plots
      library(plotrix) ## Drawing circles
      library(geosphere) ##FOV Lines
    #Is there radar data... do I have to do any work at all?
      #Time
        #Convert user input to UNIX time
        start.time <- as.numeric(as.POSIXlt(start.time, "%m/%d/%Y %H:%M:%S", origin="1970-01-01", tz="GMT"))
          #assign("start.time",start.time,.GlobalEnv)
        end.time <- as.numeric(as.POSIXlt(end.time, "%m/%d/%Y %H:%M:%S", origin="1970-01-01", tz="GMT"))
          #assign("end.time",end.time,.GlobalEnv)
        filename.time <- as.character(as.POSIXct(start.time,origin="1970-01-01",tz="GMT"), "%Y.%m.%d_%H%M%S")
    #Define Variables
      #Directories
      working.directory <- getwd()
    #Create an output directory
      subDir <- "Validated"
      dir.create(file.path(working.directory, subDir), showWarnings = FALSE)
      csv.filename <- paste0("R_",radar,"_",filename.time,"_",obs.UTM.Northing,"N_",obs.UTM.Easting,"E_",fov,"FOV_",obs.bearing,"bearing_",target.heading,"_heading",".csv")
      no.match.csv.filename<-paste0("R_",radar,"_",filename.time,"_",obs.UTM.Northing,"N_",obs.UTM.Easting,"E_",fov,"FOV_",obs.bearing,"bearing_",target.heading,"_heading_NO_MATCH",".csv")
      insufficient.csv.filename<-paste0("R_",radar,"_",filename.time,"_",fov,"FOV_",obs.bearing,"bearing_",target.heading,"_heading_insufficient",".csv")
      rain.csv.filename<-paste0("R_",radar,"_",filename.time,"_",obs.UTM.Northing,"N_",obs.UTM.Easting,"E_",fov,"FOV_",obs.bearing,"bearing_",target.heading,"_RAIN",".csv")
      no.data.csv.filename<-paste0("R_",radar,"_",filename.time,"_",obs.UTM.Northing,"N_",obs.UTM.Easting,"E_",fov,"FOV_",obs.bearing,"bearing_",target.heading,"_NO_DATA",".csv")
    if(file.exists(csv)){
    #Data (get some)
      data <- read.csv(csv, header=TRUE, sep=",",stringsAsFactors = FALSE)
      data$UNIX.Update.Time <- as.numeric(as.POSIXlt(data$Update.Time, origin="1970-01-01", tz="GMT"))
      data$Heading..deg.N.circular <- circular(data$Heading..deg.N.,units="degrees",template="geographics",modulo="2pi",rotation="clock")
      oharemap <- readShapePoly("TRAN_17_Illinois_GU_STATEORTERRITORY/Trans_AirportRunway.shp")
      oharegrid <- readShapePoly("ORD_wildlife_grid/ORD_grid_poly_2.shp",IDvar="grid")
    
    #Should anything be done
    if(weather != "Rain"){
    if(!is.na(obs.UTM.Northing) | !is.na(obs.UTM.Easting)){

    #Radar
      radar.Northing <- 4649444.3
      radar.Easting <- 422631.5
      radar.Lat <- 41.99321
      radar.Long <- -87.9341
    
    if(radar == "AR1"){
      radar.angle <- 2
    }
    else if(radar == "AR2-1"){
      radar.angle <- 4
    }
    else if(radar == "AR2-2" & 1334793600 <= start.time & start.time <= 1339027200){
      radar.angle <- 30
    }
    else if(radar == "AR2-2" & 1349913600 <= start.time & start.time <= 1360022400){
      radar.angle <- 30 
    }
     else{radar.angle <- 8
    }
    #Observer (convert observer position to lat and long for plotting)
      observer.position.utm <- SpatialPoints(cbind(obs.UTM.Easting,obs.UTM.Northing),proj4string=CRS("+proj=utm +zone=16T"))
      observer.position.longlat<-spTransform(observer.position.utm, CRS("+proj=longlat"))
      observer.long<-as.numeric(observer.position.longlat$obs.UTM.Easting)
      #assign("observer.long",observer.long,.GlobalEnv)
      observer.lat<-as.numeric(observer.position.longlat$obs.UTM.Northing)
    
    #Units (use meters and ensure circular statistics work properly)
      obs.bearing <- circular(obs.bearing,units="degrees",template="geographics",modulo="2pi",rotation="clock")
      obs.dist.estimate.meters <- obs.dist.estimate.yards * 0.9144 
      obs.alt.estimate.meters <- obs.alt.estimate.ft * 0.3048
      
    #Observer Bird Position
      
      obs.bird.bearing<-obs.bearing * pi/180
      obs.bird.x<-obs.UTM.Easting+obs.dist.estimate.meters*sin(obs.bird.bearing)  ##X UTM Easting
      obs.bird.y<-obs.UTM.Northing+obs.dist.estimate.meters*cos(obs.bird.bearing)  ##Y UTM Northing
      #Oh no! We need lat long!
      obs.bird.position.utm <- SpatialPoints(cbind(obs.bird.x,obs.bird.y),proj4string=CRS("+proj=utm +zone=16T"))
      observer.bird.position.longlat<-spTransform(obs.bird.position.utm, CRS("+proj=longlat"))
      observer.bird.long<-as.numeric(observer.bird.position.longlat$obs.bird.x)
      observer.bird.lat<-as.numeric(observer.bird.position.longlat$obs.bird.y)
      
    
#Start finding tracks      
      #Subset data based on observer time.interval. Allow 10 seconds on either side to account for radar processing/time sync issues
        sub.time <- subset(data, UNIX.Update.Time >= start.time - 10 & UNIX.Update.Time <= end.time + 10)
          #assign("sub.time", sub.time,.GlobalEnv)
      #Calculate bearing for each TrackPoint from Observation Point
        sub.time$bearing.to.target <-ifelse((sub.time$UTM.Easting - obs.UTM.Easting >=0 & sub.time$UTM.Northing -obs.UTM.Northing >=0),(atan(((sub.time$UTM.Easting - obs.UTM.Easting)/(sub.time$UTM.Northing - obs.UTM.Northing))))/pi*180,ifelse(sub.time$UTM.Northing -obs.UTM.Northing <0,(atan(((sub.time$UTM.Easting - obs.UTM.Easting)/(sub.time$UTM.Northing - obs.UTM.Northing))))/pi*180+180,(atan(((sub.time$UTM.Easting - obs.UTM.Easting)/(sub.time$UTM.Northing - obs.UTM.Northing))))/pi*180 + 360))
      #Calculate distance to each TrackPoint from Observation Point (m)
        sub.time$distance.to.target.m <- sqrt((obs.UTM.Easting-sub.time$UTM.Easting)^2 + (obs.UTM.Northing-sub.time$UTM.Northing)^2)
      #Calculate difference between observer position guestimate and actual track position
        #sub.time$distance.difference.m <- abs(obs.dist.estimate.meters - sub.time$distance.to.target.m)
        sub.time$distance.difference.m <- sqrt((obs.bird.x-sub.time$UTM.Easting)^2 + (obs.bird.y-sub.time$UTM.Northing)^2)
      #Calculate beam dimensions at track update
        sub.time$low.beam.height.m <- tan((radar.angle-2)*(pi/180))*sub.time$Range.from.radar...m.
        sub.time$high.beam.height.m <- tan((radar.angle+2)*(pi/180))*sub.time$Range.from.radar...m.
      #Calculate difference between observer altitude estimate and calculated value
        #sub.time$altitude.difference.m <- sub.time$Height...m. - obs.alt.estimate.meters
      #Is the Observer Altitude estimate within the beam pattern for this radar at this tracks position?
        sub.time$within.beam <- obs.alt.estimate.meters >= sub.time$low.beam.height.m & obs.alt.estimate.meters <= sub.time$high.beam.height.m
      #Calculate difference between observer altitude estimate and top and bottom of the beam
        ifelse(obs.alt.estimate.meters > sub.time$high.beam.height.m,sub.time$alt.diff.to.top<-sub.time$high.beam.height.m - obs.alt.estimate.meters,sub.time$alt.diff.to.top<-NA)
        ifelse(obs.alt.estimate.meters < sub.time$low.beam.height.m, sub.time$alt.diff.to.bottom<- obs.alt.estimate.meters - sub.time$low.beam.height.m,sub.time$alt.diff.to.bottom<-NA)
          
          #Is the Observer Altitude estimate within the beam pattern for this radar at this tracks position plus half of obsrerver estimate?
            #sub.time$within.beam.1pts <- obs.alt.estimate.meters + (.5 * obs.alt.estimate.meters) >= sub.time$low.beam.height.m & obs.alt.estimate.meters + (.5 * obs.alt.estimate.meters) <= sub.time$high.beam.height.m
      #Subset data based on observer target.heading
        if(target.heading == "N" | target.heading == "n"){
          sub.heading <- subset(sub.time, Heading..deg.N.circular >=315 | Heading..deg.N.circular <=45)
        }
        else if(target.heading == "NNE" | target.heading == "nne") {
          sub.heading <- subset(sub.time, Heading..deg.N.circular >=337.5 | Heading..deg.N.circular <=67.5)
        }
        else if(target.heading == "NE" | target.heading == "ne") {
          sub.heading <- subset(sub.time, Heading..deg.N.circular <=90)
        }
        else if(target.heading == "ENE" | target.heading == "ene") {
          sub.heading <- subset(sub.time, Heading..deg.N.circular >=22.5 & Heading..deg.N.circular <=112.5)
        }
        else if(target.heading == "E" | target.heading == "e") {
          sub.heading <- subset(sub.time, Heading..deg.N.circular >=45 & Heading..deg.N.circular <=135)
        }
        else if(target.heading == "ESE" | target.heading == "ese") {
          sub.heading <- subset(sub.time, Heading..deg.N.circular >=67.5 & Heading..deg.N.circular <=157.5)
        }
        else if(target.heading == "SE" | target.heading == "se") {
          sub.heading <- subset(sub.time, Heading..deg.N.circular >=90 & Heading..deg.N.circular <=180)
        }
        else if(target.heading == "SSE" | target.heading == "sse") {
          sub.heading <- subset(sub.time, Heading..deg.N.circular >=112.5 & Heading..deg.N.circular <=202.5)
        }
        else if(target.heading == "S" | target.heading == "s") {
          sub.heading <- subset(sub.time, Heading..deg.N.circular >=135 & Heading..deg.N.circular <=225)
        }
        else if(target.heading == "SSW" | target.heading == "ssw") {
          sub.heading <- subset(sub.time, Heading..deg.N.circular >=157.5 & Heading..deg.N.circular <=247.5)
        }
        else if(target.heading == "SW" | target.heading == "sw") {
          sub.heading <- subset(sub.time, Heading..deg.N.circular >=180 & Heading..deg.N.circular <=270)
        }
        else if(target.heading == "WSW" | target.heading == "wsw") {
          sub.heading <- subset(sub.time, Heading..deg.N.circular >=202.5 & Heading..deg.N.circular <=292.5)
        }
        else if(target.heading == "W" | target.heading == "w") {
          sub.heading <- subset(sub.time, Heading..deg.N.circular >=225 & Heading..deg.N.circular <=315)
        }
        else if(target.heading == "WNW" | target.heading == "wnw") {
          sub.heading <- subset(sub.time, Heading..deg.N.circular >=247.5 & Heading..deg.N.circular <=337.5)
        }
        else if(target.heading == "NW" | target.heading == "nw") {
          sub.heading <- subset(sub.time, Heading..deg.N.circular >=270)
        }
        else if(target.heading == "NNW" | target.heading == "nnw") {
          sub.heading <- subset(sub.time, Heading..deg.N.circular >=292.5 | Heading..deg.N.circular <=22.5)
        }
        else(sub.heading <- sub.time)
      #Determine if Observation is Overhead based on obs.bearing variable and do something different if it is
        if(obs.dist.estimate.meters == 0 | obs.dist.estimate.meters <= 10 ) {
          sub.bearing <- subset(sub.heading, distance.to.target.m <= oh.factor * obs.alt.estimate.meters + 10) ##Radius (m) = oh.factor * obs.alt.estimate.meters +100
        }
        else{
      #Based on the field of view, calculate the lower bound of acceptable bearings
        low.bearing <- obs.bearing -(fov/2)
        if(low.bearing < 0){
          low.bearing.1 <- low.bearing + 360
        } else {
        low.bearing.1 <- low.bearing
        }
      #Based on the field of view, calculate the upper bound of acceptable bearings
        high.bearing <- obs.bearing +(fov/2)
        if(high.bearing > 360){
          high.bearing.1 <- high.bearing - 360
        } else {
        high.bearing.1 <- high.bearing
        }
      #Subset based on low and high bearings and store as new dataframe (sub.bearing)
        if(high.bearing.1 > low.bearing.1){
          sub.bearing <- subset(sub.heading, bearing.to.target >= low.bearing.1 & bearing.to.target <= high.bearing.1)
          #Restrict Observation range To X * observation distance estimate
          sub.bearing <-subset(sub.bearing,distance.to.target.m <= 3 * obs.dist.estimate.meters)
        } else {
        sub.bearing <- subset(sub.heading, bearing.to.target >= low.bearing.1 | bearing.to.target <= high.bearing.1)
        #Restrict Observation range To X * observation distance estimate
        sub.bearing <-subset(sub.bearing,distance.to.target.m <= 3 * obs.dist.estimate.meters)
        }
        }
      #Order the data by Ascending Track.ID and then by UNIX.Update.Time. Determine Grid location, and if it matches USDA guess. Save result.
        validate.list <- sub.bearing[with(sub.bearing, order(Track.ID, UNIX.Update.Time)), ]
        if(nrow(validate.list)>=1){
        pts<-SpatialPoints(cbind(validate.list$Longitude,validate.list$Latitude))
        USDA.grid.location<-over(pts,oharegrid)
        validate.list$USDA.grid.location<-USDA.grid.location$grid
        validate.list$match.USDA.grid.estimate<-USDA.grid.loc==validate.list$USDA.grid.location}
        assign("validate.list",validate.list,.GlobalEnv)

#Graph It!!!
      #Create a title and subtitle for the graph based on previous user input
        title <- paste0("Chicago O'Hare International Airport \n",radar," Tracks ", obs.bearing,"° from Observer with ",fov, "° FOV ","from \n ",round(observer.lat,digits=4),"°, ", round(observer.long,digits=4),"° and ",target.heading," Heading ")
        sub.title <- paste0(as.POSIXct(start.time,origin="1970-01-01",tz="GMT")," GMT - ", as.POSIXct(end.time,origin="1970-01-01",tz="GMT")," GMT")
      #Process Results
        unique.track.results <- unique(validate.list$Track.ID)
        other.positions <- subset(data, Track.ID %in% unique.track.results)      
      #Plot subsetted data, observation point, ARTI position
        par(mar=c(6.1,4.1,5.1,2.1))
        plot(oharemap,xlim=c(-87.997,-87.81),ylim=c(41.938,42.041))
        #par(new=T)
        plot(oharegrid,xlim=c(-87.997,-87.81),ylim=c(41.938,42.041),add=TRUE)
        text(coordinates(oharegrid),labels=oharegrid$grid,cex=.5)
        par(new=T)
        plot(other.positions$Longitude,other.positions$Latitude,xlim=c(-87.997,-87.81),ylim=c(41.938,42.041),col="grey",xlab="",ylab="",axes=F)
        par(new=T)
        plot(validate.list$Longitude,validate.list$Latitude,xlim=c(-87.997,-87.81),ylim=c(41.938,42.041),col="dark red",xlab="Longitude",ylab="Latitude",sub=sub.title)
        par(new=T)
        plot(radar.Long,radar.Lat,xlim=c(-87.997,-87.81),ylim=c(41.938,42.041),col="black",pch=17,axes=F,xlab="",ylab="")
        par(new=T)
        plot(observer.long,observer.lat,xlim=c(-87.997,-87.81),ylim=c(41.938,42.041),col="blue",pch=10,axes=F,xlab="",ylab="")
        par(new=T)
        points(observer.bird.long,observer.bird.lat,col="green",pch=20,cex=1)
        par(new=T)
        title(title)
        legend(-87.855,42.04,c("AR1","Observer","Bird Position Estimate"),pch=c(17,10,20),col=c("black","blue","green"),cex=0.60)
      #Add FOV lines to the plot, if not an Overhead observation
        if(obs.dist.estimate.meters == 0 | obs.dist.estimate.meters <= 10) {
          circle.radius.x<-obs.UTM.Easting+(oh.factor * obs.alt.estimate.meters +100)*sin(90)  ##X UTM Easting
          circle.radius.y<-obs.UTM.Northing+(oh.factor * obs.alt.estimate.meters +100)*cos(90)  ##Y UTM Northing
          #Oh no! We need lat long!
          circle.radius.utm <- SpatialPoints(cbind(circle.radius.x,circle.radius.y),proj4string=CRS("+proj=utm +zone=16T"))
          circle.radius.longlat<-spTransform(circle.radius.utm, CRS("+proj=longlat"))
          circle.radius.long<-as.numeric(circle.radius.longlat$circle.radius.x)
          #assign("circle.radius.long",circle.radius.long,.GlobalEnv)
          circle.radius.lat<-as.numeric(circle.radius.longlat$circle.radius.y)
          draw.circle(x=observer.long,y=observer.lat,radius=abs(observer.long-circle.radius.long),nv=100,border="darkcyan")
          }
        else {
          #Calculate endpoint for lower FOV
            fov.low.bearing<-low.bearing.1 * pi/180
            fov.low.x<-obs.UTM.Easting+10000*sin(fov.low.bearing)  ##X UTM Easting
            fov.low.y<-obs.UTM.Northing+10000*cos(fov.low.bearing)  ##Y UTM Northing
          #Oh no! We need lat long!
            fov.low.position.utm <- SpatialPoints(cbind(fov.low.x,fov.low.y),proj4string=CRS("+proj=utm +zone=16T"))
            fov.low.position.longlat<-spTransform(fov.low.position.utm, CRS("+proj=longlat"))
            fov.low.long<-as.numeric(fov.low.position.longlat$fov.low.x)
            fov.low.lat<-as.numeric(fov.low.position.longlat$fov.low.y)
            low.points<- gcIntermediate(c(observer.long,observer.lat),c(fov.low.long,fov.low.lat),50)
          #Calculate endpoint for upper FOV
            fov.high.bearing<-high.bearing.1 * pi/180
            fov.high.x<-obs.UTM.Easting+10000*sin(fov.high.bearing)  ##X UTM Easting
            fov.high.y<-obs.UTM.Northing+10000*cos(fov.high.bearing)  ##Y UTM Northing
          #Oh no! We need lat long!
            fov.high.position.utm <- SpatialPoints(cbind(fov.high.x,fov.high.y),proj4string=CRS("+proj=utm +zone=16T"))
            fov.high.position.longlat<-spTransform(fov.high.position.utm, CRS("+proj=longlat"))
            fov.high.long<-as.numeric(fov.high.position.longlat$fov.high.x)
            fov.high.lat<-as.numeric(fov.high.position.longlat$fov.high.y)
            high.points<- gcIntermediate(c(observer.long,observer.lat),c(fov.high.long,fov.high.lat),50)
          #Add to plot
            lines(low.points,col="darkcyan")
            lines(high.points,col="darkcyan")
            }
      #Scalebar and North Arrow
            scalebar(4,xy=c(-87.855,41.94),type="bar",divs=4,lonlat=TRUE,below="Kilometers",cex=0.75)
            north.arrow(xb=-87.8305,yb=41.9525,len=0.0025,lab="N")
        
      #Lets Label!
        #textxy(radar.Long,radar.Lat,labs=radar)
        #textxy(observer.long,observer.lat,labs="Observer")
        #if(obs.bearing == 0 & obs.dist.estimate.meters == 0){
          #textxy(observer.bird.long,observer.bird.lat-0.002,labs="Bird Position Estimate")}
        #else{textxy(observer.bird.long,observer.bird.lat,labs="Bird Position Estimate")}
        if(nrow(validate.list)>=1){
          unique.track.results.df <- data.frame(unique.track.results)
      #assign("unique.track.results.df",unique.track.results.df,.GlobalEnv)
        sub.track <- data.frame()
        test <- data.frame()
        for(x in 1:nrow(unique.track.results.df)){
          unique.trackID <- unique.track.results.df[x,]
          unique.track.positions <- subset(other.positions, Track.ID == unique.trackID)
          validated.track.positions <- subset(validate.list, Track.ID == unique.trackID)
          max.track.time <- max(unique.track.positions$UNIX.Update.Time)
          max.unique.track.time <- subset(unique.track.positions, UNIX.Update.Time == max.track.time)
          sub.track <- rbind(sub.track, max.unique.track.time)
          }
          textxy(sub.track$Longitude,sub.track$Latitude,labs=sub.track$Track.ID)
            #assign("sub.track",sub.track,.GlobalEnv)
          }

        if(nrow(validate.list) >= 1){
        filename.time <- as.character(as.POSIXct(start.time,origin="1970-01-01",tz="GMT"), "%Y.%m.%d_%H%M%S")
        graph.filename <- paste0("R_",radar,"_",filename.time,"_",obs.UTM.Northing,"N_",obs.UTM.Easting,"E_",fov,"FOV_",obs.bearing,"bearing_",target.heading,"_heading",".png")
        dev.copy(png,filename = file.path(working.directory,subDir,graph.filename), width = 1024, height = 736,res=100)
        dev.off()
        #Write a csv file containing the filtered TrackPoints
        
        write.csv(validate.list[with(validate.list, order(Track.ID, UNIX.Update.Time)), ], file = file.path(working.directory,subDir,csv.filename),quote=FALSE, row.names = FALSE)
}
        else{
          no.match<-"No tracks matching observation characteristics. Bummer."
          write.csv(no.match, file = file.path(working.directory,subDir,no.match.csv.filename),quote=FALSE, row.names = FALSE)
        }
}
else{
  insufficient<-"Insufficient observation information. Ask Sid Majumdar (217-722-3726) why is this is."
  write.csv(insufficient, file = file.path(working.directory,subDir,insufficient.csv.filename),quote=FALSE, row.names = FALSE)
}
}
else{
  rain<-"Nothing to see here. It was raining. Ask Sid Majumdar (217-722-3726) why it was raining."
  write.csv(rain, file = file.path(working.directory,subDir,rain.csv.filename),quote=FALSE, row.names = FALSE)
}
}
else{
  no.data<-"No radar data. Nothing to do. Ask Sid Majumdar (217-722-3726) for more radar data."
  write.csv(no.data, file = file.path(working.directory,subDir,no.data.csv.filename),quote=FALSE, row.names = FALSE) 
}
}

