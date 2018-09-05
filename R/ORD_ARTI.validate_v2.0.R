ORD_ARTI.validate_v2.0<-function(obs.csv,radar.designation,weather.station.code,local.time.zone){
  
  #################### <VERSION INFO> ##################### 
    #Elwood: It's 106 miles to Chicago, we got a full tank of gas, half a pack of cigarettes, it's dark... and we're wearing sunglasses.
    #Jake: Hit it.
    
    
    ##v2.0 - Modified for ORD, added full tracks to output, parameters file output, map extents (open street maps?)... weather? Multiple radars simultaneously?
    ##Speed it up a notch
  #################### </VERSION INFO> ##################### 
  
  #################### <SETUP> ##################### 
  #Load required packages
    require(ggmap)
    require(ggthemes)
    require(data.table)
    require(bit64)
    require(rgdal)
    require(geosphere)
    require(circular)
    require(scales)
    require(grid)
    require(cowplot)
    require(plyr)
    require(weatherData)
  
  #Define some variables.
    working.directory<-file.path(path.expand("~"),"ORD_Opportunistic")
  
  #Create directories
    dir.create(file.path(working.directory,"Results","CSV"),showWarnings = F)
    dir.create(file.path(working.directory,"Results","Map"),showWarnings = F)
    dir.create(file.path(working.directory,"Results","Full.Tracks"),showWarnings = F)
  #################### </SETUP> #####################   
  
  #################### <MODIFY ME> #####################  
    #Time Allowance: +/- this value
      time.allowance <- 10 ##(seconds)
      assign("time.allowance",time.allowance,.GlobalEnv)
    #Bearing Allowance: +/- this value over 2
      bearing.allowance <- 60 ##(degrees)
    #Inclination Allowance: ##degrees - Only cosmetic
      inclination.allowance <-4
      assign("inclination.allowance",inclination.allowance,.GlobalEnv)
    #Search Distance from observer (m)
      search.distance.constant<- 100 #minimum distance to search from observer in m
      search.distance.multiplier <- 3
      assign("search.distance.constant",search.distance.constant,.GlobalEnv)
      assign("search.distance.multiplier",search.distance.multiplier,.GlobalEnv)
    #Overhead Info
      overhead.radius.constant<- 100 #minimum distance to search from the observer in m
      overhead.multiplier<-1
      assign("overhead.radius.constant",overhead.radius.constant,.GlobalEnv)
      assign("overhead.multiplier",overhead.multiplier,.GlobalEnv)
      npoints = 100 #How "nicely" do you want the overhead circle drawn?
    #Map Type
      map.type<- "terrain"
      assign("map.type",map.type,.GlobalEnv)
    #Map Zoom
      zoom <- 13
  #################### </MODIFY ME> #####################
  
  #Save all these parameters
    values<-c(obs.csv,time.allowance,bearing.allowance,inclination.allowance,search.distance.constant,search.distance.multiplier,overhead.radius.constant,overhead.multiplier,map.type)  
    parameters<-data.frame(values)
    row.names(parameters)<-c("Observation CSV","Time Allowance (sec)","Bearing Allowance (deg)","Inclination Allowance (deg)","Search Distance Constant (m)","Search Distance Multiplier","Overhead Radius Constant (m)","Overhead Multiplier","Map Type")
    assign("parameters",parameters,.GlobalEnv)
    write.csv(x = parameters,file = file.path(working.directory,"Results","parameters.csv"),row.names = T)  
  #################### <OBSERVATION DATA> #####################     
  
  #Load observation data
    setwd(file.path(working.directory,"CSV_to_Process","Observations"))  
    obs.raw<-read.csv(obs.csv,header=T)
  #Remove incomplete observations
    obs<-na.omit(obs.raw)
    obs.incomplete<-obs.raw[!complete.cases(obs.raw),] #Document which ones were removed.
    assign("obs.incomplete",obs.incomplete,.GlobalEnv)
    write.csv(x = obs.incomplete,file = file.path(working.directory,"Results","Incomplete.Observations.csv"))
    
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
  
  #Where are you?    
    site.name<-revgeocode(c(obs$obs.Longitude[1],obs$obs.Latitude[1]),output = "more") 
  
  #Any interesting weather where you is?
    setwd(file.path(working.directory,"Weather"))
    weather.list<-list.files(pattern="\\.csv$")
    if(length(weather.list)>0){
      weather<-read.csv(file = weather.list,header = T,stringsAsFactors = F)
      weather$Time<-as.POSIXct(weather$Time,tz=local.time.zone)
    } else{
      start.date<-as.Date(format(min(obs$Start.Date.Time.GMT),tz = local.time.zone))
      end.date<-as.Date(format(max(obs$Start.Date.Time.GMT),tz = local.time.zone))
      weather<-getWeatherForDate(station_id = weather.station.code, start_date = start.date, end_date = end.date, opt_detailed = T,opt_all_columns = T)
      write.csv(x = weather,file = file.path(working.directory,"Weather","weather.csv"),row.names = F)
    }
    weather$round<-as.POSIXct(round(as.numeric(weather$Time)/(60*60))*(60*60),origin=(as.POSIXlt('1970-01-01'))) ##will need this to match with weather data from ORD, rounds timestamp to nearest hour to match metar interval
    assign("weather",weather,.GlobalEnv)
  #Merge Weather with Observations (when possible)
    sub.weather<-subset(weather, round %in% obs$round) ##subset weather data.frame to contain only the metars that you need
    obs<-merge(obs,sub.weather, all.x=T) #this will duplicate bird observations when more than 1 METAR meets the criteria.
    obs$Diff.time<-difftime(obs$Start.Date.Time.local,obs$Time)
    mins<-aggregate(Diff.time ~ Record.Number, data = obs, FUN=min,na.action = na.pass)
    obs<-merge(mins,obs) #Select only the closest METAR in time
    obs <- obs[order(obs$Record.Number),]
    obs$breaks<-cut(obs$Start.Date.Time.GMT,breaks="day")
    
  #################### </OBSERVATION DATA> #####################
    
  
  #################### <RADAR DATA> #####################
  #Load Accipiter data (all of it)
    #ohare1
      setwd(file.path(working.directory,"CSV_to_Process","ARTI","ohare1"))
      ohare.1.filelist<-list.files(pattern="\\.csv$")
      datalist<-lapply(ohare.1.filelist,fread)
      ohare1<-rbindlist(datalist)
      ohare1$Radar<-"ohare1"
      assign("ohare.1.filelist",ohare.1.filelist,.GlobalEnv)
    #ohare2
      setwd(file.path(working.directory,"CSV_to_Process","ARTI","ohare2"))
      ohare.2.filelist<-list.files(pattern="\\.csv$")
      datalist<-lapply(ohare.2.filelist,fread)
      ohare2<-rbindlist(datalist)
      ohare2$Radar<-"ohare2"
      assign("ohare.2.filelist",ohare.2.filelist,.GlobalEnv)
    #ohare3
      setwd(file.path(working.directory,"CSV_to_Process","ARTI","ohare3"))
      ohare.3.filelist<-list.files(pattern="\\.csv$")
      datalist<-lapply(ohare.3.filelist,fread)
      ohare3<-rbindlist(datalist)
      ohare3$Radar<-"ohare3"
      assign("ohare.3.filelist",ohare.3.filelist,.GlobalEnv)
  #Cleanup a bit  
    data<-rbind(ohare1,ohare2,ohare3)
    rm(datalist,ohare1,ohare2,ohare3)
    arti.header<-unlist(read.csv(ohare.3.filelist[1],header=F,nrows=1,stringsAsFactors = F))
    setnames(data, c(arti.header),c("Update.Time", "Track.ID", "Start.Time..UTC.", "Latitude", "Longitude", "Speed..m.s.", "Heading..deg.N.", "Height...m.", "RCS..dBsm.", "Range.from.radar...m.", "Azimuth.from.radar..deg..", "Intensity", "UTM.Zone", "UTM.Northing", "UTM.Easting"))
  #Figure out which radars had data for each time period
    df.ohare.1.filelist<-data.frame(ohare.1.filelist,stringsAsFactors = F)
    df.ohare.1.filelist$Start.Date.Time.GMT<-as.POSIXct(strptime(x = ohare.1.filelist,format = "%Y%m%d_%H.%M.%S",tz="GMT"))
    df.ohare.2.filelist<-data.frame(ohare.2.filelist,stringsAsFactors = F)
    df.ohare.2.filelist$Start.Date.Time.GMT<-as.POSIXct(strptime(x = ohare.2.filelist,format = "%Y%m%d_%H.%M.%S",tz="GMT"))
    df.ohare.3.filelist<-data.frame(ohare.3.filelist,stringsAsFactors = F)
    df.ohare.3.filelist$Start.Date.Time.GMT<-as.POSIXct(strptime(x = ohare.3.filelist,format = "%Y%m%d_%H.%M.%S",tz="GMT"))
    obs<-merge(obs,df.ohare.1.filelist,all.x=T)
    obs<-merge(obs,df.ohare.2.filelist,all.x=T)
    obs<-merge(obs,df.ohare.3.filelist,all.x=T)
    assign("obs",obs,.GlobalEnv)
    write.csv(x = obs,file = file.path(working.directory,"Results","Complete.Observations.csv"))
    
  #Make R understand Accipiter data
    data$Update.Time<-as.POSIXct(data$Update.Time,format="%Y/%m/%d %H:%M:%S",tz="GMT") ##Time as time object with correct time zone
    data$long.ID<-as.factor(paste0(data$Track.ID,data$Start.Time..UTC.)) ##Add a unique ID
    data$Heading..deg.N.<-circular(data$Heading..deg.N.,type="angles",units="degrees",template="none",modulo="2pi",rotation="clock") ##Headings are circular data
    data$antenna.angle<-round(atan2(data$Height...m.,data$Range.from.radar...m.)*180/pi,digits=0) ##Calculate the antenna angle... why not?
  #Break up data by day for big datasets
    data$breaks<-cut(data$Update.Time,breaks="day")
    assign("data",data,.GlobalEnv)
  
  #Find the radar location from the data.
    temp<-read.csv(ohare.3.filelist[1],header=T,nrow=1,stringsAsFactors = F)
    temp$Heading..deg.N.<-circular(temp$Heading..deg.N.,type="angles",units="degrees",template="none",modulo="2pi",rotation="clock")
  #More Variables
    lat<- temp$Latitude[1]
    lon<-temp$Longitude[1]
    bearing<- temp$Azimuth.from.radar..deg.. +180
    bearing<-ifelse(bearing >= 360, bearing-360, bearing)
    distance<- temp$Range.from.radar...m.[1]/1000 #Distance from radar in km
    rad<-pi/180
  #Do some Math  
    a1<-lat*rad
    a2<-lon*rad
    tc<-bearing*rad
    d<- distance/6378.145
    nlat <- asin(sin(a1) * cos(d) + cos(a1) * sin(d) * cos(tc))
    dlon <- atan2(sin(tc) * sin(d) * cos(a1), cos(d) - sin(a1) * sin(nlat))
    nlon <- ((a2 + dlon + pi)%%(2 * pi)) - pi
    radar.location <- data.frame(lon = nlon/rad, lat = nlat/rad)
    radar.location$Radar<-"ohare1"
    radar.location$Description<-"Radar Location"
    r.2<-c(radar.location$lon[1],radar.location$lat[1],"ohare2","Radar Location")
    r.3<-c(radar.location$lon[1],radar.location$lat[1],"ohare3","Radar Location")
    radar.location<-rbind(radar.location,r.2,r.3)
    radar.location$lon<-as.numeric(radar.location$lon)
    radar.location$lat<-as.numeric(radar.location$lat)
    assign("radar.location",radar.location,.GlobalEnv)
  #################### </RADAR DATA> #####################
  
  #################### <Graphics> #####################    
    alpha<-c(1,1,1,1,1,.1)
    size<-c(1.5,1.5,1.5,1.5,1.5,.5)
    Description<-c("FOV Line","Observer Location","Observer Bird Estimate","Radar Location","Validated Update(s)","Other Update(s)")
    graphics<-data.frame(Description,alpha,size)
  #################### </Graphics> #####################
  
  #Show some progress
    pb<- txtProgressBar(min=0,max=nrow(obs),style=3)
    setTxtProgressBar(pb,0)  
  #Guess it's time to do some real work. Let's find some track matches... shall we?
  for(i in 1:nrow(obs)){
    assign("i",i,.GlobalEnv)
    what.the.hell<-i
    assign("What the hell?",what.the.hell,.GlobalEnv)
    #Coarse subset of accipiter data by date
      sub.data<-subset(data,as.character(breaks) == as.character(obs$breaks[i]))
    #Determine if observation was overhead. Do something different if it is. 
    if(obs$Dist..m[i] != 0){
      #Calculate range of acceptable bearings
        bearing.low <- ifelse(obs$Bearing[i] - bearing.allowance/2 < 0, 360 + (obs$Bearing[i] - bearing.allowance/2), obs$Bearing[i] - bearing.allowance/2)
        bearing.high <- ifelse(obs$Bearing[i] + bearing.allowance/2 > 360, (obs$Bearing[i] + bearing.allowance/2) - 360, obs$Bearing[i] + bearing.allowance/2)
      #Calculate bearing to track update from observer
        sub.data$bearing.to.target <- bearing(cbind(obs$obs.Longitude[i],obs$obs.Latitude[i]),cbind(sub.data$Longitude,sub.data$Latitude)) ##Calculate bearing to track update from observation point
        sub.data$bearing.to.target <- ifelse(sub.data$bearing.to.target < 0, 360 + sub.data$bearing.to.target, sub.data$bearing.to.target) ##Adjust so that no negative bearings are present
      #Calculate distance to track update from observer
        sub.data$dist.to.target <- distGeo(cbind(obs$obs.Longitude[i],obs$obs.Latitude[i]),cbind(sub.data$Longitude,sub.data$Latitude))
      #Subset track updates by time
        sub.time <- subset(sub.data, Update.Time >= obs$Start.Date.Time.GMT[i] - time.allowance & Update.Time <= obs$End.Date.Time.GMT[i] + time.allowance)
      #Subset track updates by bearing from observer
        sub.bearing <- subset(sub.time, bearing.to.target >= bearing.low & bearing.to.target <= bearing.high)
      #Subset track updates by distance from observer
        search.distance<-(search.distance.multiplier * obs$Dist..m[i]) + search.distance.constant
        sub.dist <- subset(sub.bearing, dist.to.target <= search.distance)
      #Which way was that winged wascal wandering?
      if(obs$Heading[i] == "N"){
        sub.heading <- subset(sub.dist, Heading..deg.N. >=315   | Heading..deg.N. <=45)
      } else if(obs$Heading[i] == "NNE") {
        sub.heading <- subset(sub.dist, Heading..deg.N. >=337.5 | Heading..deg.N. <=67.5)
      } else if(obs$Heading[i] == "NE")  {
        sub.heading <- subset(sub.dist, Heading..deg.N. <=90)
      } else if(obs$Heading[i] == "ENE") {
        sub.heading <- subset(sub.dist, Heading..deg.N. >=22.5  & Heading..deg.N. <=112.5)
      } else if(obs$Heading[i] == "E")   {
        sub.heading <- subset(sub.dist, Heading..deg.N. >=45    & Heading..deg.N. <=135)
      } else if(obs$Heading[i] == "ESE") {
        sub.heading <- subset(sub.dist, Heading..deg.N. >=67.5  & Heading..deg.N. <=157.5)
      } else if(obs$Heading[i] == "SE")  {
        sub.heading <- subset(sub.dist, Heading..deg.N. >=90    & Heading..deg.N. <=180)
      } else if(obs$Heading[i] == "SSE") {
        sub.heading <- subset(sub.dist, Heading..deg.N. >=112.5 & Heading..deg.N. <=202.5)
      } else if(obs$Heading[i] == "S")   {
        sub.heading <- subset(sub.dist, Heading..deg.N. >=135   & Heading..deg.N. <=225)
      } else if(obs$Heading[i] == "SSW") {
        sub.heading <- subset(sub.dist, Heading..deg.N. >=157.5 & Heading..deg.N. <=247.5)
      } else if(obs$Heading[i] == "SW")  {
        sub.heading <- subset(sub.dist, Heading..deg.N. >=180   & Heading..deg.N. <=270)
      } else if(obs$Heading[i] == "WSW") {
        sub.heading <- subset(sub.dist, Heading..deg.N. >=202.5 & Heading..deg.N. <=292.5)
      } else if(obs$Heading[i] == "W")   {
        sub.heading <- subset(sub.dist, Heading..deg.N. >=225   & Heading..deg.N. <=315)
      } else if(obs$Heading[i] == "WNW") {
        sub.heading <- subset(sub.dist, Heading..deg.N. >=247.5 & Heading..deg.N. <=337.5)
      } else if(obs$Heading[i] == "NW")  {
        sub.heading <- subset(sub.dist, Heading..deg.N. >=270)
      } else if(obs$Heading[i] == "NNW") {
        sub.heading <- subset(sub.dist, Heading..deg.N. >=292.5 | Heading..deg.N. <=22.5)
      } else{sub.heading <- sub.dist
      }
      #Anything left? Store it as this data.frame:    
        validate.list<-sub.heading
      
      #If validated track updates, store them as a CSV and map
        if(nrow(validate.list) > 0){
          validate.list$Obs.Time<-obs$Start.Date.Time.GMT[i] #Put the observation time into the validated set
          validate.list$Record.Number<-obs$Record.Number[i] #Put the record number into the validated set
          assign("validate.list",validate.list,.GlobalEnv)
          filename.time<-as.character(obs$Start.Date.Time.GMT[i],format="%Y.%m.%d_%H%M%S")
          graph.title.time<-as.character(obs$Start.Date.Time.GMT[i],format="%Y-%m-%d %H:%M:%S")
          csv.filename<-paste0("R_",obs$Record.Number[i],"_",filename.time,"_",bearing.allowance,"_FOV_",obs$Bearing[i],"_bearing_",obs$Heading[i],"_heading",".csv")
          write.csv(validate.list[with(validate.list, order(Track.ID, Update.Time)), ], file = file.path(working.directory,"Results","CSV",csv.filename),quote=FALSE, row.names = FALSE)
          
          #Extract full tracks. Pull all the updates associated with the matches
            full.tracks<-subset(data, long.ID %in% unique(validate.list$long.ID))
            assign("Full.Tracks",full.tracks,.GlobalEnv)
            full.tracks.filename<-paste0("R_",obs$Record.Number[i],"_",filename.time,"_",bearing.allowance,"_FOV_",obs$Bearing[i],"_bearing_",obs$Heading[i],"_heading","_FULL.csv")
            write.csv(x = full.tracks,file = file.path(working.directory,"Results","Full.Tracks",full.tracks.filename),quote = FALSE,row.names = FALSE)
          #For plotting heading information, create a dataset containin the maximum time interval for each individual track
            df.agg <- aggregate(Update.Time ~ long.ID, full.tracks, max)
            track.max.time.data <- merge(df.agg, full.tracks)  
          #Map Stuff
            ##FOV Lines data.frame
              fov.low<-data.frame(destPointRhumb(p = cbind(obs$obs.Longitude[i],obs$obs.Latitude[i]), b = bearing.low, d = search.distance))
              fov.low$Radar<-"ohare2"
              fov.low$Description<-"FOV Line"
              colnames(fov.low)<-c("Longitude","Latitude","Radar","Description")
              assign("fov.low",fov.low,.GlobalEnv)
              fov.high<-data.frame(destPointRhumb(p = cbind(obs$obs.Longitude[i],obs$obs.Latitude[i]), b = bearing.high, d = search.distance))
              fov.high$Radar<-"ohare3"
              fov.high$Description<-"FOV Line"
              colnames(fov.high)<-c("Longitude","Latitude","Radar","Description")
              assign("fov.high",fov.high,.GlobalEnv)
            ##Observer Location
              observer<-data.frame(obs$obs.Longitude[i],obs$obs.Latitude[i])
              observer$Radar<-"ohare1"
              observer$Description<-"Observer Location"
              o2<-c(obs$obs.Longitude[i],obs$obs.Latitude[i],"ohare2","Observer Location")
              o3<-c(obs$obs.Longitude[i],obs$obs.Latitude[i],"ohare3","Observer Location")
              observer<-rbind(observer,o2,o3)
              observer$obs.obs.Longitude.i.<-as.numeric(observer$obs.obs.Longitude.i.)
              observer$obs.obs.Latitude.i.<-as.numeric(observer$obs.obs.Latitude.i.)
              colnames(observer)<-c("Longitude","Latitude","Radar","Description")
            ##Observer Bird Estimate
              bird.estimate<-data.frame(destPointRhumb(p = cbind(obs$obs.Longitude[i],obs$obs.Latitude[i]), b = obs$Bearing[i], d = obs$Dist..m[i]))
              bird.estimate$Radar<-"ohare1"
              bird.estimate$Description<-"Observer Bird Estimate"
              b2<-c(bird.estimate$lon[1],bird.estimate$lat[1],"ohare2","Observer Bird Estimate")
              b3<-c(bird.estimate$lon[1],bird.estimate$lat[1],"ohare3","Observer Bird Estimate")
              bird.estimate<-rbind(bird.estimate,b2,b3)
              bird.estimate$lon<-as.numeric(bird.estimate$lon)
              bird.estimate$lat<-as.numeric(bird.estimate$lat)
              colnames(bird.estimate)<-c("Longitude","Latitude","Radar","Description")
            #Radar Location
              colnames(radar.location)<-c("Longitude","Latitude","Radar","Description")
            ##Longitude and Latitude of matches from ARTI
              validate.list$Description<-"Validated Update(s)"
            ##Longitude and Latitude of full tracks that match
              full.tracks$Description<-"Other Update(s)"
            ##Create a Dataframe containing all of the longitude/latitude info
              bounds<-rbind.fill(fov.low,fov.high,observer,bird.estimate,radar.location,validate.list,full.tracks)
          
            ##Download a map that fits the data
              map<-get_map(location = make_bbox(lon=Longitude,lat=Latitude,data=bounds,f=2), source="google",maptype = map.type)
            ##Get map attirbutes (corners)  
              bb<-attr(map,"bb")
              assign("bb",bb,.GlobalEnv)
            ##Use map attributes to create a scale bar
              sbar <- data.frame(lon.start = c(bb$ll.lon + 0.1*(bb$ur.lon - bb$ll.lon)),
                                 lon.end = c(bb$ll.lon + 0.25*(bb$ur.lon - bb$ll.lon)),
                                 lat.start = c(bb$ll.lat + 0.1*(bb$ur.lat - bb$ll.lat)),
                                 lat.end = c(bb$ll.lat + 0.1*(bb$ur.lat - bb$ll.lat)))
              sbar$distance = distGeo(p1 = c(sbar$lon.start,sbar$lat.start),p2 = c(sbar$lon.end,sbar$lat.end)) #Distance across downloaded map in meters
              ptspermm <- 2.83464567  # need this because geom_text uses mm, and themes use pts. Urgh.
            #Additional Track Info
              bounds$obs.dist.from.birds<- distGeo(cbind(obs$obs.Longitude[i],obs$obs.Latitude[i]),cbind(bounds$Longitude,bounds$Latitude)) #Observer distance from Birds
              bounds$Description<-factor(bounds$Description,levels = c("Radar Location","Observer Location","Observer Bird Estimate","Validated Update(s)","Other Update(s)","FOV Line"))
              bounds<-merge(bounds,graphics,all.x=T) #add in graphical parameters
              assign("bounds",bounds,.GlobalEnv)
              
            #Observer Inclination?   (needs work)
              #m.high<-tan((obs$INCL...deg.[i]+inclination.allowance/2)*pi/180)
              #m.low<-tan((obs$INCL...deg.[i]-inclination.allowance/2)*pi/180)
              #b<-0
          
            #Track Altitude (needs work. USDA study estimated altitude to target rather than recording inclination angle)
          #track.altitude.1<-ggplot(data=subset(bounds,Description != "FOV Line"),aes(x=obs.dist.from.birds,y=Height...m.,shape=Description,color=Description)) + geom_point() + scale_shape(solid=F)+ scale_colour_hue(l=40) + xlab("Observer Distance from Birds (m)") + ylab("Track Height (m)")
          #track.altitude.2<-track.altitude.1 + geom_abline(slope = ifelse(is.na(m.high)==TRUE,-1,m.high),intercept = b, alpha=0.9, linetype=4, color="blue")
          #track.altitude.3<-track.altitude.2 + geom_abline(slope = ifelse(is.na(m.low)==TRUE,-1,m.low),intercept = b, alpha=0.9, linetype=4, color="blue")
          #track.altitude.4<-track.altitude.3 + theme(legend.position = "none",axis.text=element_text(size=6),axis.title=element_text(size=6)) + scale_x_continuous(limits = c(0,max(bounds$obs.dist.from.birds +50)),expand = c(0,0)) + scale_y_continuous(limits = c(0,max(bounds$Height...m.,na.rm = T)+100)) + geom_hline(yintercept = 0,color="dark green",size = 3.5)
            #Track Speed
              #track.speed.1<-ggplot(data=subset(bounds,Description != "FOV Line"),aes(x=Update.Time,y=Speed..m.s.*2.23694,group=Track.ID,color=Description,shape=Description)) + geom_point() + geom_line() + scale_shape(solid=F)+ scale_colour_hue(l=40) + xlab("Update Time UTC") + ylab("Track Speed (mph)")
              #track.speed.2<-track.speed.1 + scale_x_datetime(labels=date_format("%H:%M:%S"),limits = c(as.POSIXct(ifelse(obs$Start.Date.Time.GMT[i]-time.allowance < min(validate.list$Update.Time)-1, obs$Start.Date.Time.GMT[i]-time.allowance-1,min(validate.list$Update.Time)-1),origin = "1970-01-01",tz = "UTC"),as.POSIXct(ifelse(obs$End.Date.Time.GMT[i]+time.allowance > max(validate.list$Update.Time)+1, obs$End.Date.Time.GMT[i]+time.allowance+1,max(validate.list$Update.Time)+1),origin = "1970-01-01",tz = "UTC")))
              #track.speed.3<-track.speed.2 + geom_vline(xintercept=as.numeric(obs$Start.Date.Time.GMT[i]-time.allowance),color="blue",linetype=4)
              #track.speed.4<-track.speed.3 + geom_vline(xintercept=as.numeric(obs$End.Date.Time.GMT[i]+time.allowance),color="blue",linetype=4)
              #track.speed.5<-track.speed.4 + theme(legend.position = "none",axis.text=element_text(size=6),axis.title=element_text(size=6))
            #Track RCS
              #track.rcs.1<-ggplot(data=subset(bounds,Description != "FOV Line"),aes(x=Update.Time,y=RCS..dBsm.,group=Track.ID,color=Description,shape=Description)) + geom_point() + geom_line() + scale_shape(solid=F)+ scale_colour_hue(l=40) + xlab("Update Time UTC") + ylab("Track RCS (dBsm)")
              #track.rcs.2<-track.rcs.1 + scale_x_datetime(labels=date_format("%H:%M:%S"),limits = c(as.POSIXct(ifelse(obs$Start.Date.Time.GMT[i]-time.allowance < min(validate.list$Update.Time)-1, obs$Start.Date.Time.GMT[i]-time.allowance-1,min(validate.list$Update.Time)-1),origin = "1970-01-01",tz = "UTC"),as.POSIXct(ifelse(obs$End.Date.Time.GMT[i]+time.allowance > max(validate.list$Update.Time)+1, obs$End.Date.Time.GMT[i]+time.allowance+1,max(validate.list$Update.Time)+1),origin = "1970-01-01",tz = "UTC")))
              #track.rcs.3<-track.rcs.2 + geom_vline(xintercept=as.numeric(obs$Start.Date.Time.GMT[i]-time.allowance),color="blue",linetype=4)
              #track.rcs.4<-track.rcs.3 + geom_vline(xintercept=as.numeric(obs$End.Date.Time.GMT[i]+time.allowance),color="blue",linetype=4)
              #track.rcs.5<-track.rcs.4 + theme(legend.position = "none",axis.text=element_text(size=6),axis.title=element_text(size=6))
            #Make a title for your graphic
              graph.title<-paste0("Chicago O'Hare International Airport\n","Observation Time: ",as.character(obs$Start.Date.Time.GMT[i])," UTC\n","Record Number: ",obs$Record.Number[i],"\n","Time Allowance: +/- ",time.allowance," sec\n","FOV: ",bearing.allowance," deg \n","Observation Bearing: ",obs$Bearing[i]," deg \n","Distance Estimate: ",obs$Dist..m[i]," m\n","Altitude Estimate: ",obs$Est.Alt..m[i]," m\n", obs$X.[i]," ",as.character(obs$X2013.Species.Code[i])," heading ",obs$Heading[i],"\n","Flight Characteristics: ",as.character(obs$Flight.Behavior[i]),"\nObservation Accuracy: ",as.character(obs$Accuracy[i]),"\n", "Notes: ",obs$Comments[i],"\nNOAA Precipitation: ",obs$PrecipitationIn[i]," in")
            #grob.title <- grobTree(textGrob(graph.title,hjust=0,vjust=1, gp=gpar(col="black", fontsize=6, fontface="bold",interpolate=T)))
              annotations <- data.frame(xpos = bb$ll.lon,ypos =  bb$ur.lat,annotateText = graph.title,hjustvar = 0 ,vjustvar = 1)
          #Actually plot the thing!!
            p<- ggmap(map,extent = "normal",maprange = F)
            q<- p + geom_point(data=subset(bounds,Description != "FOV Line"),aes(x=Longitude,y=Latitude,color=Description,shape=Description,size=size,alpha=alpha)) + scale_shape(solid=F) + scale_colour_hue(l=40) + scale_size_identity() + scale_alpha_identity()
            r<- q + geom_segment(aes(x=obs$obs.Longitude[i],y=obs$obs.Latitude[i],xend=fov.low$Longitude,yend=fov.low$Latitude),alpha=0.1,color="blue",linetype=4) #Add FOV Line
            s<- r + geom_segment(aes(x=obs$obs.Longitude[i],y=obs$obs.Latitude[i],xend=fov.high$Longitude,yend=fov.high$Latitude),alpha=0.1,color="blue",linetype=4) #Add FOV Line
            t<- s + geom_segment(data=sbar,aes(x=lon.start,xend=lon.end,y=lat.start,yend=lat.end))+geom_text(data = sbar, aes(x = (lon.start + lon.end)/2, y = lat.start + 0.025*(bb$ur.lat - bb$ll.lat),label = paste(format(distance*.000621371, digits = 4,nsmall = 2),'mi')),hjust = 0.5, vjust = 0, size = 8/ptspermm)  + coord_map(projection="mercator",xlim=c(bb$ll.lon, bb$ur.lon),ylim=c(bb$ll.lat, bb$ur.lat))+theme(plot.title = element_text(lineheight=.8, face="bold"))+theme(axis.text = element_text(size=8))#Add Scale Bar
            u<- t + geom_text(data=track.max.time.data,aes(x=Longitude,y=Latitude,label=Track.ID),size=2,hjust=1,position="identity")# + geom_text(data=validate.list,aes(x=Longitude,y=Latitude,label=Update.Time),size=2,hjust=0,position="identity")
            v<- u + geom_label(data = annotations, aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar, label=annotateText),size=1.5,alpha=0.5)
            w<- v + theme(legend.justification = c(.98,.05),legend.position = c(.98,.05),legend.background = element_rect(fill=alpha('grey', 0.6)),legend.text = element_text(size=4),legend.key = element_rect(colour = "black"),legend.title = element_text(face = "italic",size=6),axis.line=element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank(),axis.ticks=element_blank(),axis.title.x=element_blank(),axis.title.y=element_blank())       
            x<- w + facet_wrap(~Radar,strip.position = "left")
          #Cow Plot?
            #fig1<-plot_grid(track.altitude.4,track.speed.5,track.rcs.5,nrow=3,align="v")
            #fig2<-plot_grid(w,fig1,ncol=2,align="h")
          #Filename for Map
          #What to call it?  
            map.filename<-paste0("R_",obs$Record.Number[i],"_",filename.time,"_",bearing.allowance,"_FOV_",obs$Bearing[i],"_bearing_",obs$Heading[i],"_heading",".jpeg")
          #Save it  
            ggsave(filename = file.path(working.directory,"Results","Map",map.filename),plot = x, width= 12,height=4,pointsize=12,units="in" )
        }
      #Update the progress bar
      setTxtProgressBar(pb,i)
    } 
  ###Do something different for overhead    
    else{
      #Calculate distance to track update from observer
        sub.data$dist.to.target <- distGeo(cbind(obs$obs.Longitude[i],obs$obs.Latitude[i]),cbind(sub.data$Longitude,sub.data$Latitude))
      #Subset track updates by time
        sub.time <- subset(sub.data, Update.Time >= obs$Start.Date.Time.GMT[i] - time.allowance & Update.Time <= obs$End.Date.Time.GMT[i] + time.allowance)
      #Subset track updates by distance from observer. Must be less than or equal to overhead radius.
        overhead.radius<- (overhead.multiplier * obs$Est.Alt..m[i]) + overhead.radius.constant 
        sub.dist <- subset(sub.time, dist.to.target <= overhead.radius)
      #Which way was that winged wascal wandering?
      if(obs$Heading[i] == "N"){
        sub.heading <- subset(sub.dist, Heading..deg.N. >=315   | Heading..deg.N. <=45)
      } else if(obs$Heading[i] == "NNE") {
        sub.heading <- subset(sub.dist, Heading..deg.N. >=337.5 | Heading..deg.N. <=67.5)
      } else if(obs$Heading[i] == "NE")  {
        sub.heading <- subset(sub.dist, Heading..deg.N. <=90)
      } else if(obs$Heading[i] == "ENE") {
        sub.heading <- subset(sub.dist, Heading..deg.N. >=22.5  & Heading..deg.N. <=112.5)
      } else if(obs$Heading[i] == "E")   {
        sub.heading <- subset(sub.dist, Heading..deg.N. >=45    & Heading..deg.N. <=135)
      } else if(obs$Heading[i] == "ESE") {
        sub.heading <- subset(sub.dist, Heading..deg.N. >=67.5  & Heading..deg.N. <=157.5)
      } else if(obs$Heading[i] == "SE")  {
        sub.heading <- subset(sub.dist, Heading..deg.N. >=90    & Heading..deg.N. <=180)
      } else if(obs$Heading[i] == "SSE") {
        sub.heading <- subset(sub.dist, Heading..deg.N. >=112.5 & Heading..deg.N. <=202.5)
      } else if(obs$Heading[i] == "S")   {
        sub.heading <- subset(sub.dist, Heading..deg.N. >=135   & Heading..deg.N. <=225)
      } else if(obs$Heading[i] == "SSW") {
        sub.heading <- subset(sub.dist, Heading..deg.N. >=157.5 & Heading..deg.N. <=247.5)
      } else if(obs$Heading[i] == "SW")  {
        sub.heading <- subset(sub.dist, Heading..deg.N. >=180   & Heading..deg.N. <=270)
      } else if(obs$Heading[i] == "WSW") {
        sub.heading <- subset(sub.dist, Heading..deg.N. >=202.5 & Heading..deg.N. <=292.5)
      } else if(obs$Heading[i] == "W")   {
        sub.heading <- subset(sub.dist, Heading..deg.N. >=225   & Heading..deg.N. <=315)
      } else if(obs$Heading[i] == "WNW") {
        sub.heading <- subset(sub.dist, Heading..deg.N. >=247.5 & Heading..deg.N. <=337.5)
      } else if(obs$Heading[i] == "NW")  {
        sub.heading <- subset(sub.dist, Heading..deg.N. >=270)
      } else if(obs$Heading[i] == "NNW") {
        sub.heading <- subset(sub.dist, Heading..deg.N. >=292.5 | Heading..deg.N. <=22.5)
      } else{sub.heading <- sub.dist
      }
      #Anything left? Store it as this data.frame:    
        validate.list<-sub.heading
      
      #If validated track updates, store them as a CSV and map
      if(nrow(validate.list) > 0){
        validate.list$Obs.Time<-obs$Start.Date.Time.GMT[i] #Put the observation time into the validated set
        validate.list$Record.Number<-obs$Record.Number[i] #Put the record number into the validated set
        #validate.list$dup<-duplicated(validate.list$long.ID)
        #unique.validate.list <- subset(validate.list, dup == FALSE)
        assign("validate.list",validate.list,.GlobalEnv)
        #assign("unique.validate.list",unique.validate.list,.GlobalEnv)
        filename.time<-as.character(obs$Start.Date.Time.GMT[i],format="%Y.%m.%d_%H%M%S")
        graph.title.time<-as.character(obs$Start.Date.Time.GMT[i],format="%Y-%m-%d %H:%M:%S")
        csv.filename<-paste0("R_",obs$Record.Number[i],"_",filename.time,"_OH_",obs$Heading[i],"_heading",".csv")
        write.csv(validate.list[with(validate.list, order(Track.ID, Update.Time)), ], file = file.path(working.directory,"Results","CSV",csv.filename),quote=FALSE, row.names = FALSE)
        #write.csv(unique.validate.list[with(unique.validate.list, order(Track.ID, Update.Time)), ], file = file.path(working.directory,"Results","Unique.CSV",csv.filename),quote=FALSE, row.names = FALSE)
        
        #Extract full tracks. Pull all the updates associated with the matches
        full.tracks<-subset(data, long.ID %in% unique(validate.list$long.ID))
        assign("Full.Tracks",full.tracks,.GlobalEnv)
        full.tracks.filename<-paste0("R_",obs$Record.Number[i],"_",filename.time,"_OH_",obs$Heading[i],"_heading","_FULL.csv")
        write.csv(x = full.tracks,file = file.path(working.directory,"Results","Full.Tracks",full.tracks.filename),quote = FALSE,row.names = FALSE)
        #For plotting heading information, create a dataset containin the maximum time interval for each individual track
        df.agg <- aggregate(Update.Time ~ long.ID, full.tracks, max)
        track.max.time.data <- merge(df.agg, full.tracks)  
        #Map Stuff
        ##Make a circle representing the overhead radius as a data.frame
        r  <- overhead.radius
        tt <- seq(0,2*pi,length.out = npoints)
        xx <- obs$Easting[i] + r * cos(tt)
        yy <- obs$Northing[i] + r * sin(tt)
        #Transform to long/lat
        circ.utmcoor<- SpatialPoints(cbind(xx,yy), proj4string=CRS("+proj=utm +zone=16T")) ##Classify as Spatial Data. Mind the UTM Zone
        circ.longlatcoor<- spTransform(circ.utmcoor,CRS("+proj=longlat +datum=WGS84")) ##Convert to Latitue/Longitude
        circle<-data.frame(circ.longlatcoor$xx,circ.longlatcoor$yy)
        circle$Description <- "Overhead Radius"
        colnames(circle)<-c("Longitude","Latitude","Description")
        assign("circle",circle,.GlobalEnv)
        ##Observer Location
        observer<-data.frame(obs$obs.Longitude[i],obs$obs.Latitude[i])
        observer$Radar<-"ohare1"
        o2<-c(obs$obs.Longitude[i],obs$obs.Latitude[i],"ohare2","Transponder Position")
        o3<-c(obs$obs.Longitude[i],obs$obs.Latitude[i],"ohare3","Transponder Position")
        observer<-rbind(observer,o2,o3)
        observer$obs.obs.Longitude.i.<-as.numeric(observer$obs.obs.Longitude.i.)
        observer$obs.obs.Latitude.i.<-as.numeric(observer$obs.obs.Latitude.i.)
        observer$Description<-"Observer Location"
        colnames(observer)<-c("Longitude","Latitude","Radar","Description")
        ##Observer Bird Estimate
        bird.estimate<-data.frame(destPointRhumb(p = cbind(obs$obs.Longitude[i],obs$obs.Latitude[i]), b = obs$Bearing[i], d = obs$Dist..m[i]))
        bird.estimate$Radar<-"ohare1"
        bird.estimate$Description<-"Observer Bird Estimate"
        b2<-c(bird.estimate$lon[1],bird.estimate$lat[1],"ohare2","Observer Bird Estimate")
        b3<-c(bird.estimate$lon[1],bird.estimate$lat[1],"ohare3","Observer Bird Estimate")
        bird.estimate<-rbind(bird.estimate,b2,b3)
        bird.estimate$lon<-as.numeric(bird.estimate$lon)
        bird.estimate$lat<-as.numeric(bird.estimate$lat)
        colnames(bird.estimate)<-c("Longitude","Latitude","Radar","Description")
        #Radar Location
        colnames(radar.location)<-c("Longitude","Latitude","Radar","Description")
        ##Longitude and Latitude of matches from ARTI
        validate.list$Description<-"Validated Update(s)"
        ##Longitude and Latitude of full tracks that match
        full.tracks$Description<-"Other Update(s)"
        ##Create a Dataframe containing all of the longitude/latitude info
        bounds<-rbind.fill(circle,observer,radar.location,validate.list,full.tracks)
        
        ##Download a map that fits the data
        map<-get_map(location = make_bbox(lon=Longitude,lat=Latitude,data=bounds,f=2), source="google",maptype = map.type)
        ##Get map attirbutes (corners)  
        bb<-attr(map,"bb")
        assign("bb",bb,.GlobalEnv)
        ##Use map attributes to create a scale bar
        sbar <- data.frame(lon.start = c(bb$ll.lon + 0.1*(bb$ur.lon - bb$ll.lon)),
                           lon.end = c(bb$ll.lon + 0.25*(bb$ur.lon - bb$ll.lon)),
                           lat.start = c(bb$ll.lat + 0.1*(bb$ur.lat - bb$ll.lat)),
                           lat.end = c(bb$ll.lat + 0.1*(bb$ur.lat - bb$ll.lat)))
        sbar$distance = distGeo(p1 = c(sbar$lon.start,sbar$lat.start),p2 = c(sbar$lon.end,sbar$lat.end)) #Distance across downloaded map in meters
        ptspermm <- 2.83464567  # need this because geom_text uses mm, and themes use pts. Urgh.
        #Additional Track Info
        #Track within observation FOV in Inclination?
        #Track within observation FOV in Inclination?
        
        bounds$obs.dist.from.birds<- distGeo(cbind(obs$obs.Longitude[i],obs$obs.Latitude[i]),cbind(bounds$Longitude,bounds$Latitude)) #Observer mean latitude and longitude of birds
        bounds$Description<-factor(bounds$Description,levels = c("Radar Location","Observer Location","Observer Bird Estimate","Validated Update(s)","Other Update(s)","Overhead Radius"))
        bounds<-merge(bounds,graphics,all.x=T) #add in graphical parameters
        assign("bounds",bounds,.GlobalEnv)
        
        m.high<-tan((obs$INCL...deg.[i]+inclination.allowance/2)*pi/180)
        m.low<-tan((obs$INCL...deg.[i]-inclination.allowance/2)*pi/180)
        b<-0
        
        #Track Altitude
        #track.altitude.1<-ggplot(data=subset(bounds,Description != "FOV Line"),aes(x=obs.dist.from.birds,y=Height...m.,shape=Description,color=Description)) + geom_point() + scale_shape(solid=F)+ scale_colour_hue(l=40) + xlab("Observer Distance from Birds (m)") + ylab("Track Height (m)")
        #track.altitude.2<-track.altitude.1 + geom_abline(slope = ifelse(is.na(m.high)==TRUE,-1,m.high),intercept = b, alpha=0.9, linetype=4, color="blue")
        #track.altitude.3<-track.altitude.2 + geom_abline(slope = ifelse(is.na(m.low)==TRUE,-1,m.low),intercept = b, alpha=0.9, linetype=4, color="blue")
        #track.altitude.4<-track.altitude.3 + theme(legend.position = "none",axis.text=element_text(size=6),axis.title=element_text(size=6)) + scale_x_continuous(limits = c(0,max(bounds$obs.dist.from.birds +50)),expand = c(0,0)) + scale_y_continuous(limits = c(0,max(bounds$Height...m.,na.rm = T)+100)) + geom_hline(yintercept = 0,color="dark green",size = 3.5)
        
        #Track Speed
        #track.speed.1<-ggplot(data=subset(bounds,Description != "FOV Line"),aes(x=Update.Time,y=Speed..m.s.*2.23694,group=Track.ID,color=Description,shape=Description)) + geom_point() + geom_line() + scale_shape(solid=F)+ scale_colour_hue(l=40) + xlab("Update Time UTC") + ylab("Track Speed (mph)")
        #track.speed.2<-track.speed.1 + scale_x_datetime(labels=date_format("%H:%M:%S"),limits = c(as.POSIXct(ifelse(obs$Date.Time.UTC[i]-time.allowance < min(validate.list$Update.Time)-1, obs$Date.Time.UTC[i]-time.allowance-1,min(validate.list$Update.Time)-1),origin = "1970-01-01",tz = "UTC"),as.POSIXct(ifelse(obs$Date.Time.UTC[i]+time.allowance > max(validate.list$Update.Time)+1, obs$Date.Time.UTC[i]+time.allowance+1,max(validate.list$Update.Time)+1),origin = "1970-01-01",tz = "UTC")))
        #track.speed.3<-track.speed.2 + geom_vline(xintercept=as.numeric(obs$Date.Time.UTC[i]-time.allowance),color="blue",linetype=4)
        #track.speed.4<-track.speed.3 + geom_vline(xintercept=as.numeric(obs$Date.Time.UTC[i]+time.allowance),color="blue",linetype=4)
        #track.speed.5<-track.speed.4 + theme(legend.position = "none",axis.text=element_text(size=6),axis.title=element_text(size=6))
        #Track RCS
        #track.rcs.1<-ggplot(data=subset(bounds,Description != "FOV Line"),aes(x=Update.Time,y=RCS..dBsm.,group=Track.ID,color=Description,shape=Description)) + geom_point() + geom_line() + scale_shape(solid=F)+ scale_colour_hue(l=40) + xlab("Update Time UTC") + ylab("Track RCS (dBsm)")
        #track.rcs.2<-track.rcs.1 + scale_x_datetime(labels=date_format("%H:%M:%S"),limits = c(as.POSIXct(ifelse(obs$Date.Time.UTC[i]-time.allowance < min(validate.list$Update.Time)-1, obs$Date.Time.UTC[i]-time.allowance-1,min(validate.list$Update.Time)-1),origin = "1970-01-01",tz = "UTC"),as.POSIXct(ifelse(obs$Date.Time.UTC[i]+time.allowance > max(validate.list$Update.Time)+1, obs$Date.Time.UTC[i]+time.allowance+1,max(validate.list$Update.Time)+1),origin = "1970-01-01",tz = "UTC")))
        #track.rcs.3<-track.rcs.2 + geom_vline(xintercept=as.numeric(obs$Date.Time.UTC[i]-time.allowance),color="blue",linetype=4)
        #track.rcs.4<-track.rcs.3 + geom_vline(xintercept=as.numeric(obs$Date.Time.UTC[i]+time.allowance),color="blue",linetype=4)
        #track.rcs.5<-track.rcs.4 + theme(legend.position = "none",axis.text=element_text(size=6),axis.title=element_text(size=6))
        #Make a title for your graphic
        graph.title<-paste0("Chicago O'Hare International Airport\n","Observation Time: ",as.character(obs$Start.Date.Time.GMT[i])," UTC\n","Record Number: ",obs$Record.Number[i],"\n","Time Allowance: +/- ",time.allowance," sec\n","Overhead Radius: ",overhead.radius," m\n","Distance Estimate: ",obs$Dist..m[i]," m\n","Altitude Estimate: ",obs$Est.Alt..m[i]," m\n", obs$X.[i]," ",as.character(obs$X2013.Species.Code[i])," heading ",obs$Heading[i],"\n","Flight Characteristics: ",as.character(obs$Flight.Behavior[i]),"\nObservation Accuracy: ",as.character(obs$Accuracy[i]),"\n", "Notes: ",obs$Comments[i],"\nNOAA Precipitation: ",obs$PrecipitationIn[i]," in")
        #grob.title <- grobTree(textGrob(graph.title,hjust=0,vjust=1, gp=gpar(col="black", fontsize=6, fontface="bold",interpolate=T)))
        annotations <- data.frame(xpos = bb$ll.lon,ypos =  bb$ur.lat,annotateText = graph.title,hjustvar = 0 ,vjustvar = 1)
        #Actually plot the thing
        
        p<- ggmap(map,extent = "normal",maprange = F)
        q<- p + geom_point(data=subset(bounds,Description != "Overhead Radius"),aes(x=Longitude,y=Latitude,color=Description,shape=Description,size=size,alpha=alpha)) + scale_shape(solid=F) + scale_colour_hue(l=40) + scale_size_identity() + scale_alpha_identity()
        r<- q + geom_path(data = circle, aes(x=Longitude,y=Latitude),alpha=0.5,color="blue",linetype=4)
        t<- r + geom_segment(data=sbar,aes(x=lon.start,xend=lon.end,y=lat.start,yend=lat.end))+geom_text(data = sbar, aes(x = (lon.start + lon.end)/2, y = lat.start + 0.025*(bb$ur.lat - bb$ll.lat),label = paste(format(distance*.000621371, digits = 4,nsmall = 2),'mi')),hjust = 0.5, vjust = 0, size = 8/ptspermm)  + coord_map(projection="mercator",xlim=c(bb$ll.lon, bb$ur.lon),ylim=c(bb$ll.lat, bb$ur.lat))+theme(plot.title = element_text(lineheight=.8, face="bold"))+theme(axis.text = element_text(size=8))#Add Scale Bar
        u<- t + geom_text(data=track.max.time.data,aes(x=Longitude,y=Latitude,label=Track.ID),size=2,hjust=1,position="identity")# + geom_text(data=validate.list,aes(x=Longitude,y=Latitude,label=Update.Time),size=2,hjust=0,position="identity")
        v<- u + geom_label(data = annotations, aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar, label=annotateText),size=2,alpha=0.5)
        w<- v + theme(legend.justification = c(.98,.05),legend.position = c(.98,.05),legend.background = element_rect(fill=alpha('grey', 0.6)),legend.text = element_text(size=4),legend.key = element_rect(colour = "black"),legend.title = element_text(face = "italic",size=6),axis.line=element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank(),axis.ticks=element_blank(),axis.title.x=element_blank(),axis.title.y=element_blank())       
        x<- w + facet_wrap(~Radar,strip.position = "left")
        #Cow Plot?
        #fig1<-plot_grid(track.altitude.4,track.speed.5,track.rcs.5,nrow=3,align="v")
        #fig2<-plot_grid(w,fig1,ncol=2,align="h")
        #Filename for Map
        #What to call it?  
        map.filename<-paste0("R_",obs$Record.Number[i],"_",filename.time,"_OH_",obs$Heading[i],"_heading",".jpeg")
        #Save it  
        ggsave(filename = file.path(working.directory,"Results","Map",map.filename),plot = x, width= 12,height=4,pointsize=12,units="in" )
      }
      #Update the progress bar
      setTxtProgressBar(pb,i)
    }  
  }
}

