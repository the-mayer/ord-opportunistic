#Format for entering variables:
#time:"3/13/2013 7:15:00 PM" -> no leading zeros on month, no leading zeros on time, AM/PM required, must " "
#time.interval: value in seconds, eg, 5 seconds will look plus or minus 5 seconds from time entered (10 second total interval)
#fov: the field of view of the optical equipment used to make the observation
#target.heading: the direction the bird was traveling in. Valid cardinal directions are N,NNE,NE,ENE,E,ESE,SE,SSE,S,SSW,SW,WSW,W,WNW,NW,NNW or RA (random). Entry must be in " ".
#obs.inclination: the inclination to the bird from the observer
#obs.bearing: the bearing from the observer to the bird. If the bird is overhead enter "OH".
#obs.northing: the UTM Northing coordinate of the observation location.
#obs.easting: the UTM Easting coordinate coordinate of the observation location.
#obs.elevation: the elevation of the observation location. Determined using Google earth.
#csv:"filename.csv" ->must include ""


validate <- function(time,time.interval, fov, target.heading, obs.inclination, obs.bearing, obs.northing,obs.easting,obs.elevation, csv) {
  
#Set Working Directory(where are the TrackPoints csv files?)
  working.directory <- getwd()
#assign("working.directory",working.directory,.GlobalEnv)
  setwd(working.directory)
#Set BSTAR particulars
  bstar.elev <- 173.126
  BSTAR1.Northing <- 3643856
  BSTAR1.Easting <- 684012
  obs.cartesianX <- obs.easting - BSTAR1.Easting
  obs.cartesianY <- obs.northing - BSTAR1.Northing
#Read TrackPoints csv file to "data" variable
  data <- read.csv(csv, header=TRUE, sep=",", col.names=c("Id","TimeStamp","Longitude..degrees.","Latitude..degrees.","Altitude..m.","Velocity..m.s.","Heading..degrees.","Track","Radar","RadarTrackNumber","DiscriminationType","Range.m.","Azimuth.degrees.","Elevation..degrees.","VelocityX..m.s.", "VelocityY..m.s.","VelocityZ..m.s.","TrackQuality","Biomass","ClassificationConfidence","PositionCartesianX..m.","PositionCartesianY..m.","PositionCartesianZ..m."),colClasses=c("integer","character","numeric","numeric","numeric","numeric","numeric","integer","integer","integer","character","numeric","numeric","numeric","numeric","numeric","numeric","integer","numeric","integer","numeric","numeric","numeric"),stringsAsFactors = FALSE)
  #assign("data",data,.GlobalEnv)
  #Convert "time" variable from character to numeric to allow for math to define interval
  var.time0 <- strptime(time, "%m/%d/%Y %I:%M:%S %p")
  #assign("var.time0",var.time0,.GlobalEnv)
#Create an output directory
  if(var.time0 < strptime(paste0(as.character(var.time0,"%m/%d/%Y"), " 12:00:00"),"%m/%d/%Y %H:%M:%S")){
    obs.session <- "DAWN"
  }
  else if(var.time0 >= strptime(paste0(as.character(var.time0,"%m/%d/%Y"), " 12:00:00"),"%m/%d/%Y %H:%M:%S") & var.time0 < strptime(paste0(as.character(var.time0,"%m/%d/%Y"), " 16:00:00"),"%m/%d/%Y %H:%M:%S")){
    obs.session <- "MIDDAY"
  }
  else if(var.time0 >= strptime(paste0(as.character(var.time0,"%m/%d/%Y"), " 16:00:00"),"%m/%d/%Y %H:%M:%S")){
    obs.session <- "DUSK"
  }
  sub.Dir.name <- paste0("R_", as.character(var.time0, "%m.%d.%Y"),"_",obs.session,"_Validated")
  subDir <- sub.Dir.name
  dir.create(file.path(working.directory, subDir), showWarnings = FALSE)
#Define interval, plus or minus how many seconds
  var.time1 <- var.time0-time.interval
  var.time2 <- var.time0+time.interval
#Convert time interval back to character for comparing with TrackPoints csv
#Windows systems and Unix systems handle time formatting differetnly. Defined cases for UNIX(if) and Windows(else)
  if(.Platform$OS.type == "unix") {
  sub.time1 <- as.character(var.time1, "%-m/%e/%Y %-I:%M:%S %p")
  sub.time2 <- as.character(var.time2, "%-m/%e/%Y %-I:%M:%S %p")
  } else {
    sub.time1 <- as.character(var.time1, "%#m/%#d/%Y %#I:%M:%S %p")
    sub.time2 <- as.character(var.time2, "%#m/%#d/%Y %#I:%M:%S %p")
  }
#Subset data based on time.interval. If you also want to subset by discrimination type, add this to the next line: & DiscriminationType=="Bird"
  sub.time <- subset(data, strptime(TimeStamp, "%m/%d/%Y %I:%M:%S %p") >= strptime(sub.time1,  "%m/%d/%Y %I:%M:%S %p") & strptime(TimeStamp, "%m/%d/%Y %I:%M:%S %p") <= strptime(sub.time2,  "%m/%d/%Y %I:%M:%S %p"))
  #assign("sub.time", sub.time,.GlobalEnv)
#Calculate bearing for each TrackPoint based on Observation Point
  bearing.to.target <-ifelse((sub.time$PositionCartesianX..m. - obs.cartesianX >=0 & sub.time$PositionCartesianY..m. -obs.cartesianY >=0),(atan(((sub.time$PositionCartesianX..m. - obs.cartesianX)/(sub.time$PositionCartesianY..m. - obs.cartesianY))))/pi*180,ifelse(sub.time$PositionCartesianY..m. -obs.cartesianY <0,(atan(((sub.time$PositionCartesianX..m. - obs.cartesianX)/(sub.time$PositionCartesianY..m. - obs.cartesianY))))/pi*180+180,(atan(((sub.time$PositionCartesianX..m. - obs.cartesianX)/(sub.time$PositionCartesianY..m. - obs.cartesianY))))/pi*180 + 360))
#Add calculated bearing to sub.time dataframe
  sub.time$obs.bearing.to.target <- bearing.to.target
#Calculate distance to each TrackPoint based on Observation Point
  distance <- sqrt((obs.cartesianX-sub.time$PositionCartesianX..m.)^2 + (obs.cartesianY-sub.time$PositionCartesianY..m.)^2)
#Add calculated distance to trackpoint to sub.time dataframe
  sub.time$obs.distance.to.target <- distance
  #assign("distance",distance,.GlobalEnv)
#Calculate inclination to each TrackPoint based on Observation Point
  inclination.from.observer <- atan((sub.time$PositionCartesianZ..m.+(bstar.elev-obs.elevation))/distance)/(pi/180)
  #assign("inclination.from.observer",inclination.from.observer,.GlobalEnv)
#Add calculated inclination to trackpoint to sub.time dataframe
  sub.time$obs.inclination.to.target <- inclination.from.observer
  #assign("sub.time", sub.time, .GlobalEnv)
  time.difference <- strptime(sub.time$TimeStamp, "%m/%d/%Y %I:%M:%S %p")- var.time0
#add time difference to sub.time dataframe
  sub.time$time.difference <- time.difference
#Validation Number Calculation
  time.diff.0 <- sub.time$time.difference == 0 
  sub.time$time.diff.0 <- time.diff.0
  
  time.diff.within.2 <- sub.time$time.difference >=-2 & sub.time$time.difference <=2
  sub.time$time.diff.within.2 <- time.diff.within.2
  
  time.diff.within.3 <- sub.time$time.difference >=-3 & sub.time$time.difference <=3
  sub.time$time.diff.within.3 <- time.diff.within.3
  
  within.incline.fov <- sub.time$obs.inclination.to.target >= obs.inclination-(fov/2) & sub.time$obs.inclination.to.target <= obs.inclination+(fov/2)
  sub.time$within.incline.fov <- within.incline.fov
  
  within.incline.fov.2x <- sub.time$obs.inclination.to.target >= obs.inclination-(fov) & sub.time$obs.inclination.to.target <= obs.inclination+(fov)
  sub.time$within.incline.fov.2x <- within.incline.fov.2x
  
  validation.number <- time.diff.0 + time.diff.within.2 +  time.diff.within.3 + within.incline.fov + within.incline.fov.2x
  sub.time$validation.number <- validation.number
  
#Subset data based on target.heading
  if(target.heading == "N" | target.heading == "n"){
    sub.heading <- subset(sub.time, Heading..degrees. >=315 | Heading..degrees. <=45)
  }
    else if(target.heading == "NNE" | target.heading == "nne") {
      sub.heading <- subset(sub.time, Heading..degrees. >=337.5 | Heading..degrees. <=67.5)
    }
    else if(target.heading == "NE" | target.heading == "ne") {
      sub.heading <- subset(sub.time, Heading..degrees. <=90)
    }
    else if(target.heading == "ENE" | target.heading == "ene") {
      sub.heading <- subset(sub.time, Heading..degrees. >=22.5 & Heading..degrees. <=112.5)
    }
    else if(target.heading == "E" | target.heading == "e") {
      sub.heading <- subset(sub.time, Heading..degrees. >=45 & Heading..degrees. <=135)
    }
    else if(target.heading == "ESE" | target.heading == "ese") {
    sub.heading <- subset(sub.time, Heading..degrees. >=67.5 & Heading..degrees. <=157.5)
    }
    else if(target.heading == "SE" | target.heading == "se") {
    sub.heading <- subset(sub.time, Heading..degrees. >=90 & Heading..degrees. <=180)
    }
    else if(target.heading == "SSE" | target.heading == "sse") {
      sub.heading <- subset(sub.time, Heading..degrees. >=112.5 & Heading..degrees. <=202.5)
    }
    else if(target.heading == "S" | target.heading == "s") {
      sub.heading <- subset(sub.time, Heading..degrees. >=135 & Heading..degrees. <=225)
    }
    else if(target.heading == "SSW" | target.heading == "ssw") {
      sub.heading <- subset(sub.time, Heading..degrees. >=157.5 & Heading..degrees. <=247.5)
    }
    else if(target.heading == "SW" | target.heading == "sw") {
      sub.heading <- subset(sub.time, Heading..degrees. >=180 & Heading..degrees. <=270)
    }
    else if(target.heading == "WSW" | target.heading == "wsw") {
      sub.heading <- subset(sub.time, Heading..degrees. >=202.5 & Heading..degrees. <=292.5)
    }
    else if(target.heading == "W" | target.heading == "w") {
      sub.heading <- subset(sub.time, Heading..degrees. >=225 & Heading..degrees. <=315)
    }
    else if(target.heading == "WNW" | target.heading == "wnw") {
      sub.heading <- subset(sub.time, Heading..degrees. >=247.5 & Heading..degrees. <=337.5)
    }
    else if(target.heading == "NW" | target.heading == "nw") {
      sub.heading <- subset(sub.time, Heading..degrees. >=270)
    }
    else if(target.heading == "NNW" | target.heading == "nnw") {
      sub.heading <- subset(sub.time, Heading..degrees. >=292.5 | Heading..degrees. <=22.5)
    }
    else(sub.heading <- sub.time)
  #assign("sub.heading", sub.heading, .GlobalEnv)
#Determine if Observation is Overhead based on obs.bearing variable and do something different if it is
  if(obs.bearing == "OH" | obs.bearing == "oh" | obs.inclination == 90) {
    sub.bearing <- subset(sub.heading, obs.inclination.to.target >= 40)
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
    sub.bearing <- subset(sub.heading, obs.bearing.to.target >= low.bearing.1 & obs.bearing.to.target <= high.bearing.1)
  } else {
    sub.bearing <- subset(sub.heading, obs.bearing.to.target >= low.bearing.1 | obs.bearing.to.target <= high.bearing.1)
  }
  }
#Order the data by Ascending RadarTrackNumber and then by TimeStamp. Print result.
  validate.list <- sub.bearing[with(sub.bearing, order(RadarTrackNumber, TimeStamp)), ]
  #print(validate.list)
#Process Results
  library(calibrate)
  unique.track.results <- unique(validate.list$Track)
  other.positions <- subset(data, Track %in% unique.track.results)
  #assign("other.positions",other.positions,.GlobalEnv)

#Output list of confirmed tracks to the Global Environment
  assign("validate.list",sub.bearing[with(sub.bearing, order(RadarTrackNumber, TimeStamp)), ],.GlobalEnv)
#Create a title and subtitle for the graph based on previous user input
  title <- paste0("TrackPoints ", obs.bearing,"° from Observer with ",fov, "° FOV ","\nfrom ",obs.cartesianY+BSTAR1.Northing," N ",obs.cartesianX+BSTAR1.Easting," E"," and Heading ",target.heading)
  sub.title <- paste0(sub.time1," - ", sub.time2)
#Plot subsetted data, observation point, BSTAR position
  filename.time <- as.character(var.time0, "%m.%d.%Y_%H%M%S")
  plot(other.positions$PositionCartesianX..m.,other.positions$PositionCartesianY..m.,xlim=c(-10500,10500),main=title,sub=sub.title,ylim=c(-10500,10500),col="light grey",xlab="Cartesian X Position (m)",ylab="Cartesian Y Position (m)")
  par(new=T)
  plot(validate.list$PositionCartesianX..m.,validate.list$PositionCartesianY..m.,xlim=c(-10500,10500),ylim=c(-10500,10500),xlab="",ylab="",axes=F,col="dark red")
  par(new=T)
  plot(obs.cartesianX,obs.cartesianY,xlim=c(-10500,10500),ylim=c(-10500,10500),pch=18,xlab="",ylab="",axes=F,col="blue")
  par(new=T)
  plot(0,0,xlim=c(-10500,10500),ylim=c(-10500,10500),pch=17, xlab="",ylab="",axes=F,col="black")
  par(new=F)
#Add FOV lines to the plot, if not an Overhead observation
  if(class(obs.bearing)=="numeric"){
#Based on the field of view, calculate the lower fov line  
  low.bearing.2 <- low.bearing.1+90
  slope.low <- -1*tan(low.bearing.2*(pi/180))
  intercept.low <- ((-1* slope.low)*obs.cartesianX)+obs.cartesianY
  #assign("slope.low",slope.low,.GlobalEnv)

  #Based on the field of view, calculate the upper fov line
  high.bearing.2 <- high.bearing.1+90
  slope.high <- -1*tan((high.bearing.2*(pi/180)))
  intercept.high <- ((-1*slope.high)*obs.cartesianX)+obs.cartesianY
  #assign("slope.high",slope.high,.GlobalEnv)     
  
  abline(intercept.low,slope.low,col="darkcyan")
  abline(intercept.high,slope.high,col="darkcyan")
  }
#Label the plot
  textxy(0,0,labs="BSTAR",cx=1)
  textxy(obs.cartesianX,obs.cartesianY,labs="Observer",cx=1)
  if(nrow(validate.list)>=1){
  unique.track.results.df <- data.frame(unique.track.results)
  #assign("unique.track.results.df",unique.track.results.df,.GlobalEnv)
  sub.track <- data.frame()
  test <- data.frame()
  for(x in 1:nrow(unique.track.results.df)){
  unique.trackID <- unique.track.results.df[x,]
  unique.track.positions <- subset(other.positions, Track == unique.trackID)
  validated.track.positions <- subset(validate.list, Track == unique.trackID)
  max.track.time <- max(strptime(unique.track.positions$TimeStamp, "%m/%d/%Y %I:%M:%S %p"))
  max.validation.number <- max(validated.track.positions$validation.number)
  max.unique.track.time <- subset(unique.track.positions, strptime(TimeStamp, "%m/%d/%Y %I:%M:%S %p") == max.track.time)
  max.unique.track.time$validation.number <- max.validation.number
  if(max.validation.number >=0 & max.validation.number <=1){color <- "black"}
  else if(max.validation.number >=2 & max.validation.number <=3){color <- "darkgoldenrod4"}
  else if(max.validation.number >=4 & max.validation.number <=5){color <- "darkgreen"}
  max.unique.track.time$color <- color
  sub.track <- rbind(sub.track, max.unique.track.time)
  }
  textxy(sub.track$PositionCartesianX..m.,sub.track$PositionCartesianY..m.,labs=sub.track$RadarTrackNumber,dcol=sub.track$color,cx=1)
  #assign("sub.track",sub.track,.GlobalEnv)
}
  if(nrow(validate.list) >= 1){
  graph.filename <- paste0("R_", filename.time,"_",obs.cartesianY+BSTAR1.Northing,"N_",obs.cartesianX+BSTAR1.Easting,"E_",fov,"FOV_",obs.bearing,"bearing_",target.heading,"_heading",".png")
  dev.copy(png, filename = file.path(working.directory,subDir,graph.filename), width = 1024, height = 1024)
  dev.off()
  }
#Write a csv file containing the filtered TrackPoints
  if(nrow(validate.list)>=1){
  filename.time <- as.character(var.time0, "%m.%d.%Y_%H%M%S")
  csv.filename <- paste0("R_", filename.time,"_",obs.cartesianY+BSTAR1.Northing,"N_",obs.cartesianX+BSTAR1.Easting,"E_",fov,"FOV_",obs.bearing,"bearing_",target.heading,"_heading",".csv")
  write.csv(validate.list[with(validate.list, order(RadarTrackNumber, TimeStamp)), ], file = file.path(working.directory,subDir,csv.filename),quote=FALSE, row.names = FALSE)
  }
  else{
    filename.time <- as.character(var.time0, "%m.%d.%Y_%H%M%S")
    csv.filename <- paste0("R_", filename.time,"_",obs.cartesianY+BSTAR1.Northing,"N_",obs.cartesianX+BSTAR1.Easting,"E_",fov,"FOV_",obs.bearing,"bearing_",target.heading,"_heading","_EMPTY.csv")  
    no.match <- "No tracks matching observation, What a bummer!"
    write.csv(no.match, file = file.path(working.directory,subDir,csv.filename),quote=FALSE, row.names = FALSE)
  }
}
