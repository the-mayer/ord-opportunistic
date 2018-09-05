#Format for entering variables:
  #start.time:"08/16/2012 00:20:53 GMT" ->  must " "
  #end.time: "08/16/2012 00:20:58 GMT" ->  must " "
  #fov: the field of view of the optical equipment used to make the observation
  #target.heading: the direction the bird was traveling in. Valid cardinal directions are N,NNE,NE,ENE,E,ESE,SE,SSE,S,SSW,SW,WSW,W,WNW,NW,NNW or RA (random). Entry must be in " ".
  #obs.bearing: the bearing from the observer to the bird. If the bird is overhead enter "OH".
  #UTM.Northing: the Y coordinate of the observation location. Found in the spreadsheet.
  #UTM.Easting: the x coordinate of the observation location. Found in the spreadsheet.
  #csv:"filename.csv" ->must include ""


validate <- function(start.time, end.time, fov, target.heading, obs.bearing, obs.UTM.Northing, obs.UTM.Easting, obs.dist.estimate.yards,obs.alt.estimate.ft, weather, radar, csv) {
  
#Set Working Directory(where are the TrackPoints csv files?)
  working.directory <- getwd()
#assign("working.directory",working.directory,.GlobalEnv)
  setwd(working.directory)
#Create an output directory
  subDir <- "Validated"
  dir.create(file.path(working.directory, subDir), showWarnings = FALSE)
#Set Radar Location 
  radar.Northing <- 4649444.98
  radar.Easting <- 422641.5
  if(radar == "AR1"){
    radar.angle <- 2
  }
  else if(radar == "AR2-1"){
    radar.angle <- 4
  }
  else{
    radar.angle <- 8
  }
  if(weather == "Rain" | weather == "Lt. Rain" | weather == "Moderate Rain" | weather == "Hvy. Rain"){
    filename.time <- as.character(strptime(start.time, "%m/%d/%Y %H:%M:%S"), "%Y.%m.%d_%H%M%S")
    csv.filename <- paste0("R_", filename.time,"_",obs.UTM.Northing,"N_",obs.UTM.Easting,"E_",fov,"FOV_",obs.bearing,"bearing_",target.heading,"_heading","_RAIN.csv")
    rain <- "nothing to see here, it was raining"
    write.csv(rain, file = file.path(working.directory,subDir,csv.filename),quote=FALSE, row.names = FALSE)
  }
  else{
  if(file.exists(csv)== TRUE){
#Read TrackPoints csv file to "data" variable
  data <- read.csv(csv, header=TRUE, sep=",",stringsAsFactors = FALSE)
  
#Add UNIX update time to data frame
  UNIX.Update.Time <- as.numeric(as.POSIXlt(data$Update.Time, origin="1970-01-01", tz="GMT"))
  data$UNIX.Update.Time <- UNIX.Update.Time
    assign("data",data,.GlobalEnv)
#Convert user input to UNIX time
  start.time0 <- as.numeric(as.POSIXlt(start.time, "%m/%d/%Y %H:%M:%S", origin="1970-01-01", tz="GMT"))
    assign("start.time0",start.time0,.GlobalEnv)
  end.time0 <- as.numeric(as.POSIXlt(end.time, "%m/%d/%Y %H:%M:%S", origin="1970-01-01", tz="GMT"))
    assign("end.time0",end.time0,.GlobalEnv)
  
#Subset data based on time.interval. 
  sub.time <- subset(data, UNIX.Update.Time >= start.time0 & UNIX.Update.Time <= end.time0)
  assign("sub.time", sub.time,.GlobalEnv)
#Calculate bearing for each TrackPoint based on Observation Point
  bearing.to.target <-ifelse((sub.time$UTM.Easting - obs.UTM.Easting >=0 & sub.time$UTM.Northing -obs.UTM.Northing >=0),(atan(((sub.time$UTM.Easting - obs.UTM.Easting)/(sub.time$UTM.Northing - obs.UTM.Northing))))/pi*180,ifelse(sub.time$UTM.Northing -obs.UTM.Northing <0,(atan(((sub.time$UTM.Easting - obs.UTM.Easting)/(sub.time$UTM.Northing - obs.UTM.Northing))))/pi*180+180,(atan(((sub.time$UTM.Easting - obs.UTM.Easting)/(sub.time$UTM.Northing - obs.UTM.Northing))))/pi*180 + 360))
#Add calculated bearing to sub.time dataframe
  sub.time$obs.bearing.to.target <- bearing.to.target
#Calculate distance to each TrackPoint based on Observation Point (m)
  distance <- sqrt((obs.UTM.Easting-sub.time$UTM.Easting)^2 + (obs.UTM.Northing-sub.time$UTM.Northing)^2)
#Calculate distance to each TrackPoint based on Observation Point (yards)  
  distance.yards <- distance * 1.09361
  sub.time$obs.distance.to.target.m <- distance
#Add calculated distance to trackpoint to sub.time dataframe
  sub.time$obs.distance.to.target.yards <- distance.yards
#Calculate difference between observer distance estimate and calculated value
  distance.difference <- distance.yards - obs.dist.estimate.yards
#Add calculated distance difference to trackpoint to sub.time dataframe  
  sub.time$diff.btwn.obs.estimate.and.actual.yards <- distance.difference
#Calculate difference between observer altitude estimate and calculated value
  altitude.difference <- (sub.time$Height...m. * 3.28084) - obs.alt.estimate.ft
#Calculate beam dimensions at track update
  low.beam.height.m <- tan((radar.angle-2)*(pi/180))*distance
  low.beam.height.ft <- low.beam.height.m * 3.28084
  high.beam.height.m <- tan((radar.angle+2)*(pi/180))*distance
  high.beam.height.ft <-high.beam.height.m * 3.28084
#Add calculated beam dimensions to sub.time dataframe
  sub.time$low.beam.height.ft <- low.beam.height.ft
  sub.time$high.beam.height.ft <- high.beam.height.ft
#Determine if if target should have been in radar
  within.beam <- obs.alt.estimate.ft >= low.beam.height.ft & obs.alt.estimate.ft <= high.beam.height.ft
  sub.time$within.beam <- within.beam
#Add calculated altitude difference to trackpoint to sub.time dataframe
  sub.time$altitude.diff.btwn.obs.estimate.and.actual.ft <- altitude.difference
  #assign("distance",distance,.GlobalEnv)
  #assign("sub.time",sub.time,.GlobalEnv)
#Subset data based on target.heading
  if(target.heading == "N" | target.heading == "n"){
    sub.heading <- subset(sub.time, Heading..deg.N. >=315 | Heading..deg.N. <=45)
  }
    else if(target.heading == "NNE" | target.heading == "nne") {
      sub.heading <- subset(sub.time, Heading..deg.N. >=337.5 | Heading..deg.N. <=67.5)
    }
    else if(target.heading == "NE" | target.heading == "ne") {
      sub.heading <- subset(sub.time, Heading..deg.N. <=90)
    }
    else if(target.heading == "ENE" | target.heading == "ene") {
      sub.heading <- subset(sub.time, Heading..deg.N. >=22.5 & Heading..deg.N. <=112.5)
    }
    else if(target.heading == "E" | target.heading == "e") {
      sub.heading <- subset(sub.time, Heading..deg.N. >=45 & Heading..deg.N. <=135)
    }
    else if(target.heading == "ESE" | target.heading == "ese") {
    sub.heading <- subset(sub.time, Heading..deg.N. >=67.5 & Heading..deg.N. <=157.5)
    }
    else if(target.heading == "SE" | target.heading == "se") {
    sub.heading <- subset(sub.time, Heading..deg.N. >=90 & Heading..deg.N. <=180)
    }
    else if(target.heading == "SSE" | target.heading == "sse") {
      sub.heading <- subset(sub.time, Heading..deg.N. >=112.5 & Heading..deg.N. <=202.5)
    }
    else if(target.heading == "S" | target.heading == "s") {
      sub.heading <- subset(sub.time, Heading..deg.N. >=135 & Heading..deg.N. <=225)
    }
    else if(target.heading == "SSW" | target.heading == "ssw") {
      sub.heading <- subset(sub.time, Heading..deg.N. >=157.5 & Heading..deg.N. <=247.5)
    }
    else if(target.heading == "SW" | target.heading == "sw") {
      sub.heading <- subset(sub.time, Heading..deg.N. >=180 & Heading..deg.N. <=270)
    }
    else if(target.heading == "WSW" | target.heading == "wsw") {
      sub.heading <- subset(sub.time, Heading..deg.N. >=202.5 & Heading..deg.N. <=292.5)
    }
    else if(target.heading == "W" | target.heading == "w") {
      sub.heading <- subset(sub.time, Heading..deg.N. >=225 & Heading..deg.N. <=315)
    }
    else if(target.heading == "WNW" | target.heading == "wnw") {
      sub.heading <- subset(sub.time, Heading..deg.N. >=247.5 & Heading..deg.N. <=337.5)
    }
    else if(target.heading == "NW" | target.heading == "nw") {
      sub.heading <- subset(sub.time, Heading..deg.N. >=270)
    }
    else if(target.heading == "NNW" | target.heading == "nnw") {
      sub.heading <- subset(sub.time, Heading..deg.N. >=292.5 | Heading..deg.N. <=22.5)
    }
    else(sub.heading <- sub.time)
  #assign("sub.heading", sub.heading, .GlobalEnv)
#Determine if Observation is Overhead based on obs.bearing variable and do something different if it is
  if(obs.bearing == "OH" | obs.bearing == "oh") {
    sub.bearing <- subset(sub.heading, obs.distance.to.target.m <= ((obs.dist.estimate.yards*.9144)*4))
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
#Order the data by Ascending Track.ID and then by UNIX.Update.Time. Print result.
  validate.list <- sub.bearing[with(sub.bearing, order(Track.ID, UNIX.Update.Time)), ]
  #print(validate.list)

#Output list of confirmed tracks to the Global Environment
  assign("validate.list",sub.bearing[with(sub.bearing, order(Track.ID, UNIX.Update.Time)), ],.GlobalEnv)
#Create a title and subtitle for the graph based on previous user input
  title <- paste0("TrackUpdates ", obs.bearing,"? from Observer with ",fov, "? FOV ","\nfrom ",obs.UTM.Northing," N ", obs.UTM.Easting," E and Heading ",target.heading)
  sub.title <- paste0(start.time," - ", end.time)
#Plot subsetted data, observation point, ARTI position
  plot(sub.bearing$UTM.Easting,sub.bearing$UTM.Northing,xlim=c(radar.Easting-10000,radar.Easting+10000),main=title,sub=sub.title,ylim=c(radar.Northing-10000,radar.Northing+10000),col="dark red",xlab="Easting",ylab="Northing")
  par(new=T)
  plot(obs.UTM.Easting,obs.UTM.Northing,xlim=c(radar.Easting-10000,radar.Easting+10000),ylim=c(radar.Northing-10000,radar.Northing+10000),pch=18,xlab="",ylab="",axes=F,col="blue")
  par(new=F)
  par(new=T)
  plot(radar.Easting,radar.Northing,xlim=c(radar.Easting-10000,radar.Easting+10000),ylim=c(radar.Northing-10000,radar.Northing+10000),pch=17, xlab="",ylab="",axes=F,col="black")
  par(new=F)
  if(nrow(validate.list) >= 1){
  filename.time <- as.character(strptime(start.time, "%m/%d/%Y %H:%M:%S"), "%Y.%m.%d_%H%M%S")
  graph.filename <- paste0("R_", filename.time,"_",obs.UTM.Northing,"N_",obs.UTM.Easting,"E_",fov,"FOV_",obs.bearing,"bearing_",target.heading,"_heading",".png")
  dev.copy(png, filename = file.path(working.directory,subDir,graph.filename), width = 1024, height = 1024)
  dev.off()
  }
#Write a csv file containing the filtered TrackPoints
  filename.time <- as.character(strptime(start.time, "%m/%d/%Y %H:%M:%S"), "%Y.%m.%d_%H%M%S")
  csv.filename <- paste0("R_", filename.time,"_",obs.UTM.Northing,"N_",obs.UTM.Easting,"E_",fov,"FOV_",obs.bearing,"bearing_",target.heading,"_heading",".csv")
  write.csv(validate.list[with(validate.list, order(Track.ID, UNIX.Update.Time)), ], file = file.path(working.directory,subDir,csv.filename),quote=FALSE, row.names = FALSE)
  }
  else {
    filename.time <- as.character(strptime(start.time, "%m/%d/%Y %H:%M:%S"), "%Y.%m.%d_%H%M%S")
    csv.filename <- paste0("R_", filename.time,"_",obs.UTM.Northing,"N_",obs.UTM.Easting,"E_",fov,"FOV_",obs.bearing,"bearing_",target.heading,"_heading","_No.Radar.Data.csv")
    radar.down <- "nothing to see here, radar down"
    write.csv(radar.down, file = file.path(working.directory,subDir,csv.filename),quote=FALSE, row.names = FALSE)
  }
  }
}
