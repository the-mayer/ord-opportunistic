#Format for entering variables:
#start.time:"2012-08-16 00:20:53 GMT" ->  must " "
#end.time: "2012-08-16 00:20:58 GMT" ->  must " "
#fov: the field of view of the optical equipment used to make the observation
#target.heading: the direction the bird was traveling in. Valid cardinal directions are N,NNE,NE,ENE,E,ESE,SE,SSE,S,SSW,SW,WSW,W,WNW,NW,NNW or RA (random). Entry must be in " ".
#obs.bearing: the bearing from the observer to the bird. If the bird is overhead enter "OH".
#UTM.Northing: the Y coordinate of the observation location. Found in the spreadsheet.
#UTM.Easting: the x coordinate of the observation location. Found in the spreadsheet.
#csv:"filename.csv" ->must include ""


circle <- function(obs.csv) {
  
#Set Working Directory(where are the TrackPoints csv files?)
  working.directory <- getwd()
#assign("working.directory",working.directory,.GlobalEnv)
  setwd(working.directory)
#Set Radar Location 
  radar.Northing <- 4649444.98
  radar.Easting <- 422641.5
  obs.data <- read.csv(obs.csv, header=TRUE, sep=",",stringsAsFactors = FALSE)
  assign("obs.data",obs.data,.GlobalEnv)
#Plot subsetted data, observation point, ARTI position
  plot(radar.Easting,radar.Northing,xlim=c(radar.Easting-10000,radar.Easting+10000),ylim=c(radar.Northing-10000,radar.Northing+10000),pch=17, xlab="Easting",ylab="Northing",col="black")
  par(new=T)
  plot(obs.data$Easting.Long,obs.data$Northing.Lat,xlim=c(radar.Easting-10000,radar.Easting+10000),ylim=c(radar.Northing-10000,radar.Northing+10000),pch=18,xlab="",ylab="",axes=F,col="blue")  
  par(new=t)
  for(i in 0:nrow(obs.data)){
    sub.data <- obs.data[i,]
  
  draw.circle(sub.data$Easting.Long, sub.data$Northing.Lat, (sub.data$Dist..yds.*0.9144),border="dark red")
}
}
