auto_validate <- function(obs.csv,radar,oh.factor){
  #Get ready, GO!
  source("R/arti_validate_v0.2.R")

  #Read in observation data
  obs.data <- read.csv(obs.csv, header=TRUE, sep=",",stringsAsFactors = FALSE)
    assign("obs.data",obs.data,.GlobalEnv)
  #Loop through for as many rows of data are contained in the observation data.csv
  for(i in 1:nrow(obs.data)){
    sub.data <- obs.data[i,]
        #assign("sub.data",sub.data,.GlobalEnv)
  #Pull variables from obs.csv
    start.time <- paste0(sub.data$Date," ", as.character(strptime(sub.data$Start.Time.GMT, "%H:%M:%S"),"%H:%M:%S")," GMT")
        #assign("start.time",start.time, .GlobalEnv)  
    end.time <- paste0(sub.data$Date," ", as.character(strptime(sub.data$End.Time.GMT, "%H:%M:%S"),"%H:%M:%S")," GMT")
        #assign("end.time",end.time, .GlobalEnv)
    
    fov <-90
        #assign("fov",fov,.GlobalEnv)
  
    target.heading <- sub.data$Heading
    if(target.heading==""){target.heading<-"RA"}
        #assign("target.heading",target.heading,.GlobalEnv)
    obs.bearing<-sub.data$Bearing
        if(is.na(obs.bearing)){obs.bearing<-0}
        #assign("obs.bearing",obs.bearing,.GlobalEnv)
    obs.UTM.Northing <- as.numeric(sub.data$Northing.Lat)
        #assign("obs.UTM.Northing",obs.UTM.Northing,.GlobalEnv)
    obs.UTM.Easting <- as.numeric(sub.data$Easting.Long)
        #assign("obs.UTM.Easting",obs.UTM.Easting,.GlobalEnv)
    
    obs.dist.estimate.yards <- as.numeric(sub.data$Dist..yds.)
    if(is.na(obs.dist.estimate.yards)){obs.dist.estimate.yards<-0}
        #assign("obs.dist.estimate.yards",obs.dist.estimate.yards,.GlobalEnv)
    obs.alt.estimate.ft <- as.numeric(sub.data$Est.Alt..ft.)
        #assign("obs.alt.estimate.ft",obs.alt.estimate.ft,.GlobalEnv)
  
    weather <- sub.data$Weather
        #assign("weather",weather,.GlobalEnv)
    if(radar=="AR2-1"){radar.schema<-"ohare1"}
    else if(radar=="AR2-2"){radar.schema<-"ohare2"}
    else if(radar=="AR1"){radar.schema<-"ohare3"}
    
    csv <- paste0("CSV/",radar.schema,"/",as.character(strptime(sub.data$Date,"%m/%d/%Y"),"%Y%m%d"),"_",as.character(strptime(sub.data$Start.Time.GMT,"%H:%M:%S"),"%H.%M.%S"),"_GMT_",radar.schema,".csv")
      #assign("csv",csv,.GlobalEnv)
    USDA.grid.loc<-sub.data$Grid.Loc
  
    #map<-"TRAN_17_Illinois_GU_STATEORTERRITORY/Trans_AirportRunway.shp"
    
    #Pass variables to validate function
    validate(start.time, end.time, fov, target.heading, obs.bearing, obs.UTM.Northing, obs.UTM.Easting, obs.dist.estimate.yards, obs.alt.estimate.ft, weather, radar,csv, oh.factor,USDA.grid.loc)
  }
}