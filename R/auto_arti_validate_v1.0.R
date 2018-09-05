auto_validate <- function(obs.csv,radar,fov,oh.factor,time.allowance,dist.allowance.factor){
  #Get ready, GO!
  source("R/arti_validate_v1.0.R")
  run.start.time<-Sys.time()
  working.directory<-getwd()
  subDir <- paste0(radar,"_Validated")
  dir.create(file.path(working.directory, subDir), showWarnings = FALSE)
  
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
    
    #fov <-90
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
    USDA.number.of.birds<-sub.data$X..of.Birds
    USDA.species<-sub.data$X2013.Species.Code
    USDA.flight.behavior<-sub.data$Flight.Behavior
    USDA.accuracy<-sub.data$Accuracy
    USDA.comments<-sub.data$Comments

    
    #Pass variables to validate function
    validate(start.time, end.time, fov, target.heading, obs.bearing, obs.UTM.Northing, obs.UTM.Easting, obs.dist.estimate.yards, obs.alt.estimate.ft, weather, radar,csv, oh.factor,USDA.grid.loc,time.allowance,dist.allowance.factor,USDA.number.of.birds,USDA.species,USDA.flight.behavior,USDA.accuracy,USDA.comments)
  }
  #Write processing parameters file
  run.finish.time<-Sys.time()
  run.time<-run.finish.time-run.start.time
  parameters<-data.frame(Radar=radar,FOV=fov,OH.Factor=oh.factor,Time.Allowance=time.allowance,Dist.Allowance.Factor=dist.allowance.factor,Script.Runtime=run.time)
  parameters.filename<-paste0(radar,"_",as.character(run.finish.time,format="%Y%m%d_%H%M%S"),"_parameters.csv")
  write.csv(parameters, file = file.path(working.directory,subDir,parameters.filename),quote=FALSE, row.names = FALSE)
  #write summary file
  obs.processed<-length(list.files(path=file.path(working.directory,subDir,"CSV")))
  no.match<-length(list.files(path=file.path(working.directory,subDir,"CSV"),pattern="*_NO_MATCH.csv"))
  rain<-length(list.files(path=file.path(working.directory,subDir,"CSV"),pattern="*_RAIN.csv"))
  insufficient<-length(list.files(path=file.path(working.directory,subDir,"CSV"),pattern="*_insufficient.csv"))
  no.data<-length(list.files(path=file.path(working.directory,subDir,"CSV"),pattern="*_NO_DATA.csv"))
  match<-length(list.files(path=file.path(working.directory,subDir,"PNG")))
  percent.validated<-(match)/(no.match+match)*100
  summary<-data.frame(Total.Obs.Processed=obs.processed,Match=match,No.Match=no.match,Percent.Validated=percent.validated,Obs.w.Rain=rain,Insufficient.Obs=insufficient,Missing.Radar.Data=no.data)
  summary.filename<-paste0(radar,"_",as.character(run.finish.time,format="%Y%m%d_%H%M%S"),"_summary.csv")
  write.csv(summary, file = file.path(working.directory,subDir,summary.filename),quote=FALSE, row.names = FALSE)
  list<-list.files(path=file.path(working.directory,subDir,"PNG"))
  list.filename<-paste0(radar,"_",as.character(run.finish.time,format="%Y%m%d_%H%M%S"),"_list.csv")
  write.csv(list, file = file.path(working.directory,subDir,list.filename),quote=FALSE, row.names = FALSE)
  print("That was a hoot!")
}