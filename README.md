# ord-oportunistic
R functions were developed to isolate radar tracks that matched characteristics of birds observed by wildlife personnel on the airport. 

A study was conducted at O'Hare International Airport, where birds were observed through binoculars by wildlife personnel. 3 avian radar systems were simultaneously operating at the airport during this time. Species, time, and flight characteristics were recorded and digitized. These digitized records were used to isolate corresponding radar tracks, when within user definable tolerances (accounting for human error in recording). Characteristics of human observations with matching radar targets were explored and used to determine the efficacy of the avian radar system that was deployed at the airport

#ORD_ARTI.validate_2.R:
latest and greatest, used in final manuscript. Loops through observations, isolates matching tracks, writes tabular information and plots findings on a map for review

#clutter.density.map_2:
function developed to plot observed species in relation to where each individual radar had coverage -- radar coverage defined as being absent of radar clutter and had high levels of recorded track activity through the duration of the study. Many observed birds were located in areas of poor radar coverage, leading to low correlation between observer and radar. 

#the rest:
Some of my earliest R code, even before I knew the joys of ggplot2. Bits and pieces were iterated and folded into the latest and greatest track finding function mentioned above. 
