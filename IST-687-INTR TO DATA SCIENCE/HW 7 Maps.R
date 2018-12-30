#COURSE NUMBER: IST 687
#NAME: Harsh Darji
#SUID: 810131016
# Homework 7- Submitted by Harsh Darji on October 17, 2018
# Portions of this code came from Introduction to Data Science
# but the comments are all original.

#Step A: Load and Merge datasets
#1) Read in the census dataset and the USArrests and merge them (just like HW6)
csvFile <- "https://www2.census.gov/programs-surveys/popest/datasets/2010-2017/state/asrh/scprc-est2017-18+pop-res.csv"
dfStates <- read.csv(url(csvFile)) #Reading the csv file from the url into a variable
newStates <- function(){ #Creating a new function which cleans the dataframe 'dfStates'
  dfStates <- dfStates[-1,-1:-4]
  lastIndex <- nrow(dfStates)
  dfStates <- dfStates[-lastIndex,]
  colnames(dfStates) <- c("stateName", "population", "popOver18", "percentOver18")
  return(dfStates)
}
states <- newStates() 
View(states)

arrests <- USArrests #Storing a dataset into a new variable
View(arrests)

arrests$stateName <- rownames(arrests) #Copying rownames of arrests and pasting in a new column in arrests dataset
mergeDf <- merge(states, arrests, by = "stateName") #merging columns of arrests and states dataset with reference to stateName and pasting into a new dataset
View(mergeDf)

#2) Add the area of each state, and the center of each state, to the merged dataframe, 
#using the 'state.center', 'state.center' and 'state.name' vectors
stateName <- state.name #getting all state names
stateArea<-state.area #getting all state areas
stateCenter <- state.center #getting coordinates of the centers of all states

otherDf <- data.frame(stateName, stateArea, stateCenter) #merging above three datasets to form a dataframe
View(otherDf)

mergeDf <- merge(mergeDf, otherDf, by = "stateName") #merging columns of arrests and states dataset with reference to stateName and pasting into a new dataset
View(mergeDf)

mergeDf$stateName <- tolower(mergeDf$stateName) #converting all state names to lower case because R cannot process capital letters
View(mergeDf)

#Step B: Generate a color coded map
#3) Create a color coded map, based on the area of the state 
us <- map_data("state") #turn data from the maps package in to a data frame suitable for plotting with ggplot2
View(us)
mapArea <- ggplot(mergeDf, aes(map_id = stateName)) #initializing a ggplot object and passing mergeDf as the input data with map ID as stateName.
mapArea <- mapArea + geom_map(map = us, aes(fill= stateArea)) #creating a map visualization
mapArea <- mapArea + expand_limits(x = mergeDf$x, y= mergeDf$y) #defining the x and y axes values of the map
mapArea <- mapArea + coord_map() + ggtitle("Map of US based on state area") #coord_map() handles the distortion and aspect ratio of the map. ggtitle() gives a title to the map
mapArea

#Step C: Create a color shaded map of the U.S. based on the Murder rate for each state 
#4) Repeat step B, but color code the map based on the murder rate of each state.
mapArea1 <- ggplot(mergeDf, aes(map_id = stateName))
mapArea1 <- mapArea1 + geom_map(map = us, aes(fill=Murder))
mapArea1 <- mapArea1 + expand_limits(x = mergeDf$x, y= mergeDf$y)
mapArea1 <- mapArea1 + coord_map() + ggtitle("Map of US based on murder rate of states")
mapArea1

#5) Show the population as a circle per state (the larger the population, the larger the circle), using the location defined by the center of each state
mapArea2 <- mapArea1 + geom_point(data = mergeDf, aes(x = mergeDf$x, y= mergeDf$y, color = "red", size = mergeDf$population)) #geom_point() plots points at the center of every state. size attribute controls the size of the points
mapArea2

#Step D: Zoom the map
#6) Repeat step C, but only show the states in the north east
#Hint: get the lat and lon of new york city
#Hint: set the xlim and ylim to NYC +/- 10
latlon <- geocode("new york city, ny", source = "dsk") #getting the latitude and longitude of an address. source attribute hold the source of the geocodes
latlon
mapArea3 <- ggplot(mergeDf,aes(map_id = stateName)) + geom_map(map= us, aes(fill = Murder)) + expand_limits(x= mergeDf$x, y= mergeDf$y) + coord_map() + ggtitle("Zoomed map of US") 
mapArea3 <- mapArea3 + xlim(latlon$lon-10,latlon$lon+10) + ylim(latlon$lat-10, latlon$lat+10) #Observations not in range will be dropped completely
mapArea3