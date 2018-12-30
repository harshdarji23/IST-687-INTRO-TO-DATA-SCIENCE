##################################################################
# IST 687- Introduction to Data Science
# Due date - 09/26/18 by 11:59 p.m.
# Homework 8:Linear Modeling- Submitted by Harsh Darji on 10/24/18
# Portions of this code came from Introduction to Data Science
# but the comments are all original.


#A. Load and condition the data

#Importing the necessary libraries for working on JSON files and plots
library(RCurl)
library(jsonlite)
library(ggplot2)
library(ggmap)

getwd() #Locating the current directory



hotelresults=fromJSON("hotelSurveySherison.json") #Loading the JSON file
hotelresults=data.frame(hotelresults) # converting gthe JSON file to a dataframe
View(hotelresults)
str(hotelresults) #Accessing the structure of the JSON file


#B. Explore the data

#The dependent variable here would be overall customer satisfaction which 
#depends aon anumber of attributes in the dataframe.
#So, the overall customer satisfaction attribute would be on the Y-axis

#Plotting overall customer satisfaction with respect to the following variables
#1. Size of the hotel
plt_1 <- ggplot(hotelresults,aes(x=hotelSize,y=overallCustSat))+
  geom_jitter()
plt_1

#2. State in which the hotel is located 
ustate<- map_data("state")
hotelresults$hotelState <-tolower(hotelresults$hotelState)
plt_2=ggplot(hotelresults,aes(map_id = hotelState,fill=as.factor(overallCustSat)))+
  geom_map(map=us,color= "black")+
  expand_limits(x=us$long, y=us$lat)
plt_2

#3. Cleanliness of the hotel
plt_3 <- ggplot(hotelresults,aes(x=hotelClean,y=overallCustSat))+
  geom_jitter()
plt_3

#4. Friendliness of the hotel
plt_4 <- ggplot(hotelresults,aes(x=hotelFriendly,y=overallCustSat))+
  geom_jitter()
plt_4

#5. Length of stay at the hotel
plot_5 <- ggplot(hotelresults,aes(x=lengthOfStay,y=overallCustSat))+
  geom_jitter()
plot_5

#6. Age of Guests at the hotel
plt_6 <- ggplot(hotelresults,aes(x=guestAge,y=overallCustSat))+
  geom_jitter()
plt_6

#7. Number of days before the booking was done
plot_7 <- ggplot(hotelresults,aes(x=whenBookedTrip,y=overallCustSat))+
  geom_jitter()
plt_7

#8. Gender
plt_8 <- ggplot(hotelresults,aes(x=gender,y=overallCustSat))+
  geom_jitter()
plt_8

#9. Satisfaction while checking in
plot9 <- ggplot(hotelresults,aes(x=checkInSat,y=overallCustSat))+
  geom_jitter()
plot9

#The geom_jitter() command is used is give a more spread of datapoints in the output plot
#thus, providing a better visualization


#C. Generate a linear model
options(scipen=999) # To obtain a distribution of values on the axes in readable form

#To find the overall customer satisfaction w.r.t all variables but freeText
custSat=lm(formula= overallCustSat ~.-freeText, data= hotelresults)
custSat
summary(custSat)

#The R -squared value is 0.92 and the p-value is extremely less than 0.05,
#which signifies that the model developed is a very good one, which is quite 
#obvious as the number of independent variables are more in this particular model
#which provide an extremely good fit for the model on the data, thus the high value of R-squared

#The significant variables as obtained from the summary with their coefficients are
#1. checkInSat -  0.0000046240720
#2. hotelStatemaine- 0.00422 
#3. hotelFriendly - < 0.0000000000000002
#4. guestAge - 0.0000001392848

#The above model is mostly dependent on the above four signifacnt varaibles,
#which are satisfaction while checking in, friendliness of the hotel,
#age of guests and hotel in the state of maine. These independent variables have a 
#very low p-value and contribute the most to the fit of the model on the overall customer 
#satisfaction values.

#D. Generate a different linear model
#considering the independent variable with the lowest p-value(highest significance)

#Considering friendliness of the hotel
model2=lm(formula=overallCustSat ~hotelFriendly, data=hotelresults)
summary(model2)#Gtting the summary
plot(hotelresults$overallCustSat, hotelresults$hotelFriendly)
abline(model2) #To draw a line over the linear model

#From the two linear models, it can inferred that friendliness of the hotel
#when considered alone, is the most significant variable
#This is because in the first linear model, the number of significant variables were more
#and thus , the model there provided us with a better R-squared value and a better fit
#through the data. However, in the second model, the most significant variable alone here 
#cannot provide the best fit, but it provides a better fit than the other 3 significant variables
#because of its comparatively high R-squared value and a very low p-value, which is an 
#important contributor. Still, the first linear model would be a more accurate one because of both
#a high R-squared and a low p-value
