# IST687  HW 9  - Finding Patterns with arules
# NAME:HARSH DARJI 
# Due date- 11/08/2018
# Submitted Date-11/07/2018


install.packages("RJSONIO") #install package RJSONIO
library(RJSONIO)

#Part A: Explore Data Set

#Load the dataset: hotelSurveyBarriot.json (similar to HW8, but a different dataset)
#Name the dataframe hotelSurvey
data<-"hotelsurveysherison.json" #storing json file in a variable
hotelSurvey<- fromJSON(data, simplify = TRUE, nullValue = NA)#loading json file
hotelSurvey<- data.frame(hotelSurvey) #converting json file to dataframe
hotelSurvey<- hotelSurvey[,-11]# remove free text column
View(hotelSurvey)
str(hotelSurvey) #view structure of file
summary(hotelSurvey)

#Part B: Explore Data Set

#Maping each numeric attribute to a category  – Since we want to create rules, 
#we should convert the attributes that have a numeric range into buckets 
createBucketSurvey<- function(vec) #creating a function createBucketSurvey to map attributes
{
vBuckets <- replicate(length(vec), "Average") # making all the values average
vBuckets[vec > 7] <- "High" # now generate high value group
vBuckets[vec < 7] <- "Low" # now generate low value
return(vBuckets) 
}

custSat<-createBucketSurvey(hotelSurvey$overallCustSat) #function call
checkInSat<-createBucketSurvey(hotelSurvey$checkInSat) #function call
hotelClean<-createBucketSurvey(hotelSurvey$hotelClean) #function call
hotelFriendly<-createBucketSurvey(hotelSurvey$hotelFriendly)#function call

# creating function creatBucketSurvey2 for other varibales 
createBucketSurvey2 <- function(vec)
{
  q <- quantile(vec, c(0.4, 0.6))
  vBuckets2 <- replicate(length(vec), "Average")
  vBuckets2[vec <= q[1]] <- "Low"
  vBuckets2[vec > q[2]] <- "High"
  return(vBuckets2)
}

HotelSize<-createBucketSurvey2(hotelSurvey$hotelSize) #function call
Age<-createBucketSurvey2(hotelSurvey$guestAge) #function call
LenghtOfStay<-createBucketSurvey2(hotelSurvey$lengthOfStay) #function call
WhenBookedTrip<-createBucketSurvey2(hotelSurvey$whenBookedTrip) #function call

#Counting the people in each category of for the age and friendliness attributes
table(Age)
table(hotelFriendly)
table <-table(Age, hotelFriendly)

#Expressing the results of Age and hotelFriendly
prop.table(table) *100
table2 <- table(Age, custSat)
prop.table(table2)*100
# we observe that highest percentage of customers satisfaction is 
# high when the customer age is high
# other thing we can observe that highest percentage of customer satisfaction is Average when 
#the Age of customer is Low

#PartC: Coerce the data frame into transactions

#Install and library two packages: arules and arulesViz
install.packages("arules")
library(arules)
install.packages("arulesViz")
library(arulesViz)

#create Dataframe
hotelSurveyX<- data.frame(custSat,checkInSat,hotelClean,hotelFriendly,HotelSize,Age,LenghtOfStay,WhenBookedTrip)
View(hotelSurveyX)

#Creating hotelSurvey data frame into a sparse transactions matrix 
hotelSurveyX <- as(hotelSurveyX,"transactions")  
View(hotelSurveyX)

#Use the inspect( ), itemFrequency( ), and itemFrequencyPlot( ) commands 
#to explore the contents of hotelSurveyX.
inspect(hotelSurveyX)
itemFrequency(hotelSurveyX)
itemFrequencyPlot(hotelSurveyX)

#Part D: Use arules to discover patterns

#Running the apriori command to try and predict happy customers.
ruleset<-apriori(hotelSurveyX, parameter = list(support=0.2,confidence=0.5),
                 appearance = list(default="lhs",rhs=("custSat=High")))
inspect(ruleset) # viewing the rulesets
summary(ruleset) #summary of the rules

#Contains a value for lift which is large enough to make this a good rule, and conditions which will be a valid condition to check
#{checkInSat=High,                                                          
# hotelClean=High,                                                          
# WhenBookedTrip=High}   => {custSat=High}  0.2186  0.9014433 1.897376  2186

#Contains a value for lift which is large enough to make this a good rule, and conditions which will be a valid condition to check
#{checkInSat=High,                                                          
# hotelClean=High,                                                          
# Age=High}              => {custSat=High}  0.2239  0.8621486 1.814668  2239

