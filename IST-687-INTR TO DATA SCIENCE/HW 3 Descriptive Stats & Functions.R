

#COURSE NUMBER: IST 687
#NAME: Harsh Darji
#SUID: 810131016
# Homework 3- Submitted by Harsh Darji on September 13, 2018
# Portions of this code came from Introduction to Data Science
# but the comments are all original.

#Step A: Use read.csv( ) and url( ) to read a CSV file form the web into a data frame

##1.	Use R code to read directly from a URL on the web. Store the dataset into a new dataframe, called dfStates. Use stringsAsFactors=FALSE. The URL is: 
  #https://www2.census.gov/programs-surveys/popest/datasets/2010-2017/state/asrh/scprc-est2017-18+pop-res.csv
dfstates<-read.csv(url("https://www2.census.gov/programs-surveys/popest/datasets/2010-2017/state/asrh/scprc-est2017-18+pop-res.csv"),stringsAsFactors=FALSE)
dfstates
  
#Step B: Clean the dataframe

##2.Use View( ), head( ), and tail( ) to examine the data frame. 
View(dfstates)
head(dfstates)
tail(dfstates)

##3.	Remove unneeded columns and rows by using the minus sign in the rows or columns of the [ , ] accessor. 
dfstates<-dfstates[-1, ]
dfstates
 ##4.	Remove the last Row (for Puerto Rico)
dfstates<-dfstates[-grep("Puerto Rico Commonwealth", dfstates$NAME),]
View(dfstates)

#5.	Make sure there are exactly 51 rows (one per state + the district of Columbia).  
#Hint: remove Puerto Rico and the summary for the united states

nrow(dfstates)
# 51

#6.	Make sure there are precisely 4 columns, with the following names:
#  stateName, population, popOver18, percentOver18. 
#Hint: use colnames( ) and you will need to remove some columns

colnames(dfstates)
dfstates[,c("SUMLEV","REGION","DIVISION","STATE")]<- list(NULL)
colnames(dfstates)

#rename(dfstates,c("NAME"="stateName","POPESTIMATE2017"="population","POPEST18PLUS2017"="popOver18","PCNT_POPEST18PLUS"="percentOver18" ))
names(dfstates)
names(dfstates)<-c("stateName","population","popOver18","percentOver18")
names(dfstates)
View(dfstates)

#Step C: Create a Function
##7.	Create a function that takes no parameters and returns the clean dataframe created in step 6 above


my_fun<- function()
{
 df<-data.frame(dfstates)
 return(df)
}
my_fun()

#Step D: Explore the dataframe
##9.	Find the state with the highest population  (use which.max)
highestpopulation<-row.names(df)[which.max(df$population)]
highestpopulation
df[5,1]
# "California"

##10.	Create a histogram of the state populations, what do you observe?
hist(df$population,breaks=100)

#Most of the states in the Unted States are clubbed together whuch indicates that the population of most 
# of the states have similar population. However there are outliers which are separated which indicates 
# the satte having highest population as there's a huge difference between average population and state with 
# highest population.

hist(rnorm(51,6053834,6823984))
rnorm(51,6053834,6823984)

##11.	Sort the data frame by population (hint the use 'order' function)
df1<-df[order(df$population,decreasing = FALSE),]
df1


##12.	Show the 10 states with the lowest populations
rownames(df1)<- df1$stateName
df1
Low10<-row.names(df1)[1:10]
Low10

##13.	Use barplot( ) to create a plot of each of the population from the sorted dataframe.  What do you observe?
  
barplot(df1$population)
barplot(df1$popOver18)
barplot(df1$percentOver18)
 #Most of the states in the United States has similar population.There is huge difference betweer state
# with highest population and state with lowest population.


