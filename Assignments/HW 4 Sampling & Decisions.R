#COURSE NUMBER: IST 687
#NAME: Harsh Darji
#SUID: 810131016
# Homework 4- Submitted by Harsh Darji on September 26, 2018
# Portions of this code came from Introduction to Data Science
# but the comments are all original.
########################################
#Part A: Write a function to reveal the distribution of a vector of numeric values
#1.	Create a new function 'printVecInfo' and have it take one numeric vector as its input argument. 


printVecInfo<-function(testVector)
  #2.	Make the function print the following information for the vector supplied in the argument:
  #a.	Mean
  #b.	Median
  #c.	Min & Max
  #d.	Standard deviation
  #e.	0.05 and 0.95 quantiles (Use the quantile( ) function) 

{
  M <-mean(testVector)
  M1<-median(testVector)
  Min1<-min(testVector)
  Max2<-max(testVector)
  S1<-sd(testVector)
  Q1<-quantile(testVector, 0.05)
  Q3<-quantile(testVector,0.95)
  
  cat("Mean is: ",M ,"\n")
  cat("Median is :" ,M1,"\n")
  cat("Minimum is :",Min1,"\n")
  cat("Maximum is :",Max2,"\n")
  cat("Standard deviation :",S1,"\n")
  cat("Quantile at 5%:",Q1,"\n")
  cat("Quantile at 95%:",Q3,"\n")
  
}
#3.	Test the function with this vector: testVector <- 1:10. Results should look something like this:
  ##[1] 10
#[1] 1
#[1] 3.02765
#5%  95% 
 # 1.45 9.55

testVector <- 1:10
testVector

#4.	 Add labels to each element of the function's output.


printVecInfo(testVector)
#Mean is:  5.5 
#Median is : 5.5 
#Minimum is : 1 
#Maximum is : 10 
#Standard deviation : 3.02765 
#Quantile at 5%: 1.45 
#Quantile at 95%: 9.55 

#Part B: Read the census dataset
  #5.	Read in the Census dataset 
   #Hint: reuse the function you created in HW 3
dfstates<-read.csv(url("https://www2.census.gov/programs-surveys/popest/datasets/2010-2017/state/asrh/scprc-est2017-18+pop-res.csv"))
dfstates
names(dfstates)<-c("stateName","population","popOver18","percentOver18")


my_fun<- function()
{
  
  return(dfstates)
}

my_fun()
x<-par(mfrow=c(3,2))
x
#Part C: Sample from the state population data frame
#6.	Sample 20 observations from states$population and use printVecInfo( ) to display the characteristics of the resulting sample, and then display the results as a histogram.

mysample<-sample(dfstates$population,size = 20, replace = TRUE)
printVecInfo(mysample)
options(scipen=999)
hist(mysample,main="My samaple 1",xlab="population",ylab = "frequency")
#7.	Repeat step five two more times. Each time that you create a sample, run the resulting vector through printVecInfo( ) and create a histogram. 

mysample1<-sample(dfstates$population,size = 20, replace = TRUE)
printVecInfo(mysample1)
options(scipen=999)
hist(mysample1,main="My samaple 2",xlab="population",ylab = "frequency")

mysample2<-sample(dfstates$population,size = 20, replace = TRUE)
printVecInfo(mysample2)
options(scipen=999)
hist(mysample2,main="My samaple 3",xlab="population",ylab = "frequency")

#8.	Using a block comment, explain in a comment why each result is different.
# Each result is different because we are taking different random samples for population which are 
# distributed unevenly and have a different mean values.

#Part D: Replicate the sampling
##9.	Use the replicate function, to replicate the sampling (described in step 5 above). Replicate the sampling 2000 times, then use printVecInfo( ) to display the characteristics of the resulting replicated sample, and then display the results as a histogram.
colors = c("red", "yellow", "green", "violet", "orange", 
             "blue", "pink", "cyan")

myrep<-replicate(2000,mean(sample(dfstates$population,size = 20, replace = TRUE)))
printVecInfo(myrep)
options(scipen=999)
hist(myrep,main="My replicate ",xlab="population",ylab = "frequency",col=colors)

##10.	Repeat step 8 two more times. Each time that you create the replicated sample, run the resulting vector through printVecInfo( ) and create a histogram. 
myrep1<-replicate(2000,mean(sample(dfstates$population,size = 20, replace = TRUE)))
printVecInfo(myrep1)
options(scipen=999)
hist(myrep1,main="My replicate 1",xlab="population",ylab = "frequency",col=colors)

myrep2<-replicate(2000,mean(sample(dfstates$population,size = 20, replace = TRUE)))
printVecInfo(myrep2)
options(scipen=999)
hist(myrep2,main="My replicate 2",xlab="population",ylab = "frequency",col=colors)
##11.	 Using a block comment,  explain why the histograms generated in Part C are different than the histograms generated in Part D
# Histograms generated in part c are different then part D because in part C we aren't replicating the values of population
#while in part D we are replicating the values. So, we see a normal distribution as we take mean 2000times.
#Rhus replicating the mean of sdample helps us to see a normal distribution.
