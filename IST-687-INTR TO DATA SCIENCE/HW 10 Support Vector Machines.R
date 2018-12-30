# IST687  HW 10  - SVM
# NAME:HARSH DARJI 
# Due date- 11/15/2018
# Submitted Date-11/14/2018


install.packages("RJSONIO") #install package RJSONIO
library(RJSONIO)

#Part A: Load and condition the data  
#1.	The data is available on blackboard (hotelSurveyBarriot), as a JSON file.
#Hint: Don't forget to use setwd() to make sure that R is looking in the right folder for your text file.

data<-"hotelsurveysherison.json" #storing json file in a variable
hotelSurvey<- fromJSON(data, simplify = TRUE, nullValue = NA)#loading json file
hotelSurvey<- data.frame(hotelSurvey) #converting json file to dataframe
hotelSurvey<- hotelSurvey[,-11]# remove free text column
View(hotelSurvey)
str(hotelSurvey) #view structure of file
summary(hotelSurvey)
#Part B: Create a happy customer variable 
#2.	To focus on predicting happy customers, we need to generate a new column (where overallCustSat is 8 or higher).

install.packages('kernlab')
library(kernlab)
newrep=replicate(length(hotelSurvey$overallCustSat), "nil")
newrep[hotelSurvey$overallCustSat >= 8]="Happy"
newrep[hotelSurvey$overallCustSat < 8]="notHappy"
hotelSurvey$Customers_sats=newrep
hotelSurvey$Customers_sats
View(hotelSurvey)


#Part C: Create training and test data sets
#Using techniques discussed in class, create two datasets - one for training, one for testing.
#3.	Pages 235 - 237 of the book describe how to create a training data set and a test data set. Following the strategy in the book, the training data should contain about two thirds of the whole data set, with the remaining one third going to the test data.


randIndex=sample(1:dim(hotelSurvey)[1])
randIndex
cutPoint2_3=floor(2 * dim(hotelSurvey)[1]/3)
cutPoint2_3
trainData=hotelSurvey[randIndex[1:cutPoint2_3],]
trainData
View(trainData)
testData=hotelSurvey[randIndex[(cutPoint2_3+1):dim(hotelSurvey)[1]],]
testData
str(testData)
View(testData)
#4.	Use the dim( ) function to demonstrate that the resulting training data set and test data set contain the appropriate number of cases

dim(trainData)
dim(testData)

#Step C: Build a Model using ksvm( ) 
#5.	Build a support vector model using the ksvm( ) function using two or three of the variables to predict a happy customer. Once you have specified the model statement and the name of the training data set, you can use the same parameters as shown on page 237: kernel= "rbfdot", kpar = "automatic", C = 5, cross = 3, prob.model = TRUE
#parameters
# Data parameters help to select the dataframe.The kpar argument refers to a variety of parameters that can be used to control
#the operation of the radial basis function kernel. In this case, we are
#depending on the goodwill of the designers of this algorithm by specifying
#automatic.#c argument helps to select cost of constraints.
svm1=ksvm(Customers_sats ~ hotelFriendly + guestAge, data=trainData, kernel = "rbfdot",
            kpar="automatic", C=5, cross=3, prob.model=TRUE)
#7.	Store the output of kvsm( ) in a variable and then echo that variable to the console.   
svm1

#Part D: Predict Values in the Test Data and Create a Confusion Matrix
#8.	Use the predict( ) function to validate the model against test data. Assuming that you put the output from the ksvm( ) call into svmOutput and that your test data set is in a data frame called testData, the call would be:
 # svmPred <- predict(svmOutput, testData, type = "votes")

svmpred1=predict(svm1, testData[c(6,8)], type="votes")
svmpred1
#9.	Now the svmPred object contains a list of votes in each of its rows. The votes are either for "happy" or "notHappy". Review the contents of svmPred using str( ) and head( ).
str(svmpred1)
head(svmpred1)

#10.	Create a confusion matrix (a 2 x 2 table) that compares the second row of svmPred to the contents of testData$happy variable.
compTable1=data.frame(testData[,11], svmpred1[2,])
table(compTable1)

#
#Part F: Find a good prediction
#12.	Repeat Parts C and D to try and improve your prediction

svm2=ksvm(Customers_sats ~ hotelClean + hotelFriendly + guestAge, data=trainData, kernel = "rbfdot",
            kpar="automatic", C=5, cross=3, prob.model=TRUE)
svm2

svmpred2=predict(svm2, testData[c(5,6,8)], type="votes")
svmpred2
str(svmpred2)
head(svmpred2)

compTable2=data.frame(testData[,11], svmpred2[2,])
table(compTable2)

svm3=ksvm(Customers_sats ~ checkInSat + hotelClean + hotelFriendly + guestAge, data=trainData, kernel = "rbfdot",
            kpar="automatic", C=5, cross=3, prob.model=TRUE)
svm3

svmpred3=predict(svm3, testData[c(3,5,6,8)], type="votes")
svmpred3
str(svmpred3)
head(svmpred3)

compTable3=data.frame(testData[,11], svmpred3[2,])
table(compTable3)

svm4=ksvm(Customers_sats ~ checkInSat + hotelFriendly + guestAge + hotelSize + lengthOfStay
            + whenBookedTrip, data=trainData, kernel = "rbfdot",
            kpar="automatic", C=5, cross=3, prob.model=TRUE)
svm4

svmpred4=predict(svm4, testData[c(2,3,5,6,8,9,10)], type="votes")
svmpred4
str(svmpred4)
head(svmpred4)

compTable4=data.frame(testData[,11], svmpred1[2,])
table(compTable4)
#First and foremost, we create test partitions to provide us honest assessments of the performance of our predictive models.  No amount of mathematical reasoning and manipulation of results based on the training data will be convincing to an experienced observer.
# models are expected to remain relatively stable over time so that a well-constructed model built on last month's data is reasonably expected to perform adequately on next month's data 