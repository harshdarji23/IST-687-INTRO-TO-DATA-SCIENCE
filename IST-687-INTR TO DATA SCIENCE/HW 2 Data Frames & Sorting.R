
#COURSE NUMBER: IST 687
#NAME: Harsh Darji
#SUID: 810131016
# Homework 2- Submitted by Harsh Darji on September 12, 2018
# Portions of this code came from Introduction to Data Science
# but the comments are all original.

#Step A: Initialize an 'arrests' dataframe
#1)	Copy USArrests into a new variable (called 'arrests')

arrests<-USArrests
arrests


#Step B: Explore the assault rate

#2)	Write a comment: Is a higher or lower assault rate best?
# LOW assault rate is best

#3)	Which state has the best assault rate ?
best<-row.names(arrests)[which.min(arrests$Assault)]
best
# North Dakota

#Step C: Explore the murder rate  

#4)	Which state has the highest murder rate?
highestmurder<-row.names(arrests)[which.max(arrests$Murder)]
highestmurder
# "Georgia"

#5)	Create a sorted dataframe, based on descending murder rate

MurderDF<-arrests[order(arrests$Murder, decreasing =TRUE),]
MurderDF

#6)	Show the 10 states with the highest murder rate
Top10<-row.names(MurderDF)[1:10]
Top10

#7)	What is the value of the 20'th row, third column (in the sorted dataframe)? Use R code (not visual inspection)
Twenty<-MurderDF[20,3]
Twenty
#50

#Step D: Which state is the least safe? Explain your logic

#8)	Write the R code to determine your answer
MurderDF$Crime<-(MurderDF$Murder) + (MurderDF$Assault)+ (MurderDF$Rape)
MurderDF

leastsafe<-row.names(MurderDF)[which.max(MurderDF$Crime)]
leastsafe

# "Florida"

#9)	Write a comment to explain your logic
" According to the USARRESTS dataset, we have information regarding Murder per 100000,Assault per 100000 and Rape per 100000. 
Urban population is in percentage(which is not necessary) of all the states in USA.Least safe state will be a state where 
crime rate is high. Crime rate is Summation of Murder,assault and rape. So, Florida has crime of 382.3 on 100000. Thus 
making it  least safe state."