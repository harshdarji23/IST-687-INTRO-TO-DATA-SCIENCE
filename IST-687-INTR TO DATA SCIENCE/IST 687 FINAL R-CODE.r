

data=read.csv("C:\\Users\\Harsh Darji\\Desktop\\SS.csv", header=TRUE)   # Naming the file data


# Renaming Columns 

colnames(data)<-c("satisfaction","airline_status","age","gender","price_sensitivity","year_of_first_flight","no_of_flights_per_annum",
                  "percent_of_flights_with_other_airlines","type_of_travel","no_of_loyalty_cards","shopping_amount_at_airport",
                  "eat_drink_at_airport","class","day_of_month","flight_date","airline_code","airline_name","origin_city",
                  "origin_state","destination_city","destination_state","schedule_departure_hour","departure_delay_in_minutes",
                  "arrival_delay_in_minutes","flight_cancelled","flight_time_in_minutes",
                  "flight_distance","arrival_delay_greater_than_5_minutes") 

data$satisfaction<-substr(data$satisfaction,0,4) # treating 4.00.34 properly

str(data)

data$satisfaction<-as.numeric(data$satisfaction)

data$cust_satisfaction<-as.factor(data$cust_satisfaction)

cust_satisfaction=replicate(length(data$satisfaction), "nil")
cust_satisfaction[data$satisfaction >= 3]="1"
cust_satisfaction[data$satisfaction < 3]="0"
data$cust_satisfaction =cust_satisfaction

# Finding null values

colSums(is.na(data))

data_c<-subset(data,flight_cancelled=="Yes")

head(data_c)

colSums(is.na(data_c))

dim(data_c)

## Replacing NA's in  by mean


data_c$departure_delay_in_minutes[is.na(data_c$departure_delay_in_minutes)]<-round(mean(data_c$departure_delay_in_minutes,na.rm=TRUE))

# Replacing NAs by 0

data_c$flight_time_in_minutes <- replace(data_c$flight_time_in_minutes, is.na(data_c$flight_time_in_minutes),0)

data_c$arrival_delay_in_minutes <- replace(data_c$arrival_delay_in_minutes, is.na(data_c$arrival_delay_in_minutes),0)

data_m<-subset(data,flight_cancelled=="No")

colSums(is.na(data_m))

## Replacing NA's in Arrival_delay_in_minutes by mean



data_m$arrival_delay_in_minutes[is.na(data_m$arrival_delay_in_minutes)]<-round(mean(data_m$arrival_delay_in_minutes,na.rm=TRUE))

## Replacing NA's in fligt_time_in_minutes by mean

data_m$flight_time_in_minutes[is.na(data_m$flight_time_in_minutes)]<-round(mean(data_m$flight_time_in_minutes,na.rm=TRUE))

data_s<-subset(data_m,airline_name=="Southeast Airlines Co. ")

dim(data_s)

colSums(is.na(data_s))

data_a<-subset(data_m, airline_name!="Southeast Airlines Co. " )

dim(data_a)

# Plots

library(ggplot2)

ggplot(data_s,aes(x=age,y=satisfaction))+geom_count()+
stat_summary(aes(y =data_s$satisfaction ,group=1), fun.y=mean, colour="blue", geom="line",group=1)

ggplot(data_a,aes(x=age,y=satisfaction))+geom_count()+
stat_summary(aes(y =data_a$satisfaction ,group=1), fun.y=mean, colour="red", geom="line",group=1)

ggplot(data_s,aes(x=gender,y=satisfaction))+geom_count()+
stat_summary(aes(y =data_s$satisfaction ,group=1), fun.y=mean, colour="blue", geom="point",group=1)

ggplot(data_a,aes(x=gender,y=satisfaction))+geom_count()+
stat_summary(aes(y =data_a$satisfaction ,group=1), fun.y=mean, colour="red", geom="point",group=1)

ggplot(data_s,aes(x=type_of_travel,y=satisfaction))+geom_count()+
stat_summary(aes(y =data_s$satisfaction ,group=1), fun.y=mean, colour="blue", geom="line",group=1)

ggplot(data_a,aes(x=type_of_travel,y=satisfaction))+geom_count()+
stat_summary(aes(y =data_a$satisfaction ,group=1), fun.y=mean, colour="red", geom="line",group=1)

ggplot(data_s,aes(x=class,y=satisfaction))+geom_count()+
stat_summary(aes(y =data_s$satisfaction ,group=1), fun.y=mean, colour="blue", geom="line",group=1)

ggplot(data_a,aes(x=class,y=satisfaction))+geom_count()+
stat_summary(aes(y =data_a$satisfaction ,group=1), fun.y=mean, colour="red", geom="line",group=1)

# Satisfaction Vs Airline Status

ggplot(data_s,aes(x=airline_status,y=satisfaction))+geom_count()+
stat_summary(aes(y =data_s$satisfaction ,group=1), fun.y=mean, colour="blue", geom="line",group=1)

ggplot(data_a,aes(x=airline_status,y=satisfaction))+geom_count()+ 
stat_summary(aes(y =data_a$satisfaction ,group=1), fun.y=mean, colour="red", geom="line",group=1)

# Satisfaction Vs No of flights per annum

ggplot(data_s,aes(x=no_of_flights_per_annum,y=satisfaction))+geom_count()+
stat_summary(aes(y =data_s$satisfaction ,group=1), fun.y=mean, colour="blue", geom="point",group=1)

ggplot(data_a,aes(x=no_of_flights_per_annum,y=satisfaction))+geom_count()+
stat_summary(aes(y =data_a$satisfaction ,group=1), fun.y=mean, colour="red", geom="point",group=1)

# satisfaction Vs year of first flight

data_s$year_of_first_flight<-as.factor(data_s$year_of_first_flight)
data_a$year_of_first_flight<-as.factor(data_a$year_of_first_flight)

ggplot(data_s,aes(x=year_of_first_flight,y=satisfaction))+geom_count(aes(color = ..n..))+
stat_summary(aes(y=data_s$satisfaction ,group=1), fun.y=mean, colour="red", geom="line",group=1)

ggplot(data_a,aes(x=year_of_first_flight,y=satisfaction))+geom_count(aes(color = ..n..))+
stat_summary(aes(y=data_a$satisfaction ,group=1), fun.y=mean, colour="red", geom="line",group=1)

# Satisfaction Vs shopping amount at AIRPORT

ggplot(data_s,aes(x=shopping_amount_at_airport,y=satisfaction))+geom_count()+
stat_summary(aes(y=data_s$satisfaction ,group=1), fun.y=mean, colour="red", geom="point",group=1)


ggplot(data_a,aes(x=shopping_amount_at_airport,y=satisfaction))+geom_count()+
stat_summary(aes(y=data_a$satisfaction ,group=1), fun.y=mean, colour="blue", geom="point",group=1)





ggplot(data_s,aes(x=eat_drink_at_airport,y=satisfaction))+geom_count()+
stat_summary(aes(y=data_s$satisfaction ,group=1), fun.y=mean, colour="red", geom="point",group=1)




ggplot(data_a,aes(x=eat_drink_at_airport,y=satisfaction))+geom_count()+
stat_summary(aes(y=data_a$satisfaction ,group=1), fun.y=mean, colour="blue", geom="point",group=1)






ggplot(data_s,aes(x=departure_delay_in_minutes,y=satisfaction))+geom_count()+
stat_summary(aes(y=data_s$satisfaction ,group=1), fun.y=mean, colour="red", geom="point",group=1)


ggplot(data_a,aes(x=departure_delay_in_minutes,y=satisfaction))+geom_count()+
stat_summary(aes(y=data_a$satisfaction ,group=1), fun.y=mean, colour="blue", geom="point",group=1)





ggplot(data_s,aes(x=arrival_delay_in_minutes,y=satisfaction))+geom_count()+
stat_summary(aes(y=data_s$satisfaction ,group=1), fun.y=mean, colour="red", geom="point",group=1)


ggplot(data_a,aes(x=arrival_delay_in_minutes,y=satisfaction))+geom_count()+
stat_summary(aes(y=data_a$satisfaction ,group=1), fun.y=mean, colour="blue", geom="point",group=1)





ggplot(data_s,aes(x=flight_time_in_minutes,y=satisfaction))+geom_count(aes(color = ..n..))+
stat_summary(aes(y=data_s$satisfaction ,group=1), fun.y=mean, colour="red", geom="point",group=1)


ggplot(data_a,aes(x=flight_time_in_minutes,y=satisfaction))+geom_count(aes(color = ..n..))+
stat_summary(aes(y=data_a$satisfaction ,group=1), fun.y=mean, colour="blue", geom="point",group=1)





ggplot(data_s,aes(x=flight_distance,y=satisfaction))+geom_count()+
stat_summary(aes(y=data_s$satisfaction ,group=1), fun.y=mean, colour="red", geom="point",group=1)


ggplot(data_a,aes(x=flight_distance,y=satisfaction))+geom_count()+
stat_summary(aes(y=data_a$satisfaction ,group=1), fun.y=mean, colour="blue", geom="point",group=1)





ggplot(data_s,aes(x=arrival_delay_greater_than_5_minutes,y=satisfaction))+geom_count(aes(color = ..n..))+
stat_summary(aes(y=data_s$satisfaction ,group=1), fun.y=mean, colour="red", geom="line",group=1)


ggplot(data_a,aes(x=arrival_delay_greater_than_5_minutes,y=satisfaction))+geom_count(aes(color = ..n..))+
stat_summary(aes(y=data_a$satisfaction ,group=1), fun.y=mean, colour="blue", geom="line",group=1)





ggplot(data_s,aes(x=day_of_month,y=satisfaction))+geom_count(aes(color = ..n..))+
stat_summary(aes(y=data_s$satisfaction ,group=1), fun.y=mean, colour="red", geom="line",group=1)


ggplot(data_a,aes(x=day_of_month,y=satisfaction))+geom_count(aes(color = ..n..))+
stat_summary(aes(y=data_a$satisfaction ,group=1), fun.y=mean, colour="blue", geom="line",group=1)

data_s$cust_satisfaction <-as.factor(data_s$cust_satisfaction)
data_a$cust_satisfaction <-as.factor(data_a$cust_satisfaction)

b0<-data.frame(table(data_s$cust_satisfaction))
b0

b1<-ggplot(b0,aes(x=reorder(Var1,Freq),y=Freq, group=1))
b1<-b1+geom_bar(stat="identity",fill="black",width=0.2)
#b1<-b1+theme(axis.text.x=element_text(angle=90,hjust=1))
b1<-b1+xlab("Cust satisfaction")+ylab("NUmber of satisfied customers")+ggtitle("1=Satisfied cutomers,0=not satisfied customer")
b1


b0<-data.frame(table(data_a$cust_satisfaction))
b0

b1<-ggplot(b0,aes(x=reorder(Var1,Freq),y=Freq, group=1))
b1<-b1+geom_bar(stat="identity",fill="black",width=0.2)
#b1<-b1+theme(axis.text.x=element_text(angle=90,hjust=1))
b1<-b1+xlab("Cust satisfaction")+ylab("NUmber of satisfied customers")+ggtitle("1=Satisfied cutomers,0=not satisfied customer")
b1

#Linear Regression On Southeast:

class(s)

lm1<-lm(satisfaction~age+airline_status+price_sensitivity+year_of_first_flight+no_of_flights_per_annum
           +percent_of_flights_with_other_airlines+type_of_travel+no_of_loyalty_cards+shopping_amount_at_airport
           +eat_drink_at_airport+class+day_of_month+flight_date+origin_city+origin_state+destination_city+destination_state
           +schedule_departure_hour+departure_delay_in_minutes+ +arrival_delay_in_minutes+flight_time_in_minutes+flight_distance
           +arrival_delay_greater_than_5_minutes,data=data_s)

summary(lm1)

#Taing significant P values to make a new model

final_lm<-lm(satisfaction~age+airline_status+no_of_flights_per_annum+type_of_travel+eat_drink_at_airport+
             class+schedule_departure_hour+arrival_delay_greater_than_5_minutes,
             data=data_s)

summary(final_lm)

# But we see that Adjusted R square has gone up, which indicates new model is better and we can discard other variables..

# Logistic

logit.m<-glm(cust_satisfaction~age+airline_status+price_sensitivity+year_of_first_flight+no_of_flights_per_annum
           +percent_of_flights_with_other_airlines+type_of_travel+no_of_loyalty_cards+shopping_amount_at_airport
           +eat_drink_at_airport+class+day_of_month
           +schedule_departure_hour+departure_delay_in_minutes+ +arrival_delay_in_minutes+flight_time_in_minutes+flight_distance
           +arrival_delay_greater_than_5_minutes,data=data_s,family=binomial(link='logit'))

summary(logit.m)

#Anova

#The difference between the null deviance and the residual deviance shows how our model is
# doing against the null model (a model with only the intercept). The wider this gap, the better.

anova(logit.m,test="Chisq")

anova(logit.m,test="Chisq")

# Adding Age, airline_status and type_of_travel significantly reduces the residual deviance. 
# The other variables seem to improve the model less even though class and  arrival_delay_greater_than_5_minutes 
# has a low p-value. 
# A large p-value here indicates that the model without the variable explains more or less the same amount of variation. 
# Ultimately what you would like to see is a significant drop in deviance and the AIC.
# Here(logistic regression) doesn't have equivalent R^2 value like in Linear regression,
# but the McFadden R2 index can be used to assess the model fi

logit.final<-glm(cust_satisfaction~age+airline_status+no_of_flights_per_annum+type_of_travel+departure_delay_in_minutes+
                arrival_delay_in_minutes+arrival_delay_greater_than_5_minutes,data=data_s,
           family=binomial(link='logit'))                

summary(logit.m)

anova(logit.final,test="Chisq")

# From both linear and logistic regression we get the sam variables which are significant so now we will use only those variables for prediction


nrow(data_s)

test<-data_s[7000:9000,]

colnames(data_s)

fitted.results <- predict(logit.final,newdata=subset(data_s,select=c(2,3,7,9,12,13,22,23,24,28)),type='response')
fitted.results <- ifelse(fitted.results > 0.3,1,0)
misClasificError <- mean(fitted.results != data_s$cust_satisfaction)

print(paste('Accuracy',1-misClasificError))

# Finding area under the curve

library(ROCR)

p<- predict(logit.m,newdata=subset(test,select=c(2,3,4,5,6,7,8,9,10,11,12,13,14,22,23,24,26,27,28)),type='response')
pr<-prediction(p,test$cust_satisfaction)
prf<-performance(pr,measure="tpr",x.measure="fpr")
plot(prf)

auc<-performance(pr,measure="auc")
auc<-auc@y.values[[1]]
auc

aucraccy<-auc*100.0
aucraccy

#SVM

# For svm we will use only the significant variables:
set.seed(3033)
library(caret)

intrain<-createDataPartition(y=data_s$cust_satisfaction,p=0.7,list=FALSE) #spiting 70-30 
training<-data_s[intrain,]
testing<-data_s[-intrain,]

mytrain<-trainControl(method="repeatedcv",number=10, repeats=3)
set.seed(3233)
# repeatedcv= repeated cross validatio

#preProcess parameter center & scale,
#These two help for centering and scaling the data. 
#After preProcessing these convert our training data with mean value as approximately 0 and standard deviation as 1. 
# The tuneLength parameter holds an integer value. This is for tuning our algorithm.

mysvm<-train(cust_satisfaction~age+airline_status+no_of_flights_per_annum+type_of_travel+departure_delay_in_minutes+
                arrival_delay_in_minutes+arrival_delay_greater_than_5_minutes,
             data=training, 
             method="svmLinear"
            )


summary(mysvm)

mysvm

# SVM class method

library(kernlab)

newtrain <-training[,c(2,3,4,5,6,7,8,9,10,11,12,13,14,22,23,24,26,27,28,29)]
newtest<-testing[,c(2,3,4,5,6,7,8,9,10,11,12,13,14,22,23,24,26,27,28,29)]
head(newtrain$cust_satisfaction)

myOutput<-ksvm(cust_satisfaction ~., data=newtrain, kernel = "rbfdot",kpar="automatic", C=5,cross=3, prob.model=TRUE)


myOutput

# See the output of Support vector histogram to understand outcome
hist(alpha(myOutput)[[1]], main="support vector histogram with C=5",
     xlab="support vector values")

svmPred <- predict(myOutput, newtest, type = "votes")
svmPred
str(svmPred)
head(svmPred)

# Create a confusion matrix (a 2 x 2 table) 
comTable <- data.frame(newtest[ ,20], svmPred[2, ])
table(comTable)

# Calculate an error rate based on what you see in the confusion matrix.
t<-table(comTable)
y<-sum(t[1,1]+t[2,2])/sum(t)

Y<-y*100.0

Y


