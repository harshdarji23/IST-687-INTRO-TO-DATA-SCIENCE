#COURSE NUMBER: IST 687
#NAME: Harsh Darji
#HOMEWORK-6
#SUBMITTED ON:10 OCTOBER,2018
#DUE ON:11 OCTOBER,2018


#Step A: Load and Merge datasets

#1)	Read in the census dataset (using the function created in HW 3)	
dfStates<-read.csv(url("https://www2.census.gov/programs-surveys/popest/datasets/2010-2017/state/asrh/scprc-est2017-18+pop-res.csv"))
dfStates
dfStates<-dfStates[-1, ]
dfStates<-dfStates[-grep("Puerto Rico Commonwealth", dfStates$NAME),]
colnames(dfStates)
dfStates<-dfStates[,5:8]
names(dfStates)<-c("stateName","population","popOver18","percentOver18")
View(dfStates)

#2)	Copy the USArrests dataset into a local variable (similar to HW 2)
data()
arrests<-USArrests
View(arrests)

#3)	Create a merged dataframe -- with the attributes from both dataset
arrests$stateName<-rownames(arrests)
View(arrests)
df<-merge(dfStates,arrests,by="stateName")
View(df)

#Step B: Explore the Data - Understanding distributions

#installing the gglplot package 
install.packages("ggplot2")
library(ggplot2)

#4)	Create a histogram using ggplot2() for the population and a different histogram for the murder rate

#histogram for population using ggplot2
df_pop<-ggplot(df,aes(x=population))+ #descibes the which dataset to use and the column name
  geom_histogram(binwidth = 500000)+ #gives geometry i.e which to plot histogram/boxplot/scatterplot
  ggtitle("Histogram of population") #gives title to the plot
df_pop

#to create histograms for other variables we have to change the aes
#Also we have to change the binwidth according to the variable to make the histogram look right

#histogram for murder using ggplot2
df_murder<-ggplot(df,aes(x=Murder))+
  geom_histogram(binwidth = 10)+
  ggtitle("Histogram of murder")
df_murder

#histogram for assault using ggplot2
my_assault<-ggplot(df,aes(x=Assault))+
  geom_histogram(binwidth = 20)+
  ggtitle("Histogram of assault")
my_assault

##histogram for Rape using ggplot2
df_rape<-ggplot(df,aes(x=Rape))+
  geom_histogram(binwidth =5)+
  ggtitle("Histogram of Rape")
df_rape


#5)	Create a boxplot for the population, and a different boxplot for the murder rate.

#boxplot for population
df_pop_box<-ggplot(df,aes(x=factor(0),population))+
  geom_boxplot()+
  ggtitle("boxplot")
df_pop_box

#boxplot for murder
df_murder_box<-ggplot(df,aes(x=factor(0),Murder))+geom_boxplot()+ggtitle("Histogram of population")
df_murder_box

#6)	Create a block comment explaining which visualization (boxplot or histogram) you thought was more helpful (explain why)
#Boxplot is more useful than histogram as it provides more information such as min,max,median of the data 
#whereas in histogram it is tedious to find the details


#Step C: Which State had the Most Murders - bar charts

#7)Calculate the number of murders per state
df$numMurders<-df$population*df$Murder/100000
View(df)

#8)Generate a bar chart, with the number of murders per state
murder_bar<-ggplot(df,aes(reorder(stateName,numMurders),y=numMurders))+
  geom_col()+
  ggtitle("Histogram of murder")
murder_bar


#9)	Generate a bar chart, with the number of murders per state. Rotate text (on the X axis), so we can see x labels, also add a title named "Total Murders".

murder_bar_1<-ggplot(df,aes(x=stateName,y=numMurders))+
  geom_col()+
  theme(axis.text.x=element_text(angle=90, hjust=1))+
  ggtitle("Total murders")
murder_bar_1

#Generate a new bar chart, the same as in the previous step, but also sort the x-axis by the murder rate
murder_bar_ordered<-ggplot(df,aes(x=reorder(stateName,numMurders),y=numMurders))+
  geom_col()+
  theme(axis.text.x=element_text(angle=90, hjust=1))+
  ggtitle("Total murders")
murder_bar_ordered


#11)Generate a third bar chart, the same as the previous step, but also showing percentOver18 as the color of the bar
percent_over_18_plot<-ggplot(df,aes(x=reorder(stateName,numMurders),y=numMurders))+
  geom_col(aes(fill=percentOver18))+
  theme(axis.text.x=element_text(angle=90, hjust=1))+
  ggtitle("Total murders")
percent_over_18_plot


#Step D: Explore Murders - scatter chart

#12)Generate a scatter plot - have population on the X axis, the percent over 18 on the y axis, and the size & color represent the murder rate
scatter_plot<-ggplot(df, aes(x=population, y=percentOver18)) + 
  geom_point(aes(size=Murder, color=Murder))
scatter_plot




