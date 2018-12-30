# IST687  HW 10  - Text Mining
# NAME:HARSH DARJI 
# Due date- 11/29/2018
# Submitted Date-11/28/2018
#Part A: Load and condition the text file that contains the speech  
#1.	The data is available on blackboard, as a JSON file (see HW8 if you need a reminder on the dataset or how to load the dataset).
#2.	The key column to focus on is the 'freeText' column.

install.packages("RCurl")

install.packages("ggplot2")

install.packages("jsonlite")

library(RCurl)
library(jsonlite)
library(ggplot2)

getwd()

hotelresults=fromJSON("C:/Users/Harsh/Desktop/hotelSurveySherison.json")

hotelresults=data.frame(hotelresults)

hotelresults<-data.frame(hotelresults$freeText)

hotelresults

sba<-write.table(hotelresults,"e:/mydata.txt")

sba<-scan("e:/mydata.txt",character(0),sep="\n")


sba

head(sba,3)
#Part B: Create a list of word counts from the speech
#3.	Starting with the code at the bottom of page 180 in the text book, use a similar approach to transform the free text into a term document matrix, and then determine positive and negative word matches.
#4.	Calculate the percent positive words and negative words.
#5.	Write a block comment that summarizes what you learned from ratioPos and ratioNeg.

install.packages("tm")

library(tm)

words.vec<-VectorSource(sba)


words.corpus<-Corpus(words.vec)

words.corpus

words.corpus<-tm_map(words.corpus,content_transformer(tolower))

words.corpus<-tm_map(words.corpus,removePunctuation)

words.corpus<-tm_map(words.corpus,removeNumbers)

words.corpus<-tm_map(words.corpus,removeWords,stopwords("english"))

tdm<-TermDocumentMatrix(words.corpus)

tdm

inspect(tdm)

m<-as.matrix(tdm)

m<-as.matrix(tdm)
wordCounts<-rowSums(m)
wordCounts<-sort(wordCounts,decreasing=TRUE)
head(wordCounts)
cloudFrame<-data.frame(word=names(wordCounts),freq=wordCounts)
wc<-wordcloud(cloudFrame$word,cloudFrame$freq)

wordCounts<-rowSums(m)

wordCounts<-sort(wordCounts,decreasing=TRUE)

head(wordCounts)

cloudFrame<-data.frame(word=names(wordCounts),freq=wordCounts)
#Part D: Visualize the results
#6.	Create a word cloud
#7.	Create a barplot of the positive and negative words that matched (at least twice)
#8.	Write a block comment on what you observe from these two barplots and the wordcloud. 
#9.	Does these results make sense to you in terms of the kinds of emotions you see?
  #Which do you think is more informative - barplot or the wordcloud?
  
install.packages("wordcloud")

library("wordcloud")

wc<-wordcloud(cloudFrame$word,cloudFrame$freq)

wordcloud(names(wordCounts),wordCounts,min.freq=2,max.words=50,rot.per=0.35,colors=brewer.pal(8,"Dark2"))

pos<-"C:/Users/Harsh/Desktop/positive-words.txt"

neg<-"C:/Users/Harsh/Desktop/negative-words.txt"

p<-scan(pos,character(0),sep="\n")

n<-scan(neg,character(0),sep="\n")

head(p,10)

p<-p[-1:-34]

n<-n[-1:-34]

head(p,10)

head(n,10)

totalWords<-sum(wordCounts)

words<-names(wordCounts)
words

matched<-match(words,p,nomatch=0)

head(matched,10)

matched[4]

p[772]

words[4]

p[857]
words[8]

p[832]
words[9]

mCounts<-wordCounts[which(matched!=0)]
mCounts

length(mCounts)

mWords<-names(mCounts)

nPos<-sum(mCounts)
nPos

matched1<-match(words,n,nomatch=0)

head(matched1,10)

nCounts<-wordCounts[which(matched1!=0)]
nCounts



nNeg<-sum(nCounts)

nWords<-names(nCounts)

nNeg

length(nCounts)

totalWords<-length(words)

ratioPos<-nPos/totalWords

ratioPos

ratioNeg<-nNeg/totalWords

ratioNeg

# 5.	Write a block comment that summarizes what you learned from ratioPos and ratioNeg.
# ratioPos gives a very good description about number of positive words that are use in th free text  whereas 
# ratioNeg gives a very good description about number of negative words that are use in th free text
# We can easily conclude here the customer have used more of positive words thus interpreting satisfaction.

install.packages("dplyr")

a<-as.matrix(mCounts)

a1<-rowSums(a)

a1<-sort(a1,decreasing=TRUE)

head(a1)

a2<-data.frame(word=names(a1),freq=a1)

wordcloud(a2$word,a2$freq,min.freq=1)

b<-as.matrix(nCounts)
b1<-rowSums(b)
b1<-sort(b1,decreasing=TRUE)
b2<-data.frame(word=names(b1),freq=b1)
wordcloud(b2$word,b2$freq,min.freq=1)

 # merging 2 df

#install.packages("gdata", dependencies=TRUE)

#library(gdata)
#concat_data <- cbindX(a2, b2)

#concat_data

#colnames(concat_data)[3]<-"wordn"
#colnames(concat_data)[4]<-"freqn"

#head(concat_data)

#concat_data[!is.na(concat_data$wordn), ]

#concat_data

pltdata<-a2[a2$freq>1,]

pltdata1<-b2[b2$freq>1,]

pltdata

g1<-ggplot(data=pltdata,aes(x=word,y=freq))+geom_bar(stat="identity")

 
g1+theme(axis.text.x=element_text(angle=90,hjust=1))

g2<-ggplot(pltdata1,aes(x=word,y=freq))+geom_bar(stat="identity")

g2+theme(axis.text.x=element_text(angle=90,hjust=1))

#1.	Write a block comment on what you observe from these two barplots and the wordcloud.
# Wordcloud helps to look at all the words together whereas bar plot helps to get a numberof counts of each word. It is observed that
# friendly was used more often in positive words and bad/ terrible were used to descibe dissayisfaction as negative word.

#9.	Does these results make sense to you in terms of the kinds of emotions you see?
#Which do you think is more informative – barplot or the wordcloud?
# Yes the results makes sense. It shows people with satisfaction use more positive words and disatisfied customer uses negative words
# more often which indicates the rating.
# In my opinion Word cloud is more informative and appealing. Even someone who is not in data can interpret it without looking for extra inf
# Word cloud can be mademore fancy by adding colors. Words are frequently used are BOLD which helps in understanfing customer behaviour.

install.packages("kernlab")

#Part E: Evaluate Happy and not Happy customer responses
##11.	Redo Steps B, C & D, for these two subsets of the text strings.
#12.	Compare the positive and negative ratios for these two different group of customers 

library(kernlab)

hotelresults1=fromJSON("C:/Users/Harsh/Desktop/hotelSurveySherison.json")

hotelresults1=data.frame(hotelresults1)

newrep=replicate(length(hotelresults1$overallCustSat), "nil")
newrep[hotelresults1$overallCustSat >= 8]="Happy"
newrep[hotelresults1$overallCustSat < 8]="notHappy"
hotelresults1$HappyCustomer=newrep
hotelresults1$HappyCustomer


#hotelresults1=fromJSON("C:/Users/Harsh/Desktop/hotelSurveySherison.json")

#hotelresults1=data.frame(hotelresults1)

hotelresults1


dim(hotelresults1)

x1<-hotelresults1[which(hotelresults1$overallCustSat>=8),]

head(x1)

x2<-hotelresults1[which(hotelresults1$overallCustSat<8),]

head(x2)

x11<-data.frame(x1$freeText)

sbxa<-write.table(x11,"e:/mydata11.txt")

sbxa<-scan("e:/mydata11.txt",character(0),sep="\n")

sbxa

words.vec<-VectorSource(sbxa)
words.corpus<-Corpus(words.vec)
words.corpus
words.corpus<-tm_map(words.corpus,content_transformer(tolower))
words.corpus<-tm_map(words.corpus,removePunctuation)
words.corpus<-tm_map(words.corpus,removeNumbers)
words.corpus<-tm_map(words.corpus,removeWords,stopwords("english"))
tdm1<-TermDocumentMatrix(words.corpus)
tdm1
inspect(tdm1)

x11<-as.matrix(tdm1)
wordCountsx1<-rowSums(x11)
wordCountsx1<-sort(wordCountsx1,decreasing=TRUE)
head(wordCountsx1)
cloudFrame<-data.frame(word=names(wordCountsx1),freq=wordCountsx1)
wcx1<-wordcloud(cloudFrame$word,cloudFrame$freq)

wordsx1<-names(wordCountsx1)
wordsx1

matchedx1<-match(wordsx1,p,nomatch=0)

mCountsx1<-wordCountsx1[which(matchedx1!=0)]
mCountsx1

matched1x1<-match(wordsx1,n,nomatch=0)

nCountsx1<-wordCountsx1[which(matched1x1!=0)]
nCountsx1

nPosx1<-sum(mCountsx1)
nPosx1

nNegx1<-sum(nCountsx1)
nNegx1

posratio=nPosx1/totalWords
posratio

negratio=nNegx1/totalWords
negratio

c<-as.matrix(mCountsx1)
c1<-rowSums(c)
c1<-sort(c1,decreasing=TRUE)
c2<-data.frame(word=names(c1),freq=c1)
wordcloud(c2$word,c2$freq,min.freq=1)

d<-as.matrix(nCountsx1)
d1<-rowSums(d)
d1<-sort(d1,decreasing=TRUE)
d2<-data.frame(word=names(d1),freq=d1)
wordcloud(d2$word,d2$freq,min.freq=1)

pltdata2<-c2[c2$freq>1,]

pltdata2

gg1<-ggplot(data=pltdata2,aes(x=word,y=freq))+geom_bar(stat="identity") 
gg1+theme(axis.text.x=element_text(angle=90,hjust=1))

pltdata3<-d2[d2$freq>0,] #There are no words with freq>1 so we plot freq=1

pltdata3

gg2<-ggplot(data=pltdata3,aes(x=word,y=freq))+geom_bar(stat="identity") 
gg2+theme(axis.text.x=element_text(angle=90,hjust=1))

x12<-data.frame(x2$freeText)

sbxa2<-write.table(x12,"e:/mydata11x2.txt")

sbxa2<-scan("e:/mydata11x2.txt",character(0),sep="\n")

words.vec<-VectorSource(sbxa2)
words.corpus<-Corpus(words.vec)
words.corpus
words.corpus<-tm_map(words.corpus,content_transformer(tolower))
words.corpus<-tm_map(words.corpus,removePunctuation)
words.corpus<-tm_map(words.corpus,removeNumbers)
words.corpus<-tm_map(words.corpus,removeWords,stopwords("english"))
tdm1x2<-TermDocumentMatrix(words.corpus)
tdm1x2
inspect(tdm1x2)

x11x2<-as.matrix(tdm1x2)
wordCountsx1x2<-rowSums(x11x2)
wordCountsx1x2<-sort(wordCountsx1x2,decreasing=TRUE)
head(wordCountsx1x2)
cloudFrame<-data.frame(word=names(wordCountsx1x2),freq=wordCountsx1x2)
wcx1x2<-wordcloud(cloudFrame$word,cloudFrame$freq)

wordsx1x2<-names(wordCountsx1x2)
wordsx1x2


matchedx1x2<-match(wordsx1x2,p,nomatch=0)

mCountsx1x2<-wordCountsx1x2[which(matchedx1x2!=0)]
mCountsx1x2

nPosx1x2<-sum(mCountsx1x2)
nPosx1x2

posratiox1=nPosx1x2/totalWords
posratiox1

e<-as.matrix(mCountsx1x2)
e1<-rowSums(e)
e1<-sort(e1,decreasing=TRUE)
e2<-data.frame(word=names(e1),freq=e1)
wordcloud(e2$word,e2$freq,min.freq=1)

matched1x1x2<-match(wordsx1x2,n,nomatch=0)

nCountsx1x2<-wordCountsx1x2[which(matched1x1x2!=0)]
nCountsx1x2

nNegx12<-sum(nCountsx1x2)
nNegx12

negratiox1x2=nNegx12/totalWords
negratiox1x2

f<-as.matrix(nCountsx1x2)
f1<-rowSums(f)
f1<-sort(f1,decreasing=TRUE)
f2<-data.frame(word=names(f1),freq=f1)
wordcloud(f2$word,f2$freq,min.freq=1)

pltdata4<-e2[e2$freq>1,]

ggx1<-ggplot(data=pltdata4,aes(x=word,y=freq))+geom_bar(stat="identity") 
ggx1+theme(axis.text.x=element_text(angle=90,hjust=1))

pltdata5<-f2[f2$freq>1,]

ggx2<-ggplot(data=pltdata5,aes(x=word,y=freq))+geom_bar(stat="identity") 
ggx2+theme(axis.text.x=element_text(angle=90,hjust=1))
