#COURSE NUMBER: IST 687
#NAME: Harsh Darji
#SUID: 810131016
# Homework 1 - Submitted by Harsh Darji on September 5, 2018
# Portions of this code came from Introduction to Data Science
# but the comments are all original.

# 1)Define a vector 'grades', which contains the numbers 4.0, 3.3 and 3.7 (i.e., three numbers in the vector 'grades'). 
grades<-c(4.0,3.3,3.7)

# 2)Define a vector 'courseName', which contain the strings "Bio", "Math", "History". 
courseName<-c("Bio","Math","History")

# 3)Define a variable 'BetterThanB', that is equal to 3
BetterThanB<-3

# 4)Compute the average of the grades vector with the mean() function
mean(grades)
#3.666667

# 5)Calculate the number of observations in the grades vector with the length() function, and store the result in the variable 'total.length'
total.length<-length(grades)
# 6)output of total.length
total.length
#3

# 7)Calculate the sum of  'grades' with the sum() function, store the result in 'total'.
total<-sum(grades)
total
#11

# 8)Recompute the average of all the grades by combining questions 2 and 4
total/(total.length)
#3.666667

#9)	Compute the max grades, store the result in 'maxG'
maxG<-max(grades)
#4

#10)	Compute the min grades, store the results in 'minG'
minG<-min(grades)
#3.3

# 11)Create a new vector called betterGrades, which is the grades + 0.3 (each grade improved each grade by  0.3 points)
betterGrades<-c(grades+0.3)
betterGrades

# 12)Compute the average of betterGrades
mean(betterGrades)
#3.966667

# 13)Test if maxG is greater than3.5 (output "yes" or "no")
if(maxG>3.5)"yes" else "no"

# 14)Test if minG is greater than the variable 'BetterThanB'' (output "yes" or "no")
if(maxG>BetterThanB)"yes" else "no"

# 15)Accessing an element in a vector
Syllabus_highlights<-courseName[2]
Syllabus_highlights
#"Math"
