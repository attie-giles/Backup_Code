#====================================================================================
# Module 1 : Intro to R and basic data manipulations
#====================================================================================
# Welcome to your first R script. Scripts are set of sequentially executed commands.
#====================================================================================


#Clear the environment, optional
remove(list=ls())

# Please familiarize yourself with the elements of RStudio you can see. 
# Q: What does "Run" do in the upper right?

#-----------------------------------------------------------------------------------
# Some basic mathematical operations

# R console can be used as a fancy calculator
4+3
4^3.3/log(4) 
# %% remainder operator, exp() exponent, log10 logarithm base 10
# TRY THIS: Use the remainder operator to divide 345 by 7 and find the remainder
# TRY THIS: Use the log10 operator to find log base 10 of 10, 50, and 100


# Q: what does exp(log(4)) do? 

#-----------------------------------------------------------------------------------
# Vectors

# The simplest data structure is a vector. 
c(1,3,4)
c("a", "b", "w")
c(3:6)

# Vectors are dimensionless they are not nx1 or 1xn

# Most mathematical operations can be applied to data structures. 

numbers <- c(1,2,3,4,5,6,7,8,9,10) 

# Note: that the above assignment operator "<-" creates an object in the Environment window but does not display its content
# R automatically overrides values on assignment.

# TRY THIS: numbers <- c(1:10) or this numbers <- seq(1,10,by=1) or this numbers <- (1:10)
numbers #display the content of the vector


# TRY THIS: Create your own vector with 7 elements starting from 7 and increasing by 3 that you can use throughout the class. 
numbers <- seq(from= 1,by= 3, length.out= 10)

# Let's shuffle the vector, note saving into the same vector
numbers <- sample(numbers)
numbers


# =============================================================================
# Operations apply to vectors

# The operations are of two kinds:

# 1. Mutating: takes a vector and produces a vector of the same size
11-numbers 
numbers^2 #square every element
log(numbers) # take natural logarithm
# Note: the above commands produce a vector that has the same dimension as the original vector

# 2. Summarizing: takes a vector and produces a number: sum(), mean(), or several numbers: range()
sum(numbers) # total sum
mean(numbers) # average
length(numbers) # number of elements
range(numbers)
# Note: the above commands reduce a vector to one number

# TRY THIS: Perform various calculations on your vector 

# Q: What do you think the following commands do?

sum(numbers)/length(numbers)

numbers-sum(numbers)/length(numbers)

mean(numbers-sum(numbers)/length(numbers))

# Q: What do you think the following command does?
round( numbers/sum(numbers) * 100, digits =2)

# TRY THIS: Calculate share of each observation in the total sum of your vector.

# Q: Verify that \sum_{i}(x_i-\bar{x})y_i = \sum_{i}(x_i-\bar{x})(y_i-\bar{x})

numbers <- runif(10, min=1, max=10)

#-----------------------------------------------------------------------------------
# String operations
paste("1990","01", sep="-")
toupper("spain")
substr("University",1,4)
sub("A","U","ARARAT")
gsub("A","U","ARARAT")


#-----------------------------------------------------------------------------------
# Logical operations

# Simple comparison
3<4 
3==4
3<=4
3<=3
3!=4

# TRY THIS: Compare two numbers using <= to generate FALSE

# string comparison (lexicographic)
"a">"b"
"a"<"b"
"ab">"b"

# comparing vectors to numbers
numbers<5 #comparison of a vector and a number. The number is compared to every element.
2<c(3,5,6,1) # the order does not matter
c(2,2)<c(3,5) # comparison of two equal vectors
c(2,4)<c(3,5,6,1) # comparison of two unequal vectors (the smaller vector is repeated until it reaches the length of the longer vector, the longer vector has to be a multiple)
c(2,2)<c(3,5,6,1,5) # Why does this not work?

# For more complex expressions: 
# ! - NOT, | - OR, & is AND.

# Complex logical expressions
3>=(9/3) & 4==5
3>=(9/3) | 4==5
(3<4) & (4>3)
numbers>3 & numbers%%2==0 # are numbers even and greater than 3?
numbers<3 | numbers>8 # is a number smaller than 3 or larger than 8?
numbers==max(numbers)  # is it the largest odd number

# Finding number positions of the elements in a vector that satisfy a condition
which(numbers*2>=6) # where is the number that is multiplied by 2 equals 6?
numbers*2>=6 # Note how this is different

# Summarizing logical functions (take a vector as an input)
# We can also test if any or all elements of a vector satisfy a condition
c(1:10)>=5 # makes an element by element comparison
any(c(1:10)>5) # summarizes the comparison into one 
all(c(1:10)>5) # summarizes the comparison into one

5 %in% c(3,4,6,7) # is 5 in the vector? 
c(5,4) %in% c(3,4,6,7) # check each element of the vector for belonging 
any(c(5,4) %in% c(3,4,6,7))

c(1:10)>11
any(c(1:10)>11)
all(c(1:10)>11)


# Those can also be inverted
!any(c(1:10)>5) # ! is the logical NOT that inverts True to False
!any(c(1:10)>11)

!all(c(1:10)>5)
!all(c(1:10)>11)

# Note: The results of the logical test can be stored just like vectors of numeric or string variables
testresult <- c(2,2,2,2) <= c(1,2,3,4) # Positions of elements that are greater than 2
testresult_alt <- 2 <= 1:4
identical(testresult,testresult_alt) # identical() compares two vectors element by element

# Q: Let's test our understanding. What do the next two commands produce?
TRUE | FALSE
TRUE & FALSE
FALSE & FALSE
TRUE & !FALSE | (FALSE==FALSE)

# TRUE/FALSE evaluates to 0/1 inside mathematical expressions
# Note: Results of logical operations can be used in calculations because R evaluates (or coerses) TRUE to 1 and FALSE to 0 
(3<4) + (4>3) # returns 2 because both conditions are true and are coerced to 1
(3<4) + (4>24) # returns 1 because one condition is true
sum(c(3>4,4==3,5>2)) # count how many conditions are true
mean(c(3>4,4==3,5>2)) # what is the share of conditions that are true
mean(testresult) # share of observations that passes the test from above

# The reverse is not always true: any number other than zero evaluates to TRUE and only zero to FALSE
all(1:4)
any(0:4)
# but
-2:2==TRUE
-2:2==FALSE

# Logical vectors can also be explicitly changed to 0 and 1
numbers>2 
(numbers>2)*1 # coercion happens * function * is applied
# this coercion rule makes it easy to calculate shares or counts that satisfy a condition:
sum(numbers>3) #count
mean(numbers>3) #share

#-----------------------------------------------------------------------------------
# Working with a portion of an object SUBSETTING
#-----------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------
# Subsetting vectors by indexing

# create a new vector with numbers from 11 to 20 in random order
numbers <- sample(c(11:20)) 
numbers
# TRY THIS: create your own vector with numbers from 20 to 35 in increments of 3 where elements are in random order

# Note: assigning a new vector to the same name overrides the existing without a warning

numbers[4] # pick one element
numbers[-5] # exclude one element 

# TRY THIS: select one element from your number.

# Referencing elements of a vector can be done by providing a list of positions or indexes

numbers[c(3:5)] # pick a range of elements
numbers[3:5] # pick a range of elements
numbers[c(2,5)] # pick several elements

numbers[-c(2:9)] # exclude several elements
numbers[c(-1,-(2:9))] # exclude several elements

# TRY THIS: Select two elements from your vector.

# Sequential referencing

# Referencing can be sequential
numbers[-1][1] #The first element of the sub-vector without the first element
numbers[-1][seq(1,5, by=2)] #The 1st, 3rd, 5th element of the sub-vector without the first element
numbers[-1][1:5][5] # 5th element of the 5 element sub-vector of the of the sub-vector without the first element

# How shift the vector by one
lag(numbers)

#-----------------------------------------------------------------------------------
# Subsetting vectors by logical conditions
numbers[numbers>6] #return numbers that are larger than 14
numbers[which(numbers>6)] # pick elements that are larger than 15 another way
numbers[numbers %in% c(12,16,18)] # pick elements that match a vector
numbers[numbers%%2==0] #return even numbers
numbers[numbers==max(numbers)] #return the largest number of the vector
# Q: How is the above command different from simply max(numbers)?

# Here is one more way to pick the largest number
numbers[rank(numbers)==length(numbers)]

# Here is a more involved application of indexing with a logical vector
numbers[c(T,T,F)] #remove every third element


#-----------------------------------------------------------------------------------
# Even though most data consists of vectors in most empirical applications we deal with collections of vectors.
# Matrix is an often useful data type that represents a collection of vectors of the same type. So, 
# matrix is a two dimensional data structure where all elements are of the same type.

# Vectors can be combined into a matrix as rows or columns
rbind(1:3,5:7)
cbind(1:3,5:7)

# Columns can have names
matrix1<-cbind(A=1:3,B=5:7,C=2:4) 
matrix1

matrix1<-cbind(A<-1:3,B=5:7,C=2:4) 

# Matrix can also be created by specifying dimensions

# Example: Let's generate a square matrix of random numbers
matrix1 <- matrix(
                  round(runif(n=25,min=1, max=20), digits=0), 
                  nrow = 5, 
                  ncol = 5)
matrix1 


# Subsetting a matrix is done with two numbers MATRIX[ROW,COLUMN]
matrix1[1,2]
matrix1[,2] # entire second column
matrix1[2,] # entire second row
matrix1[2:3,1:2] # first two rows and first two columns
# Select a column with name B
matrix1[,which(colnames(matrix1)=="B")]


# Most operations that can be done element by element on a vectors can be done on matrices
matrix1^2
matrix1>5 # logical operations do the same
matrix1[matrix1>10] #Note this creates a vector why do you think this is the case?

#====================================================================================
# From matrices to lists and data frames

# The limitation of matrices is that the data has to be of the same kind. Lists build on this shortcomming by allowing different data types

# ----------------------------------------------------------------
# Lists 

# A more general data structure that can contain elements of different lengths and of different types

# c() creates a vector, matrix() creates a matrix, list() creates a list


#Try this: 
c("a",1,TRUE) # all elements are converted or rather coerced  into string. Why string?
list("a",1,TRUE) # three elements all preserving their type.
matrix(c("a",1,TRUE), 3, 3)
somelist <- list(String=c("a","b","c"),
                 Numeric=c(1:10),
                 Logical=TRUE) # the elements don't even have to be of the same length! Would it make sense in a data frame?
somelist

# Subsetting lists

somelist$Numeric[4] # produces the 4th element of the "Numeric" vector, which is a part of the list


# The elements of the list are referred to as [[1]] and the elements of those elements are further referred to as [2]
somelist[[1]][2] # second element of the first list element
somelist[[1]]
somelist[1]
somelist[1][2] # second element of the part of the list that has only first element (does not make sense)

# We can also use the string name ...
somelist[["Numeric"]][4]
# TRY THIS: Show the third letter of the "Numeric" variable

# TRY THIS: Change the name of an element
names(matrix1)
names(somelist)

#NULL removes an element
somelist$String <- NULL

# TRY THIS: Can you remove an observation

# Preview: The main data structure that we will use are data frames. Data frames are like matrixes
# and have two dimensions but they are also like lists because they can have elements of different types.


# ---------------------------------------------------------
c()
matrix()
list()
sample()
round(, digits=)
sum()
length()
mean()
which()
runif()
[[]]
[]
any()
all()
==
>=
<=
!=
seq(from =,to= , by=)
identical()
rank()

