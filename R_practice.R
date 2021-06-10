# Hash works for commenting the code.
# to run a particular line here, use "Ctrl +Enter" or "Ctrl + R"
library(readr)

# Read .csv data into a dataframe
education <- read_csv(file.choose())

# To get help with any function we can use '?' Eg., ?read_csv

View(education)

# First moment Business decisions...Find Mean

mean(education$workex)
# We can attach the dataframe diferectly instead of referring it with dataframe$column all the time

attach(education)
mean(workex)

xyz=45
# To remove a particular object from Memory usage use rm
rm(xyz)
# To remove all objects from memory
rm(list=ls())

median(workex)

mode(workex) # Mode in R is already reserved for Storage mode 

# So, for mode we write a custom function
modes <- function(x){
  ux<-unique(x)# Gets all the unique values in a series X
  tab<-tabulate(match(x,ux)) # Gives the counts of all unique values in the series
  ux[tab==max(tab)]# gets the Value(s) with maximum count values
}

y <- c(19,4,5,7,29,19,19,19,19,29,13,25,29,5,5,5,5,4,4,4,4)
modes(y)
modes(workex)


# Second Moment Business Decisions  / Measures of dispersion

var(workex) # Gives the variance 
sd(workex)  # Gives Standard deviation
range <- max(workex)-min(workex)  # Getting the range 
range  # Diplaying the range     

# Including the library moments 

library(moments)

# Third Moment Business Decisions / Skewness of the Data
skewness(workex)
# Positive Skewness means the data is concentrated to wards left side and it is right tailed
# Negative Skewness means the data is concentrated towards Right side and it is left tailed.
# Zero Value for skewness means the data distribution is Normal

hist(workex)

#Fourth Moment of Business Decision / Kurtosis:

kurtosis(workex)

# Positive Kurtosis means the distribution  is peaking and narrower.
# Negative Kurtosis means the distribution is  flat and wider
# Zero Value means the distribution is Normal

#*********GRAPHICAL REPRESENTATION**********
barplot(gmat) # Shows the GMAT scores of each record in a Bar plot
dotchart(gmat) # Shows How many records are there in a given GMAT score

hist(gmat) # Shows the density distribution  of GMAT scores

boxplot(gmat) # Shows the Inter quartile range and if there are any outliers in the data
z<-boxplot(gmat)
z$out# Displays the outlier values in the data


#Probability Distribution

library("UsingR")
densityplot(gmat) # Shows the Probability Density Plot in a linear Graph

# "A Density Plot visualizes the distribution of data over a continuous interval or time period. 
# This chart is a variation of a Histogram that uses kernel smoothing to plot values, allowing for smoother distributions
# by smoothing out the noise. The peaks of a Density Plot help display where values are concentrated over the interval.
# 
# An advantage Density Plots have over Histograms is that they're better at determining the distribution shape 
# because they're not affected by the number of bins used (each bar used in a typical histogram). 
# A Histogram comprising of only 4 bins wouldn't produce a distinguishable enough shape of distribution 
# as a 20-bin Histogram would. However, with Density Plots, this isn't an issue"

# Normal Quantile-Quantile Plot

# A Quantile-Quantile Plot is used to determine if the distribution is normal or not
# On X-axis Theoretical Quantiles/ Standardized values are considered
# On Y-axis Actual Values / Sample Quantities are considered
# If the data is a straight line, then the distribution is normal
# If the data is exponential, Log transformation may be considered to normalize 
qqnorm(gmat)
qqline(gmat)

qqnorm(workex)
qqline(workex)

# Applying log transformation to the  workex to normalize the distribution.

qqnorm(log(workex))
qqline(log(workex))


##################################################################################################################################
#                                             DATA PRE-PROCESSING in R
##################################################################################################################################


####TYPECASTING####
"Type Casting is used to convert one data type into another."
# Example: Categorical data into numerical


rm(list=ls()) # removing all existing variables from the memory

# Loading ethnic Diversity dataset

data<-read.csv(file.choose())

# Checking structure and summary of the data
str(data) # Gives the data metainfo
summary(data) # Gives the data summary statistics.

attach(data) # attaching data to directly refer to the column names instead of dataframe$column

data$age<-as.numeric(data$age) # Typecasting the age data, which was int as numeric

# To check if a given column is in certain data type:
is.numeric(age) # Returns True / False
is.integer(Salaries)

# All categorical data types are mentioned in Character data type. 
str(data) # Eg., Sex is a category of "M" and "F" where but in Chr Type

# Converting the Categorical data as factors will help us in performing statistical analysis
data$Sex<-as.factor(data$Sex)
str(data) # The Data type gets updated to Factor w/ 2 levels "F","M"


# Alternatively we can set conversion of chr to factors while importing data itself
data1<-read.csv(file.choose(),stringsAsFactors = TRUE)
str(data1)
summary(data1)

# This way even the names are converted as factors



#### Handling Duplicates####

# Duplicate entries are handled using duplicated function, which stores the duplicated values
