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
