# setwd("~/projects/CS567_Project_2")
# setwd("C:/Users/Coder/OneDrive/Documents/CS 567 Computational Stats/CS567_Project_2")

library(plyr)
library(standardize)

# data cleaning
data = read.csv("./dat/Wildfires_WA_dataset.csv", header = TRUE)
# replace NA data with 0
data[is.na(data)] = 0
# translate Yes and No to 1 and 0
data$Evacuations <- revalue(data$Evacuations, c("Yes"=1))
data$Evacuations <- revalue(data$Evacuations, c("No"=0))
# calculate duration
data$duration = as.Date(as.character(data$Date.contained), format="%d-%b-%Y")-
  as.Date(as.character(data$Date.Start), format="%d-%b-%Y")
# remove unnecessary column
data = subset(data, select=-c(Fire.Name,Date.Start,Date.contained))

# Remove ',' from values in acres and total costs columns then convert to numeric
data$Size..acres. <- as.numeric(gsub(",","",data$Size..acres.))
data$Estimated.Total.Cost <- as.numeric(gsub(",","",data$Estimated.Total.Cost))

# Make sure all columns are numeric
data <- sapply(data, as.numeric)

# Standardize all columns
sdata = as.data.frame(scale(data))

# View standardized dataframe
View(sdata)

# Backwards Elimination Example Used From:
# https://www.youtube.com/watch?v=0aTtMJO-pE4
FitAll <- lm(Estimated.Total.Cost ~ . , data=sdata)

summary(FitAll)

# Do backwards elimination of columns (Eliminates non-significant columns)
step(FitAll, direction = "backward")
