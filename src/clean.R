# setwd("~/projects/CS567_Project_2")

library(plyr)

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