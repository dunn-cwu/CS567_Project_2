# setwd("~/projects/CS567_Project_2")
# setwd("C:/Users/Coder/OneDrive/Documents/CS 567 Computational Stats/CS567_Project_2")

library(plyr)
library(standardize)
library(stringr)

# data cleaning
data = read.csv("./dat/Wildfires_WA_dataset.csv", header = TRUE)

# translate Yes and No to 1 and 0
data$Evacuations <- revalue(data$Evacuations, c("Yes" = as.numeric(1)))
data$Evacuations <- revalue(data$Evacuations, c("No"  = as.numeric(0)))
data$Evacuations <- revalue(data$Evacuations, c("No " = as.numeric(0)))

# calculate duration
data$duration = 
  as.Date(as.character(data$Date.contained), format="%d-%b-%Y")-
  as.Date(as.character(data$Date.Start)    , format="%d-%b-%Y")

counties<- c(str_split(unique(data$County), ","))
causes  <- c(str_split(unique(data$Cause) , ","))

# separating the counties
for (county in counties)
{
  data[,county] <- as.numeric(0)
}

# separating the causes
for (cause in causes)
{
  data[,cause] <- as.numeric(0)
}

for (row in 1:nrow(data))
{
  # There's probabely a better than this:
  if (data[row, "Evacuations"] == 0 )
  {
    data[row, "X..of.Evacuated"] = as.numeric(0)
  }
  
  data[row, paste(data[row, "County"])] <- as.numeric(1)
  data[row, paste(data[row, "Cause"])]  <- as.numeric(1)
}

# remove unnecessary column
# NOTE: X..of.Sheltering.in.place is removed because it only has two rows of data, but otherwise NA
data = subset(data, select=-c(County, Cause, Fire.Name, Date.Start, Date.contained, X..of.Sheltering.in.place))

# Remove ',' from values in acres and total costs columns then convert to numeric
data$Size..acres. <- as.numeric(gsub(",","",data$Size..acres.))
data$Estimated.Total.Cost <- as.numeric(gsub(",","",data$Estimated.Total.Cost))

# Make sure all columns are numeric
data <- sapply(data, as.numeric, na.rm=TRUE) 

# NOTE: the na.rm=TRUE, above, takes care of the evacuations that occured be is NA
# However, when sapply is used here it increments the values in Evacuations (assuming we care if normalized)

#View(data)

# Standardize all columns
sdata = as.data.frame(scale(data))

# View standardized dataframe
#View(sdata)

# Backwards Elimination Example Used From:
# https://www.youtube.com/watch?v=0aTtMJO-pE4
FitAll <- lm(Estimated.Total.Cost ~ . , data=sdata, na.action=na.exclude)

#summary(FitAll)

# Do backwards elimination of columns (Eliminates non-significant columns)
step(FitAll, direction = "backward")
