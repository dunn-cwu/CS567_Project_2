# setwd("~/projects/CS567_Project_2")
# setwd("C:/Users/Coder/OneDrive/Documents/CS 567 Computational Stats/CS567_Project_2")

library(plyr)
library(standardize)
library(stringr)
library(ggplot2)

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

data<-na.omit(data) 

# Make sure all columns are numeric
data <- as.data.frame(sapply(data, as.numeric))

# NOTE: the na.rm=TRUE, above, takes care of the evacuations that occured be is NA
# However, when sapply is used here it increments the values in Evacuations (assuming we care if normalized)

#View(data)

sdata = subset(data, select=-c(Estimated.Total.Cost))
# Standardize all columns
sdata = as.data.frame(scale(sdata))
sdata = cbind(sdata,  subset(data, select=c(Estimated.Total.Cost)))

# View standardized dataframe
#View(sdata)

# Backwards Elimination Example Used From:
# https://www.youtube.com/watch?v=0aTtMJO-pE4
FitAll <- lm(Estimated.Total.Cost ~ . , data=sdata)

#summary(FitAll)

# Do backwards elimination of columns (Eliminates non-significant columns)
step(FitAll, direction = "backward")

selData = sdata[c("Estimated.Total.Cost","Resources.assigned..personnel.", "Structures.lost", "Lewis", "Deaths", "Injuries..Responders.",
                  "Ferry", "Chelan", "Whatcom", "X..of.Evacuated", "Okanogan", "Lincoln", "Spokane", "Douglas", "Benton", "Year", "King",
                  "Size..acres.", "Structures.threatened", "Timber", "Yakima", "U")]

index <- sample(1:nrow(selData), 0.8*nrow(selData))
train_df <- selData[index, ]
test_df  <- selData[-index, ]

model <- lm(Estimated.Total.Cost ~ ., data=train_df)

pred.int <- predict(model, test_df, interval = "confidence")
print(pred.int)

pred.df <- cbind(test_df, pred.int)
pred.df$rid <- as.numeric(row.names(pred.df))

pd <- position_dodge(0.1)

ggplot(pred.df, aes(x=rid, y=Estimated.Total.Cost, group=1)) + 
  geom_errorbar(aes(ymin=lwr, ymax=upr), colour="black", width=.1, position=pd) +
  geom_line(position=pd, color="blue", size=1) +
  geom_point(position=pd, size=3) +
  geom_line(aes(x=rid, y=fit, group=2), color="red", size=1)

