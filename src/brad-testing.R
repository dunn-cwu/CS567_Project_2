library(plyr)
library(standardize)
library(ggplot2)

df = read.csv("dat/Wildfires_WA_dataset.csv", header = TRUE)

# calculate duration
df$Duration = as.Date(as.character(df$Date.contained), format="%d-%b-%Y")-
  as.Date(as.character(df$Date.Start), format="%d-%b-%Y")

# columns no longer needed
df = subset(df, select=-c(Fire.Name, Date.Start, Date.contained))

# removing commas from size and estimated total cost
df$Size..acres. <- as.numeric(sub(",", "", df$Size..acres.))
df$Estimated.Total.Cost <- as.numeric(sub(",", "", df$Estimated.Total.Cost))

# translate Yes and No to 1 and 0
df$Evacuations <- revalue(df$Evacuations, c("Yes"=1, "No"=0, "No "=0))

# removing column with outliers
df = subset(df, select=-c(X..of.Sheltering.in.place, Deaths, Injuries..civilians.))

# factor string fields
counties<- c(str_split(unique(df$County), ","))
causes <- c(str_split(unique(df$Cause), ","))

# factoring counties
for (county in counties)
{
  df[, county] <- as.numeric(0)
}

# factoring causes
for (cause in causes)
{
  df[, cause] <- as.numeric(0)
}

for (row in 1:nrow(df))
{
  df[row, paste(df[row, "County"])] <- as.numeric(1)
  df[row, paste(df[row, "Cause"])] <- as.numeric(1)
}

# columns no longer needed
df = subset(df, select=-c(County, Cause))

# convert evacuated nas to 0
df$X..of.Evacuated[is.na(df$X..of.Evacuated)] <- as.numeric(0)

# remove rows where no estimated cost
df <- na.omit(df, cols = Estimated.Total.Cost)

# numeric-ify all columns
df <- as.data.frame(sapply(df, as.numeric))

# sapply caused 0s and 1s on evacuations to become 1s and 2s
df$Evacuations <- df$Evacuations - 1

# normalizing select columns
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

df$Size..acres. <- normalize(df$Size..acres.)
df$Structures.threatened <- normalize(df$Structures.threatened)
df$X..of.Evacuated <- normalize(df$X..of.Evacuated)
df$Resources.assigned..personnel. <- normalize(df$Resources.assigned..personnel.)
df$Duration <- normalize(df$Duration)

# split into train and test sets
index <- sample(1:nrow(df), 0.8*nrow(df))
train_df <- df[index, ]
test_df  <- df[-index, ]

# run model with all predictors
lm_model <- lm(Estimated.Total.Cost ~ ., data=train_df)
summary <- summary(lm_model)

# run step-wise selection
step <- step(lm_model, direction = "backward")

# update model with recommended call
lm_model <- lm(Estimated.Total.Cost ~ Year + Grass + Timber + Structures.damaged + 
                 Structures.threatened + Injuries..Responders. + X..of.Evacuated + 
                 Whitman + Asotin + Grant + Stevens + H + U, data=train_df)

# predictions
cost_pred <- predict(lm_model, test_df) # highest run so far was 82.4%
actuals_preds <- data.frame(cbind(actuals=test_df$Estimated.Total.Cost, predicteds=cost_pred))
correlation_accuracy <- cor(actuals_preds)
head(actuals_preds)

