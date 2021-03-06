# setwd("~/projects/CS567_Project_2")
# setwd("C:/Users/Coder/OneDrive/Documents/CS 567 Computational Stats/CS567_Project_2")

library(plyr)
library(reshape2)
library(standardize)
library(stringr)
library(ggplot2)
library(scales)
library(dvmisc)
library(tidyverse)
library(caret)
library(leaps)

# Set seed used for random sampling
SAMPLE_SEED <- 8

# data cleaning
data = read.csv("./dat/Wildfires_WA_dataset.csv", header = TRUE)

# translate Yes and No to 1 and 0
data$Evacuations <- revalue(data$Evacuations, c("Yes" = as.numeric(1)))
data$Evacuations <- revalue(data$Evacuations, c("No"  = as.numeric(0)))

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
  # some of the evacuated NAs can be set to 0
  if (data[row, "Evacuations"] == 0)
  {
    data[row, "X..of.Evacuated"] = as.numeric(0)
  }
  
  data[row, paste(data[row, "County"])] <- as.numeric(1)
  data[row, paste(data[row, "Cause"])]  <- as.numeric(1)
}

# remove unnecessary columns
# note: X..of.Sheltering.in.place is removed because it only has two rows of data
data = subset(data, select = -c(County, Cause, Fire.Name, Date.Start, Date.contained, X..of.Sheltering.in.place))

# remove ',' from values in acres and total costs columns then convert to numeric
data$Size..acres. <- as.numeric(gsub(",","",data$Size..acres.))
data$Estimated.Total.Cost <- as.numeric(gsub(",","",data$Estimated.Total.Cost))

data<-na.omit(data) 

# make sure all columns are numeric
data <- as.data.frame(sapply(data, as.numeric))

# remove outcome variable, standardize columns, then append outcome variable
sdata = subset(data, select = -c(Estimated.Total.Cost))
sdata = as.data.frame(scale(sdata))
sdata = cbind(sdata,  subset(data, select = c(Estimated.Total.Cost)))

# intial model with all columns
fit_all <- lm(Estimated.Total.Cost ~ ., data = sdata)

elimTypes = data.frame(Types = c("Backwards", "Forwards", "Both"), MSE = c(0, 0, 0))

## backward elimination ##
# step(fit_all, direction = "backward")

sel_data = sdata[c("Estimated.Total.Cost","Resources.assigned..personnel.", "Structures.lost", "Lewis", "Deaths", "Injuries..Responders.",
                   "Ferry", "Chelan", "Whatcom", "X..of.Evacuated", "Okanogan", "Lincoln", "Spokane", "Douglas", "Benton", "Year", "King",
                   "Size..acres.", "Structures.threatened", "Timber", "Yakima", "U")]

# Set seed used in random number generator
set.seed(SAMPLE_SEED)

index <- sample(1:nrow(sel_data), 0.8 * nrow(sel_data))
train_df <- sel_data[index, ]
test_df  <- sel_data[-index, ]

model <- lm(Estimated.Total.Cost ~ ., data = train_df)
elimTypes$MSE[1] <- get_mse(model)

pred_int <- predict(model, test_df, interval = "confidence")
pred_df <- cbind(test_df, pred_int)
pred_df$rid <- as.numeric(row.names(pred_df))

pd <- position_dodge(0.1)

plot_backward <- ggplot(pred_df, aes(x = rid, y = Estimated.Total.Cost)) + 
  scale_y_continuous(labels = comma) +
  geom_errorbar(aes(ymin = lwr, ymax = upr), colour = "black", width = .1, position = pd) +
  
  geom_line( position = pd, size = 1.25, aes(color = "Actual")) +
  geom_point(position = pd, size = 2,    aes(color = "Actual")) +
  
  geom_line( aes(x = rid, y = fit, color = "Predicted"), size = 1.25) +
  geom_point(aes(x = rid, y = fit, color = "Predicted"), size = 2) + 
  
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
  theme(legend.title=element_blank(), legend.justification = c(1,1), legend.position = c(1,1), text = element_text(size = 20)) +
  labs(title = "Model Accuracy with Backward Elimination", y = "Estimated Cost ($)", x = "Data Row")

ggsave("out/back_elim.pdf", units = "in", width = 10, height = 10, dpi = 300)

## forward elimination ##
# step(fit_all, direction = "forward")

sel_data = sdata[c("Estimated.Total.Cost","Year", "Size..acres.", "Grass", "Brush", "Timber",
                   "Structures.lost", "Structures.damaged", "Structures.threatened", "Deaths", "Injuries..Responders.", 
                   "Injuries..civilians.", "Evacuations", "X..of.Evacuated", "Resources.assigned..personnel.", 
                   "duration", "Spokane", "Whitman", "Okanogan", "Whatcom", "Chelan", "Walla Walla", "Yakima", "Douglas", 
                   "Klickitat", "Lincoln", "Ferry", "Asotin", "Snohomish", "Grant", "Stevens", "Lewis", "Benton", 
                   "Franklin", "Pend Oreille", "King", "Kittitas", "H", "U", "L")]

# Set seed used in random number generator
set.seed(SAMPLE_SEED)

index <- sample(1:nrow(sel_data), 0.8 * nrow(sel_data))
train_df <- sel_data[index, ]
test_df  <- sel_data[-index, ]

model <- lm(Estimated.Total.Cost ~ ., data = train_df)
elimTypes$MSE[2] <- get_mse(model)

pred_int <- predict(model, test_df, interval = "confidence")
pred_df <- cbind(test_df, pred_int)
pred_df$rid <- as.numeric(row.names(pred_df))

pd <- position_dodge(0.1)

plot_forward <- ggplot(pred_df, aes(x = rid, y = Estimated.Total.Cost)) +
  scale_y_continuous(labels = comma) +
  geom_errorbar(aes(ymin = lwr, ymax = upr), colour = "black", width = .1, position = pd) +
  
  geom_line( position = pd, size = 1.25, aes(color = "Actual")) +
  geom_point(position = pd, size = 2,    aes(color = "Actual")) +
  
  geom_line( aes(x = rid, y = fit, color = "Predicted"), size = 1.25) +
  geom_point(aes(x = rid, y = fit, color = "Predicted"), size = 2) + 
  
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
  theme(legend.title=element_blank(), legend.justification = c(1,1), legend.position = c(1,1), text = element_text(size = 20)) +
  labs(title = "Model Accuracy with Forward Elimination", y = "Estimated Cost ($)", x = "Data Row")

ggsave("out/for_elim.pdf", units = "in", width = 10, height = 10, dpi = 300)

## Both ##
# step(fit_all, direction = "both")

sel_data = sdata[c("Estimated.Total.Cost","Year", "Timber", "Structures.lost", "Deaths", "Injuries..Responders.",
                   "X..of.Evacuated", "Resources.assigned..personnel.", "Spokane", "Okanogan", 
                   "Whatcom", "Chelan", "Douglas", "Lincoln", "Ferry", "Lewis", "Benton",
                   "Kittitas", "U")]

# Set seed used in random number generator
set.seed(SAMPLE_SEED)

index <- sample(1:nrow(sel_data), 0.8 * nrow(sel_data))
train_df <- sel_data[index, ]
test_df  <- sel_data[-index, ]

model <- lm(Estimated.Total.Cost ~ ., data = train_df)
elimTypes$MSE[3] <- get_mse(model)

pred_int <- predict(model, test_df, interval = "confidence")

pred_df <- cbind(test_df, pred_int)
pred_df$rid <- as.numeric(row.names(pred_df))

pd <- position_dodge(0.1)

plot_both <- ggplot(pred_df, aes(x = rid, y = Estimated.Total.Cost)) + 
  scale_y_continuous(labels = comma) +
  geom_errorbar(aes(ymin = lwr, ymax = upr), colour = "black", width = .1, position = pd) +
  
  geom_line( position = pd, size = 1.25, aes(color = "Actual")) +
  geom_point(position = pd, size = 2,    aes(color = "Actual")) +
  
  geom_line( aes(x = rid, y = fit, color = "Predicted"), size = 1.25) +
  geom_point(aes(x = rid, y = fit, color = "Predicted"), size = 2) + 
  
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
  theme(legend.title=element_blank(), legend.justification = c(1,1), legend.position = c(1,1), text = element_text(size = 20)) +
  labs(title = "Model Accuracy with Bidirectional Elimination", y = "Estimated Cost ($)", x = "Data Row")

ggsave("out/dual_elim.pdf", units = "in", width = 10, height = 10, dpi = 300)

# A bargraph of Mean Squared Error
mse_compare <- ggplot(data = elimTypes, aes(x = Types, y = MSE / 100000000000, fill = Types)) +
  geom_bar(stat = "identity") +
  labs(title = "Model Accuracies", y = "MSE (per 100T)", x = "Elimination Types") +
  theme(text = element_text(size = 14), legend.position = "none")

ggsave("out/squared_error.pdf", units = "in", width = 10, height = 10, dpi = 300)


# k-fold cross validation for training set
set.seed(37) 
index = sample(1:nrow(sel_data), 0.8 * nrow(sel_data))
train_df = sel_data[index, ]
test_df  = sel_data[-index, ]

# Backward elimination
# Define training control
train.control = trainControl(method = "cv", number = 10)
step.model = train(Estimated.Total.Cost ~ ., 
                   data = train_df, 
                   method = "leapBackward",
                   trControl = train.control)
# Summarize the results
summary(step.model)
step.model$results
step.model$bestTune
summary(step.model$finalModel)
model <- lm(Estimated.Total.Cost ~ Structures.lost+Resources.assigned..personnel., data = train_df)
# Predict
elimTypes$MSE[1] <- get_mse(model)
pred_int <- predict(model, test_df, interval = "confidence")
pred_df <- cbind(test_df, pred_int)
pred_df$rid <- as.numeric(row.names(pred_df))
pd <- position_dodge(0.1)
plot_backward <- ggplot(pred_df, aes(x = rid, y = Estimated.Total.Cost)) + 
  scale_y_continuous(labels = comma) +
  geom_errorbar(aes(ymin = lwr, ymax = upr), colour = "black", width = .1, position = pd) +
  geom_line( position = pd, size = 1.25, aes(color = "Actual")) +
  geom_point(position = pd, size = 2,    aes(color = "Actual")) +
  geom_line( aes(x = rid, y = fit, color = "Predicted"), size = 1.25) +
  geom_point(aes(x = rid, y = fit, color = "Predicted"), size = 2) + 
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
  theme(legend.title=element_blank(), legend.justification = c(1,1), legend.position = c(1,1), text = element_text(size = 20)) +
  labs(title = "Model Accuracy with Backward Elimination (k-fold)", y = "Estimated Cost ($)", x = "Data Row")
plot_backward
ggsave("out/cross_validation/back_elim.pdf", units = "in", width = 10, height = 5, dpi = 300)


# Forward elimination
# Define training control
step.model = train(Estimated.Total.Cost ~ ., 
                   data = train_df, 
                   method = "leapForward",
                   trControl = train.control)
# Summarize the results
summary(step.model)
step.model$results
step.model$bestTune
summary(step.model$finalModel)
model <- lm(Estimated.Total.Cost ~ Structures.lost+Deaths+Resources.assigned..personnel., data = train_df)
# Predict
elimTypes$MSE[1] <- get_mse(model)
pred_int <- predict(model, test_df, interval = "confidence")
pred_df <- cbind(test_df, pred_int)
pred_df$rid <- as.numeric(row.names(pred_df))
pd <- position_dodge(0.1)
plot_forward <- ggplot(pred_df, aes(x = rid, y = Estimated.Total.Cost)) + 
  scale_y_continuous(labels = comma) +
  geom_errorbar(aes(ymin = lwr, ymax = upr), colour = "black", width = .1, position = pd) +
  geom_line( position = pd, size = 1.25, aes(color = "Actual")) +
  geom_point(position = pd, size = 2,    aes(color = "Actual")) +
  geom_line( aes(x = rid, y = fit, color = "Predicted"), size = 1.25) +
  geom_point(aes(x = rid, y = fit, color = "Predicted"), size = 2) + 
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
  theme(legend.title=element_blank(), legend.justification = c(1,1), legend.position = c(1,1), text = element_text(size = 20)) +
  labs(title = "Model Accuracy with Forward Elimination (k-fold)", y = "Estimated Cost ($)", x = "Data Row")
plot_forward
ggsave("out/cross_validation/for_elim.pdf", units = "in", width = 10, height = 5, dpi = 300)



# Train the model using stepwise selection
step.model = train(Estimated.Total.Cost ~ ., 
                   data = train_df, 
                   method = "leapSeq",
                   trControl = train.control)
# Summarize the results
summary(step.model)
step.model$results
step.model$bestTune
summary(step.model$finalModel)
model <- lm(Estimated.Total.Cost ~ Structures.lost+Snohomish+Resources.assigned..personnel., data = train_df)
# Predict
elimTypes$MSE[1] <- get_mse(model)
pred_int <- predict(model, test_df, interval = "confidence")
pred_df <- cbind(test_df, pred_int)
pred_df$rid <- as.numeric(row.names(pred_df))
pd <- position_dodge(0.1)
plot_dual <- ggplot(pred_df, aes(x = rid, y = Estimated.Total.Cost)) + 
  scale_y_continuous(labels = comma) +
  geom_errorbar(aes(ymin = lwr, ymax = upr), colour = "black", width = .1, position = pd) +
  geom_line( position = pd, size = 1.25, aes(color = "Actual")) +
  geom_point(position = pd, size = 2,    aes(color = "Actual")) +
  geom_line( aes(x = rid, y = fit, color = "Predicted"), size = 1.25) +
  geom_point(aes(x = rid, y = fit, color = "Predicted"), size = 2) + 
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
  theme(legend.title=element_blank(), legend.justification = c(1,1), legend.position = c(1,1), text = element_text(size = 20)) +
  labs(title = "Model Accuracy with Bidirectional Elimination (k-fold)", y = "Estimated Cost ($)", x = "Data Row")
plot_dual
ggsave("out/cross_validation/dual_elim.pdf", units = "in", width = 10, height = 5, dpi = 300)

