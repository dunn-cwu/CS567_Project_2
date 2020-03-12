library(tidyverse)
library(caret)
library(leaps)

# define training control
set.seed(37) 
train.control = trainControl(method = "cv", number = 10)

# Train the model using backward selection
model = train(Estimated.Total.Cost ~., 
              data = data, 
              method = "leapBackward", #fit linear regression with backward selection
              trControl = train.control)
# Summarize the results
summary(model)

# Train the model using foward selection
model = train(Estimated.Total.Cost ~., 
              data = data, 
              method = "leapForward", #fit linear regression with foward selection
              trControl = train.control)
# Summarize the results
summary(model)


# Train the model using stepwise selection
model = train(Estimated.Total.Cost ~., 
              data = data, 
              method = "leapSeq", #fit linear regression with stepwise selection
              trControl = train.control)
# Summarize the results
summary(model)