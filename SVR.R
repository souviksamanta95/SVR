
data <- read.csv("Boston Housing Prices.csv")
head(data)
str(data)
# CHecking missing values
sum(is.na(data))

# Plotting the data with regression line -
library(ggplot2)

ggplot(data,aes(x=rooms,y=cmedv))+theme_classic()+
  geom_point(color="blue")+
  geom_smooth(method='lm', formula= y~x, color="red", se=FALSE)+
  xlab("No. of rooms")+
  ylab("House cmedv")


#Predict Y using Linear Model
model <- lm(cmedv~rooms, data = data)
pred <- predict(model,data)

## RMSE Calculation for linear model

#Install Package
#install.packages("hydroGOF")

#Load Library
library(hydroGOF)

#Calculate RMSE 
RMSE=rmse(pred,data$cmedv)
RMSE


# Working on SVR with the same data -

## Fit SVR model and visualize using scatter plot

#Install Package
#install.packages("e1071")

#Load Library
library(e1071)

#Regression with SVM
modelsvm = svm(cmedv~rooms,data,kernel = "linear")

#Predict using SVM regression
predsvm = predict(modelsvm, data)

#Scatter Plot
plot(data$rooms,data$cmedv)

#Overlay SVM Predictions on Scatter Plot
points(data$rooms, predsvm, col = "red", pch=16)

#Calculate RMSE 
RMSE2=rmse(predsvm,data$cmedv)
RMSE2

#Tune the SVM model
optmodel <- tune(svm, cmedv~rooms, data=data,kernel = "linear",ranges=list(epsilon=seq(0,1,0.1), cost=1:100))

plot(optmodel)

# Revising model with optimal parameters

optmodel$best.parameters # This gives epsilon = 0.1 and cost = 1

modelsvm = svm(cmedv~rooms,data,kernel = "linear",epsilon=0.1, cost=1)

#Predict using optimal model
predsvm = predict(modelsvm, data)

#Calculate RMSE 
RMSE2=rmse(predsvm,data$cmedv)
RMSE2


#Tune the SVM model with default kernel
optmodel2 <- tune(svm, cmedv~rooms, data=data,ranges=list(epsilon=seq(0,1,0.1), cost=1:100))

plot(optmodel2)

# Revising model with optimal parameters

optmodel2$best.parameters # This gives epsilon = 0.2 and cost = 15

modelsvm = svm(cmedv~rooms,data,epsilon=0.2, cost=15)

#Predict using optimal model
predsvm = predict(modelsvm, data)

#Calculate RMSE 
RMSE2=rmse(predsvm,data$cmedv)
RMSE2

#Scatter Plot
plot(data$rooms,data$cmedv)

#Overlay SVM Predictions on Scatter Plot
points(data$rooms, predsvm, col = "red", pch=16)

#------------------------ BONUS ---------------------------

#visualizing the observations and predictions in a 3D space
#install.packages('plotly')
# Applying svr for predicting cmedv with respect to older and rooms
dataset <- data[,c(6,7,13)]
regressor <- svm(cmedv~.,data=dataset,type = "eps-regression")

library(plotly)
older = dataset$older
rooms = dataset$rooms
cmedv = dataset$cmedv
plot_ly(x=as.vector(older),y=as.vector(rooms),z=cmedv, type="scatter3d", mode="markers", name = "Obs", marker = list(size = 3)) %>%
  add_trace(x=as.vector(older),y=as.vector(rooms),z=predict(regressor, newdata=dataset), type = "mesh3d", name = "Preds")
