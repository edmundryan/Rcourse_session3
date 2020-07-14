#Clear the workspace and all plots:
rm(list = ls())
graphics.off()

#Set the working directory (this is where your files are stored):
setwd('C:/Work/Rcourse/Session3') 

#In the first part of this session we'll use a standard built-in dataset called 'cars'.  
#Being built in means that you don’t need to do anything more to access the data.
head(cars) #display the first few rows

#Visualising the data 1 - Scatter plot
scatter.smooth(x=cars$speed, y=cars$dist, main="Distance vs Speed",
               xlab="Speed", ylab="Stopping distance")
rr=round(cor(cars$speed,cars$dist),2)
text(7,100,paste("r =",rr),cex=1.5)
#Note that we are using locally weighted smoothing to create a smoothed line through 
#the scatter plot to help see relationship between the variables and foresee trends.

#Visualising the data 2 - Boxplot
par(mfrow=c(2, 2))  # divide graph area in 2 columns
boxplot(cars$speed, main="Speed", sub=paste("Outlier rows: ", boxplot.stats(cars$speed)$out)) 
boxplot(cars$dist, main="Distance", sub=paste("Outlier rows: ", boxplot.stats(cars$dist)$out))

#Visualising the data 3 - Histogram
par(mfrow=c(2, 2))  # divide graph area in 2 columns
hist(cars$speed, main="Speed", ylab="Frequency")  
hist(cars$dist, main="Stopping Distance", ylab="Frequency")  

#Building the Linear Model:
LinearMod1 = lm(dist ~ speed, data=cars)  # build linear regression model on full data
print(LinearMod1)

#Checking the assumptions
par(mfrow=c(2, 2))
plot(LinearMod1,which=1:2)
plot(LinearMod1,which=3:4)
#(1)Residuals vs Fitted:
#A horizontal line with no distinct pattern is an indication of a linear relationship.  
#If there is also no clear pattern pattern in the residuals and they are equally 
#spread  around the y=0 line, then this indicates a constant variance in the residuals.  
#(2)Normal Q-Q: 
#Used to examine whether the residuals are normally distributed. It’s good if residuals 
#points follow the straight dashed line.
#(3) Scale-Location:
#Used to check the homogeneity of variance of the residuals (homoscedasticity). Horizontal 
#line with equally spread points is a good indication of homoscedasticity. 
#(4) Residuals vs Leverage:
#Used to identify influential cases, that is extreme values that might influence the 
#regression results when included or excluded from the analysis. Observations with large
#values of the Cook's distance (around 1) or that are very different to the other distances
#suggest that those observations should be investigated.

#Checking that the standardised residuals are within (-1.96,1.96):
plot(LinearMod1$fitted.values,rstandard(LinearMod1),xlab="Fitted Values",
     ylab="standardised residuals",main="Standardised residuals vs fitted")
lines(c(0,80),rep(1.96,2),lty=2,col="red")
lines(c(0,80),rep(-1.96,2),lty=2,col="red")
#Note: this is another check of the linearity assumption

#Look at the output of the model:
summary(LinearMod1)  


#############################################################################################
#############################################################################################

#Multiple linear regression
head(mtcars)

#Visualising the data 1 - Scatter plot
par(mfrow=c(2, 2)) 
scatter.smooth(x= mtcars$disp, y=mtcars$mpg, main="MPG vs Displacement")
rr1=round(cor(mtcars$disp,mtcars$mpg),2)
text(350,30,paste("r =",rr1))
scatter.smooth(x= mtcars$hp, y=mtcars$mpg, main="MPG vs Horsepower")
rr2=round(cor(mtcars$hp,mtcars$mpg),2)
text(250,30,paste("r =",rr2))
scatter.smooth(x= mtcars$wt, y=mtcars$mpg, main="MPG vs Weight")
rr3=round(cor(mtcars$wt,mtcars$mpg),2)
text(4,30,paste("r =",rr3))

#Visualising the data 2 - Boxplots
par(mfrow=c(2, 2))  # divide graph area into 2x2 matrix 
boxplot(mtcars$mpg, main="MPG", sub=paste("Outlier rows: ", boxplot.stats(mtcars$mpg)$out))  # box plot for 'speed'
boxplot(mtcars$disp, main="Cylinder displacement", sub=paste("Outlier rows: ", boxplot.stats(mtcars$ disp)$out))  # box plot for 'speed'
boxplot(mtcars$hp, main="Horsepower", sub=paste("Outlier rows: ", boxplot.stats(mtcars$hp)$out))  # box plot for 'speed'
boxplot(mtcars$wt, main="Weight", sub=paste("Outlier rows: ", boxplot.stats(mtcars$dist)$out))  # box plot for 'distance'

#Visualising the data 3 - Histograms
par(mfrow=c(2, 2))  # divide graph area in 2 columns
hist(mtcars$mpg, main="MPG", ylab="Frequency")  
hist(mtcars$disp, main="Cylinder displacement", ylab="Frequency") 
hist(mtcars$hp, main="Horsepower", ylab="Frequency") 
hist(mtcars$wt, main="Weight", ylab="Frequency")

#Building the Linear Model:
LinearMod2 <- lm(mpg~disp+hp+wt, data = mtcars)
print(LinearMod2)

#Checking the assumptions
par(mfrow=c(2, 2))
plot(LinearMod2,which=1:2)
plot(LinearMod2,which=3:4)
mtext(text="Checking the assumptions for MPG model",
      side=3,line=-2,outer=TRUE,cex=1.5)

#Checking that the standardised residuals are within (-1.96,1.96):
plot(LinearMod2$fitted.values,rstandard(LinearMod2),xlab="Fitted Values",
     ylab="standardised residuals",main="Standardised residuals vs fitted",ylim=c(-2.5,2.5))
lines(c(0,80),rep(1.96,2),lty=2,col="red")
lines(c(0,80),rep(-1.96,2),lty=2,col="red")

#Look at the output of the model:
summary(LinearMod2)  



#############################################################################################
#############################################################################################

#Multiple linear regression with a log-transformation

#Scatter plot
par(mfrow=c(2, 2)) 
scatter.smooth(x= mtcars$disp, y=log(mtcars$mpg), main="log(MPG) vs Displacement")
scatter.smooth(x= mtcars$hp, y=log(mtcars$mpg), main="log(MPG) vs Horsepower")
scatter.smooth(x= mtcars$wt, y=log(mtcars$mpg), main="log(MPG) vs Weight")

#Building the Linear Model:
LinearMod3 <- lm(log(mpg)~disp+hp+wt, data = mtcars)
print(LinearMod3)

#Checking the assumptions
par(mfrow=c(2, 2))
plot(LinearMod3,which=1:2)
plot(LinearMod3,which=3:4)
mtext(text="Checking the assumptions for log(MPG) model",
      side=3,line=-2,outer=TRUE,cex=1.5)

#Look at the output of the model:
summary(LinearMod3)  