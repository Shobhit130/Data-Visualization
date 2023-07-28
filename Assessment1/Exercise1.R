#20BDS0162 - SHOBHIT AGRAWAL
#Exercise 1
#Q1

#Loading the in-built dataset airquality

data = airquality
data

#Analyzing the data using various functions

str(data)
head(data)
tail(data)
summary(data)

#1.Using basic plot function to plot Solar.R vs Temp

plot(data$Solar.R,data$Temp,main = "20BDS0162")

#2.Using the barplot function

#Horizontal barplot
barplot(data$Temp, horiz = TRUE, main = "Horizontal Barplot 20BDS0162", col
        ="red", xlab = "Temperature", ylab = "Observations" )

#Vertical barplot
barplot(data$Temp, horiz = FALSE, main = "Vertical Barplot 20BDS0162", col ="green",
        xlab = "Observations", ylab = "Temperature")

#3.Boxplot 

#One box plot
boxplot(data$Ozone, main="Single Boxplot 20BDS0162", col = "blue", notch = TRUE,
        outline = TRUE, border = "red")

#Multiple boxplot
boxplot(data$Ozone,data$Solar.R,data$Wind,data$Temp, notch = TRUE, col =
          c("red","green","blue","orange"), main = "Multiple Boxplots 20BDS0162")

#4.Plot with both points and lines

plot(data$Wind,type = "b",main = "Points and Lines 20BDS0162", col = "red", bg = "blue",pch = 24)

#5.Histogram

hist(data$Wind, col = "yellow", border = "green", main = "Histogram 20BDS0162")

#6.Pie Chart

pie(data$Month,labels = data$Month, main = "Pie Chart 20BDS0162")

#7.Visualizing all the above plots in a 3x3 grid

par(mfrow=c(3,3))

#8.Scatter Plot Matrix
plot(data, main = "Scatter Plot Matrix 20BDS0162") 

#Q2
library(lattice)
library(ggplot2)

data = mtcars
data

#1.Scatter Plot
ggplot(data = data, aes(x = wt,y = mpg,color = as.factor(cyl)))+
  geom_point()+xlab("Weight")+ylab("Mileage")+ggtitle("WT vs MPG 20BDS0162")

#2.Line Plot
ggplot(data = data, aes(x = wt,y = mpg,color = as.factor(cyl)))+
  geom_line()+xlab("Weight")+ylab("Mileage")+ggtitle("WT vs MPG 20BDS0162")

#3.Plot with both line and point
ggplot(data = data, aes(x = wt,y = mpg,color = as.factor(cyl)))+geom_point()+
  geom_line()+xlab("Weight")+ylab("Mileage")+ggtitle("WT vs MPG 20BDS0162")

#4.Plot with size attribute
ggplot(data = data, aes(x = wt,y = mpg,color = as.factor(cyl),size = qsec))+
  geom_point()+xlab("Weight")+ylab("Mileage")+ggtitle("WT vs MPG 20BDS0162")

#5.Plot with shape attribute
ggplot(data = data, aes(x = wt,y = mpg,shape = as.factor(cyl),color = as.factor(cyl),size = qsec))+
  geom_point()+xlab("Weight")+ylab("Mileage")+ggtitle("WT vs MPG 20BDS0162")

#6.Histogram
ggplot(data = data, aes(x = wt))+geom_histogram(color="blue",fill="yellow",bins=7)+ggtitle("WT vs MPG 20BDS0162")+xlab("Weight")+ylab("Count")

#7.Boxplot
data$gear = as.factor(data$gear)
ggplot(data = data, aes(x = gear,y = disp))+geom_boxplot(color = "red", fill = "yellow")+ggtitle("WT vs MPG 20BDS0162")+xlab("Weight")

data

#8.Column Chart
ggplot(data = data, 
       aes(x = gear,y = mpg,color = wt))+
  geom_col() + xlab("Gear") + ylab("Mpg")+
  ggtitle("Gear vs Mpg 20BDS0162")

#9.Line Plot with weight
ggplot(data = data, aes(x = wt,y = mpg,color = as.factor(cyl), size = qsec))+
  geom_line()+xlab("Weight")+ylab("Mileage")+ggtitle("WT vs MPG 20BDS0162")

ggplot(data,aes(x = gear,y = mpg,fill = as.factor(cyl))) + geom_bar(stat = "identity")
