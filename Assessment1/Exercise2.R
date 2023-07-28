#20BDS0162 - SHOBHIT AGRAWAL
#Exercise 2
#Q1

#Loading the libraries

library(dplyr)
library(tidyr)
library(gapminder)
library(ggplot2)

View(gapminder)
str(gapminder)
glimpse(gapminder) #dplyr for glimpse

#1.Extract continent 'Asia'
filter(gapminder,continent == "Asia")
gapminder %>% filter(continent == "Asia")

#2.Extract year 1957
gapminder %>% filter(year == "1957")

#3.Extract year as 2002 and country china
gapminder %>% filter(year == "2002" & country == "China")
gapminder %>% filter(year == "2002", country == "China")

#4.Sort lifeExp in desc order
sort(gapminder$lifeExp,decreasing = TRUE)
arrange(gapminder,desc(lifeExp))

#5.1957 and pop in desc order
arrange(filter(gapminder,year == "1957"),desc(pop))
gapminder %>% filter(year == "1957") %>% arrange(desc(pop))

#6.a.lifeExp in months
data = gapminder
data

#updating the same column
data$lifeExp = data$lifeExp*12

#6.b.adding a new column
data = mutate(data,lifeExpMonths = lifeExp*12)
data

#gapminder_1952
gapminder_1952 = gapminder %>% filter(year == "1952")
gapminder_1952

View(gapminder_1952)

#7.Visualize scatter plot for pop and gdpPercap for gapminder_1952
ggplot(data = gapminder_1952, 
       aes(x = pop,y = gdpPercap,color = as.factor(continent))) + 
  geom_point() + scale_x_log10() + scale_y_log10() + xlab("Pop") + ylab("GDP Per Capita") + 
  ggtitle("Pop vs GDP Per Capita 20BDS0162")

#8.Scatter plot pop and lifeExp group by continent based on population size
ggplot(data = gapminder_1952, 
       aes(x = pop,y = lifeExp,color = as.factor(continent),size = gdpPercap))+
  geom_point() + scale_x_log10() + xlab("Pop") + ylab("Life Exp") + 
  ggtitle("Pop vs Life Exp 20BDS0162")

#9.Sub-graph
ggplot(data = gapminder_1952, 
       aes(x = pop,y = lifeExp, color = continent))+
  geom_point() + scale_x_log10() + facet_wrap(~continent)+ xlab("Pop") + ylab("Life Exp") + 
  ggtitle("Pop vs Life Exp 20BDS0162")

#10.Sub-graph for year-DS:gapminder
ggplot(data = gapminder, 
       aes(x = pop,y = lifeExp, color = continent))+
  geom_point() + scale_x_log10() + facet_wrap(~year) + xlab("Pop") + ylab("Life Exp") + 
  ggtitle("Pop vs Life Exp 20BDS0162")

#11.Summarize - median lifeExp, DS:gapminder
gapminder%>%summarize(MedianLifeExp = median(lifeExp))

#12.1957, median - lifeExp, max - gdpPercap
gapminder %>% filter(year == 1957) %>% 
  summarize(MedianLifeExp = median(lifeExp),MaxgdpPercap = max(gdpPercap))

#13.group by year, median lifeExp
#store in object by_year
data = gapminder
by_year <- data %>% group_by(year)%>% summarize(MedianLifeExp = median(lifeExp))
by_year

#14.Visualize year vs MedianLifeExp:by_year
ggplot(data = by_year, 
       aes(x = year,y = MedianLifeExp))+
  geom_point() + expand_limits(y = 0) + xlab("Year") + ylab("Median Life Exp")+
  ggtitle("Year vs Median Life Exp 20BDS0162")

#15.summarize the median gdpPercap by year and continent and save it in 
#by_year_continent
data = gapminder

by_year_continent <- data %>% 
  group_by(year,continent)%>%
  summarize(MedianGdpPercap = median(gdpPercap))

by_year_continent

#16.Visualise year vs MedianGdpPercap
#16.1.Line plot
ggplot(data = by_year_continent, 
       aes(x = year,y = MedianGdpPercap,color = continent))+
  geom_line() + geom_point() + xlab("Year") + ylab("Median GDP Per Capita")+
  ggtitle("Year vs Median GDP Per Capita 20BDS0162")

#16.2.Boxplot
ggplot(data = by_year_continent, 
       aes(x = year,y = MedianGdpPercap,color = continent))+
  geom_boxplot() + xlab("Year") + ylab("Median GDP Per Capita")+
  ggtitle("Year vs Median GDP Per Capita 20BDS0162")

#16.3.Column chart
ggplot(data = by_year_continent, 
       aes(x = year,y = MedianGdpPercap,color = continent))+
  geom_col() + xlab("Year") + ylab("Median GDP Per Capita")+
  ggtitle("Year vs Median GDP Per Capita 20BDS0162")

#Q2
#Color Visualization using RColorBrewer

library(RColorBrewer)
library(viridis)
library(ggplot2)
library(gridExtra)

str(mpg)
factor(mpg$cyl)

display.brewer.all()
display.brewer.all(colorblindFriendly=T)


#mpg dataset
View(mpg)

#1.Density Plot
ggplot(data = mpg,aes(x = cty))+geom_density(aes(fill = factor(cyl))) + ggtitle("Density Plot 20BDS0162")

#2.Transparency
ggplot(data = mpg,aes(x = cty))+geom_density(aes(fill = factor(cyl),alpha=0.7)) + ggtitle("Density Plot (Transparency) 20BDS0162")

#3.Labels
ggplot(data = mpg,aes(x = cty))+geom_density(aes(fill = factor(cyl),alpha=0.7))+labs(title = "Density Plot 20BDS0162",x = "City Mileage",fill = "#Cylinder")

#4.Brewer
p1<-ggplot(data = mpg,aes(x = cty))+geom_density(aes(fill = factor(cyl),alpha=0.7))+labs(title = "D1 20BDS0162",x = "City Mileage",fill = "#Cylinder")+scale_fill_brewer(palette = "YlOrRd")
p2<-ggplot(data = mpg,aes(x = cty))+geom_density(aes(fill = factor(cyl),alpha=0.7))+labs(title = "D2 20BDS0162",x = "City Mileage",fill = "#Cylinder")+scale_fill_brewer(palette = "Set2")
p3<-ggplot(data = mpg,aes(x = cty))+geom_density(aes(fill = factor(cyl),alpha=0.7))+labs(title = "D3 20BDS0162",x = "City Mileage",fill = "#Cylinder")+scale_fill_brewer(palette = "BrBG")
p4<-ggplot(data = mpg,aes(x = cty))+geom_density(aes(fill = factor(cyl),alpha=0.7))+labs(title = "D4 20BDS0162",x = "City Mileage",fill = "#Cylinder")+scale_fill_brewer(palette = "Spectral")

grid.arrange(p1,p2,p3,p4,nrow = 2,top = "Grid Graphs 20BDS0162")

#Q4
#Colorful map using Viridis
library(maps)
library(ggmap)
library(viridis)
library(dplyr)

View(USArrests)

arrests = USArrests

arrests$region <- tolower(rownames(USArrests))
View(arrests)

#Retrieve the states map
states_map <- map_data("state")
View(states_map)
arrests_map<-left_join(x=states_map,
                       y=arrests,
                       by ='region')
View(arrests_map)

#Create the map
#1.Assault
p1<-ggplot(arrests_map,aes(x=long,y=lat,group=group))+
  geom_polygon(aes(fill=Assault),color='white')+
  labs(title="Assaults Arrests Per State 20BDS0162")+
  scale_fill_viridis_c(option='F',direction=1)

#2.Murder
p2<-ggplot(arrests_map,aes(x=long,y=lat,group=group))+
  geom_polygon(aes(fill=Murder),color='white')+
  labs(title="Murder Arrests Per State 20BDS0162")+
  scale_fill_viridis_c(option='F',direction=1)

grid.arrange(p1,p2,nrow = 1,top = "Comparison of Assault and Murder 20BDS0162")
