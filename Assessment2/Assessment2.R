#Exercise 1
install.packages("dplyr")
install.packages("arules")

library(arules)
library(dplyr)

df1 = read.csv("D:\\Sem6\\DVP\\ELA\\Assessment2\\Market_Basket_Optimisation.csv", header = FALSE)

summary(df1)
dim(df1)
str(df1)

#sparse matrix
df1 = read.transactions(file = "D:\\Sem6\\DVP\\ELA\\Assessment2\\Market_Basket_Optimisation.csv",
                        sep = ",",
                        rm.duplicates = T)

summary(df1)

itemFrequencyPlot(x = df1, topN = 10,main = "20BDS0162",col = "red", border = "blue")

#apriori algo
rules = apriori(data = df1,
                parameter = list(support = 0.004,
                                 confidence = 0.2))
#visualizing
inspect(sort(rules, by = 'lift')[1:10])

#Exercise 2
library(arules)
library(dplyr)

#k means clustering
df2 = read.csv("D:\\Sem6\\DVP\\ELA\\Assessment2\\Social_Network_Ads.csv")
df2 = df2[4:5]
df2

library(cluster)

set.seed(5000)
wcss = vector()
wcss

for(i in 1:50)
  wcss[i] = sum(kmeans(df2, i)$withinss)

plot(1:50, wcss, type = 'b',main = "20BDS0162") #5 cluster reqd 

kmeans = kmeans(x = df2, centers = 5)

y_kmeans = kmeans$cluster

z=clusplot(df2, y_kmeans,main = "20BDS0162")
z
