install.packages("sqldf")
install.packages("readxl")
library("readxl")
library("sqldf")
library("ggplot2")
prices = read.csv("/Users/yamahon/Desktop/CECS450Proj/archive/car_prices.csv")
energy = read.csv("/Users/yamahon/Desktop/CECS450Proj/archive/global-energy-substitution.csv")
sql <- "SELECT year,AVG(price) as price
        FROM prices
        GROUP BY year"
result <- sqldf(sql)  
print(result)

graph <-ggplot(result,aes(x = year,y=price))
graph + geom_bar(stat='identity')
print(graph)


energy = read.csv("/Users/yamahon/Desktop/CECS450Proj/archive/energy.csv")
