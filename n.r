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
graph <- graph + geom_line(stat='identity') +ylim(20000,35000) + geom_point(stat='identity') +ylim(20000,35000) 
print(graph)


energy = read.csv("/Users/yamahon/Desktop/CECS450Proj/archive/energy.csv")
sql <- "SELECT year,quantity
        FROM energy
        WHERE commodity_transaction = 'Gas Oil/ Diesel Oil - Consumption by transport'"
result <- sqldf(sql)  
print(result)
graph <-ggplot(result,aes(x = year,y=quantity))
graph <- graph + geom_line(stat='identity') + geom_point(stat='identity') +xlim(2010,2014)
print(graph)
