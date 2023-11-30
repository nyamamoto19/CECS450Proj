install.packages("sqldf")
install.packages("readxl")
install.packages("ggthemes")
install.packages("extrafont")
library("readxl")
library("sqldf")
library("ggplot2")
library("ggthemes")
library("extrafont")

prices = read.csv("/Users/yamahon/Desktop/CECS450Proj/archive/car_prices.csv")
energy = read.csv("/Users/yamahon/Desktop/CECS450Proj/archive/global-energy-substitution.csv")
sql <- "SELECT year,AVG(price) as price
        FROM prices
        GROUP BY year"
result <- sqldf(sql)  
print(result)

graph <-ggplot(result,aes(x = year,y=price))
graph <- graph + geom_line(stat='identity')+xlim(2010,2014) +ylim(20000,35000) + geom_point(stat='identity') +ylim(20000,35000)+
labs(y = "Price",
     x = "Year",
     title = "Average Gas Car Cost per Year")+
  theme_wsj()+
  theme(axis.title= element_text(), text = element_text(family="Trebuchet MS"))
print(graph)


energy = read.csv("/Users/yamahon/Desktop/CECS450Proj/archive/energy.csv")
sql <- "SELECT year,quantity
        FROM energy
        WHERE commodity_transaction = 'Gas Oil/ Diesel Oil - Consumption by transport'"
result <- sqldf(sql)  
print(result)
graph <-ggplot(result,aes(x = year,y=quantity))
graph <- graph + geom_line(stat='identity') + geom_point(stat='identity') +
  xlim(2010,2014)+
  labs(y = "Quantity (Metric Tons-thousands)",
       x = "Year",
       title = "Gas Usage per Year in America",
       subtitle = "Specifically used for transportation")+
  theme_wsj()+
  theme(axis.title= element_text(), text = element_text(family="Trebuchet MS"))
print(graph)

elec = read.csv("/Users/yamahon/Desktop/CECS450Proj/archive/elecvehicle.csv")

elecgraph <- ggplot(elec,aes(x=Year,y=averageprice)) +
  geom_line(stat = 'identity') + 
  geom_point(stat='identity') + 
  labs(y = "Average Car Price",
       x = "Year",
       title = "Average Electric Car Price per Year")+
  theme_wsj()+
  theme(axis.title= element_text(), text = element_text(family="Trebuchet MS"))
print(elecgraph)

sql <- "SELECT year,quantity
        FROM energy
        WHERE commodity_transaction = 'Electricity - total net installed capacity of electric power plants, autoproducer'"
result <- sqldf(sql)  
print(result)
graph <-ggplot(result,aes(x = year,y=quantity))
graph <- graph + geom_line(stat='identity') + 
  geom_point(stat='identity') + 
  labs(y = "Quantity (Kilowatts-thousands)",
       x = "Year",
       title = "Electric Usage per Year in America")+
  theme_wsj()+
  theme(axis.title= element_text(), text = element_text(family="Trebuchet MS"))
print(graph)
fonts()

#Both car prices
sql <- "SELECT year as Year,AVG(price) as averageprice
        FROM prices
        GROUP BY year"
result <- sqldf(sql)  

graph <-  ggplot(elec,aes(x=Year,y=averageprice)) +
  geom_line(data = result) + 
  geom_line(data = elec) + 
  xlim(2010,2018)

print(graph)

