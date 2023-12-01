install.packages("sqldf")
install.packages("readxl")
install.packages("ggthemes")
install.packages("extrafont")
install.packages("showtext")
library("readxl")
library("sqldf")
library("ggplot2")
library("ggthemes")
library("extrafont")
library("showtext")
font_import()
warning

#gas in general
en = read.csv("/Users/yamahon/Desktop/CECS450Proj/archive/global-energy-substitution.csv")
en = read.csv("C:/Users/nicho/Desktop/CECS450Proj/archive/global-energy-substitution.csv")
sql <- "SELECT Year,Gas
        FROM en"
result <- sqldf(sql)  
graph <- ggplot(result, aes(x = Year,y=Gas )) + geom_line() + geom_point()+
  labs(y = "Quantity (Terawatt-Hours)",
       x = "Year",
       title = "Gas Usage per Year in America",
       subtitle = "General Usage")+
  theme_wsj()+
  theme(axis.title= element_text(), text = element_text(family="Trebuchet MS"))
print(graph)

#gas car avg prices
prices = read.csv("/Users/yamahon/Desktop/CECS450Proj/archive/car_prices.csv")
prices = read.csv("C:/Users/nicho/Desktop/CECS450Proj/archive/car_prices.csv")
sql <- "SELECT year,AVG(price) as price
        FROM prices
        GROUP BY year"
result <- sqldf(sql)  
print(result)

graph <-ggplot(result,aes(x = year,y=price))
graph <- graph + geom_line(stat='identity')+ylim(20000,35000) + geom_point(stat='identity') +ylim(20000,35000)+
labs(y = "Price",
     x = "Year",
     title = "Average Gas Car Cost per Year")+
  theme_economist()+
  theme(axis.title= element_text(), text = element_text(family="Trebuchet MS"))
print(graph)

#gas by transportation usage
energy = read.csv("/Users/yamahon/Desktop/CECS450Proj/archive/energy.csv")
energy = read.csv("C:/Users/nicho/Desktop/CECS450Proj/archive/energy.csv")
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

#elec car prices
elec = read.csv("/Users/yamahon/Desktop/CECS450Proj/archive/elecvehicle.csv")
elec = read.csv("C:/Users/nicho/Desktop/CECS450Proj/archive/elecvehicle.csv")

elecgraph <- ggplot(elec,aes(x=Year,y=averageprice)) +
  geom_line(stat = 'identity') + 
  geom_point(stat='identity') + 
  labs(y = "Average Car Price",
       x = "Year",
       title = "Average Electric Car Price per Year")+
  theme_economist()+
  theme(axis.title= element_text(), text = element_text(family="Trebuchet MS"))
print(elecgraph)

#electricity usage
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

#Both car prices
sql <- "SELECT year as Year,AVG(price) as averageprice
        FROM prices
        GROUP BY year"
result <- sqldf(sql)  

graph <-  ggplot(elec,aes(x=Year,y=averageprice,)) +
  geom_line(data = result) + 
  geom_line(data = elec) + 
  geom_point(data = result) + 
  geom_point(data = elec) + 
  xlim(2010,2018)
  

print(graph)

#cpu usage
cpu = read.csv("C:/Users/nicho/Desktop/CECS450Proj/archive/cpuOwn.csv")
font_add(family = "Consolas", regular = "Consola.ttf")
showtext_auto()
graph <- ggplot(cpu,aes(x=factor(Year),y=Percent,group = Year))+
  geom_bar(stat="identity",fill="#60DB46")+
  geom_text(aes(label = Percent), vjust = -1, color = "#60DB46")+
  labs(title = "Percentage of Households that Owned a Computer",
       x = "Percentage",
       y = "Year")+
  theme_minimal()+
  theme(plot.background = element_rect(fill = "black"),
        plot.title = element_text(color = "#60DB46"),
        panel.grid.major = element_line(color = "black"),  
        axis.line = element_line(color = "#60DB46"),
        axis.title = element_text(color = "#60DB46"),
        text = element_text(family = "Consolas"))
print(graph)

