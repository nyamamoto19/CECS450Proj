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
  xlim(2010,2014)+
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

#electricity usage small
sql <- "SELECT year,quantity
        FROM energy
        WHERE commodity_transaction = 'Electricity - total net installed capacity of electric power plants, autoproducer'"
result <- sqldf(sql)  
print(result)
graph <-ggplot(result,aes(x = year,y=quantity))
graph <- graph + geom_line(stat='identity') + 
  geom_point(stat='identity') + 
  xlim(2010,2014)+
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
sql <- "SELECT Year, Percent
        FROM cpu
        WHERE Year BETWEEN 1997 AND 2000"
result <- sqldf(sql)  
cpu = read.csv("C:/Users/nicho/Desktop/CECS450Proj/archive/cpuOwn.csv")
font_add(family = "Consolas", regular = "Consola.ttf")
showtext_auto()

graph <- ggplot(cpu,aes(x=factor(Year),y=Percent,group = Year))+
  geom_bar(stat="identity")+
  geom_bar(data=result,stat="identity",fill = "#60DB46")+
  geom_text(aes(label = Percent), vjust = -1, color = "#60DB46")+
  labs(title = "Percentage of Households that Owned a Computer",
       y = "Percentage",
       x = "Year")+
  theme_minimal()+
  theme(plot.background = element_rect(fill = "black"),
        plot.title = element_text(color = "#60DB46"),
        panel.grid.major = element_line(color = "black"),  
        axis.line = element_line(color = "#60DB46"),
        axis.title = element_text(color = "#60DB46"),
        text = element_text(family = "Consolas"))
print(graph)

#cpu growth
sql <- "SELECT Year, Percent
        FROM cpu
        WHERE Year BETWEEN 1997 AND 2001"
result <- sqldf(sql)  
graph <- ggplot(cpu,aes(x=Year,y=Percent))+
  geom_line(stat = "identity",color = "#60DB46",size=1.3)+
  geom_point(data = result,stat = "identity",color = "#60DB46")+
  geom_text(data = result,aes(label = Percent), hjust = -.5, color = "#60DB46")+
  labs(title = "Percentage of Households that Owned a Computer",
       y = "Percentage",
       x = "Year")+
  theme_minimal()+
  theme(plot.background = element_rect(fill = "black"),
        plot.title = element_text(color = "#60DB46"),
        panel.grid.major = element_line(color = "black"),
        panel.grid.minor = element_line(color = "black"),
        axis.line = element_line(color = "#60DB46"),
        axis.title = element_text(color = "#60DB46"),
        text = element_text(family = "Consolas"))
print(graph)

#internet usage
internet = read.csv("C:/Users/nicho/Desktop/CECS450Proj/archive/number-of-internet-users.csv")

sql <- "SELECT Year, NumberofInternetusers as Users
        FROM internet
        WHERE Code = 'USA'"
result <- sqldf(sql)  
print(result)
sql2 <- "SELECT Year, NumberofInternetusers as Users
        FROM internet
        WHERE( (Code = 'USA') AND (Year BETWEEN 1997 AND 2001) )"
result2 <- sqldf(sql2) 
print(result2)
graph <- ggplot(result,aes(x = Year, y = Users))+
  geom_line(stat = "identity",color = "#60DB46",size = 1.3)+
  geom_point(data=result2,color = "#60DB46")+
  geom_text(data =result2,aes(label = Users), hjust = -.2, color = "#60DB46")+
  labs(y = "Users",
       x = "Year",
       title = "Number of People That Use the Internet")+
  theme_minimal()+
  theme(plot.background = element_rect(fill = "black"),
        plot.title = element_text(color = "#60DB46"),
        panel.grid.major = element_line(color = "black"),
        panel.grid.minor = element_line(color = "black"),
        axis.line = element_line(color = "#60DB46"),
        axis.title = element_text(color = "#60DB46"),
        text = element_text(family = "Consolas"))
  
print(graph)


#elec price
elecpri = read.csv("C:/Users/nicho/Desktop/CECS450Proj/archive/elecprices.csv")
graph <- ggplot(elecpri,aes(x=factor(Year),y=Price,group = Year))+
  geom_bar(stat="identity",color="black",fill="#669966")+
  geom_text(aes(label= Price), vjust = -1, color = "black")+
  labs(title = "Price of Electricity",
       y = "Price (cents per kilowatt-hour)",
       x = "Year")+
  theme_minimal()+
  theme(plot.background = element_rect(fill = "#FFFEC6"),
        plot.title = element_text(color = "black"),
        panel.grid.major = element_line(color = "#FFFEC6"),  
        axis.line = element_line(color = "black"),
        axis.title = element_text(color = "black"),
        text = element_text(family = "Consolas"))
print(graph)


#Electricity Price Comparision
comp = read.csv("C:/Users/nicho/Desktop/CECS450Proj/archive/ElecPriceComp.csv")
sql <- "SELECT TimePeriod,Era,Price
        FROM comp
        WHERE Era='1990-2000'"
result <- sqldf(sql)  

sql2 <- "SELECT TimePeriod,Era,Price
        FROM comp
        WHERE Era='2000-2020'"
result2 <- sqldf(sql2)  

graph <- ggplot(comp, aes(x=TimePeriod,y=Price,fill=Era))+
  geom_bar(stat="identity",position = "dodge")+
  geom_text(data=result,aes(label= Price),vjust=-1,hjust = +2.5, color = "black")+
  geom_text(data=result2,aes(label= Price),vjust=-1,hjust = -1, color = "black")+
  scale_fill_manual(values = c("#1047AB","#AB1017"))+
  labs(title = "Price Comparison",
       y = "Price (US Dollars)",
       x = "Time Frame")+
  theme_minimal()+
  theme(plot.background = element_rect(fill = "#FFFEC6"),
        plot.title = element_text(color = "black"),
        panel.grid.major = element_line(color = "#FFFEC6"),  
        axis.line = element_line(color = "black"),
        axis.title = element_text(color = "black"),
        text = element_text(family = "Consolas"))

print(graph)
