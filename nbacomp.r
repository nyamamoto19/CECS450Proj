seasonStats = read.csv("/Users/yamahon/Desktop/CECS450Proj/archive/all_seasons.csv")
Gamestats = read.csv("/Users/yamahon/Desktop/CECS450Proj/archive/games_details.csv")

total <- merge(seasonStats,Gamestats,by="player_name")
print(total)
library(sqldf)
library(ggplot2)
#graphs height vs avg pt per season
sql <- "SELECT player_height, AVG(seasonStats.pts) as pts
        FROM seasonStats INNER JOIN Gamestats
        using(player_name)
        GROUP BY player_height"
result <- sqldf(sql)       

graph <-ggplot(data= result,mapping = aes(x = player_height,y=pts)) + geom_point()
print(graph)
