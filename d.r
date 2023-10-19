seasonStats = read.csv("~/Documents/GitHub/CECS450Proj/archive/all_seasons.csv")
Gamestats = read.csv("~/Documents/GitHub/CECS450Proj/archive/games_details.csv")

# plus_minus calculation:
# Team points scored while that player is on the court - team points allowed while that player is on court

library(ggplot2)
# Graphing plus_minus player stats vs minutes played
graph <- ggplot(data = Gamestats, mapping = aes(x = min, y = plus_minus)) + geom_point()
# I think this graph demonstrate how effective a player will be with the amount of time the player can play
print(graph)
