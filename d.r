energySub = read.csv("~/Documents/Github/CECS450Proj/archive/global-energy-substitution.csv")

# Data presents primary energy consumption via 'substitution method'
# Substitution method attempts to correct for the energy inefficiencies (energy wasted as heat during combustion) in fossil fuel and biomass conversion.  
# https://www.kaggle.com/datasets/mohamedyosef101/global-energy-substitution-from-1983-to-2022

# Other renewables: Primary energy consumption from other renewable (measured in terawatt-hours)
# All energy category measured in terra-watt hours

carPrice = read.csv("~/Documents/Github/CECS450Proj/archive/CarPrice_Assignment.csv")

energyPriceNY = read.csv("~/Documents/Github/CECS450Proj/archive/energyPrice_dollars_per_million.csv")

# https://catalog.data.gov/dataset/energy-prices-dollars-per-million-btu-beginning-1970
# New York Energy Prices present retail energy price data.  Energy prices are provided by fuel types in nominal dollars per million Btu.

energy = read.csv("~/Documents/Github/CECS450Proj/archive/energy.csv")

library(ggplot2)
library(sqldf)
sql <- "SELECT *
        FROM energy
        WHERE commodity_transaction LIKE '%Total energy supply%'"
totalEnergySupply <- sqldf(sql)

graph <- ggplot(data = totalEnergySupply, mapping = aes(x = year)) + geom_line(aes(y = output))