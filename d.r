energySub = read.csv("archive/global-energy-substitution.csv")

# Data presents primary energy consumption via 'substitution method'
# Substitution method attempts to correct for the energy inefficiencies (energy wasted as heat during combustion) in fossil fuel and biomass conversion.  
# https://www.kaggle.com/datasets/mohamedyosef101/global-energy-substitution-from-1983-to-2022

# Other renewables: Primary energy consumption from other renewable (measured in terawatt-hours)
# All energy category measured in terra-watt hours

carPrice = read.csv("archive/CarPrice_Assignment.csv")

energyPriceNY = read.csv("archive/energyPrice_dollars_per_million.csv")

# https://catalog.data.gov/dataset/energy-prices-dollars-per-million-btu-beginning-1970
# New York Energy Prices present retail energy price data.  Energy prices are provided by fuel types in nominal dollars per million Btu.

energy = read.csv("archive/energy.csv")

options(scipen = 999)
library(ggplot2)
library(sqldf)
library(dplyr)
sql <- "SELECT *
        FROM energy
        WHERE commodity_transaction LIKE '%Total energy supply%'
        AND unit LIKE '%Terajoules%'"
totalEnergySupplyTerra <- sqldf(sql)

graph <- ggplot(data = totalEnergySupplyTerra, mapping = aes(x = year, y = quantity, color = commodity_transaction, group = commodity_transaction)) + geom_point() + geom_line() + labs(y = 'Quantity (TerraJoules)', x = 'Year', color = 'Type of Energy', title = 'Type of Energy over time') + scale_y_continuous(labels = scales::comma)

print(graph)

graph_removed <-totalEnergySupplyTerra %>%
  filter(commodity_transaction != 'Natural gas (including LNG) - total energy supply') %>%
  ggplot( aes(x = year, y = quantity, color = commodity_transaction, group = commodity_transaction)) + geom_point() + geom_line() + labs(y = 'Quantity (TerraJoules)', x = 'Year', color = 'Type of Energy', title = 'Type of Energy (except Natural Gas) over time') + scale_y_continuous(labels = scales::comma)

print(graph_removed)

intelCPU = read.csv("archive/Intel_CPUs.csv")
gpu = read.csv("archive/ALL_GPUs.csv")

gpuGraph <- ggplot(data = gpu, mapping = aes(x = Release_Date, y = Memory)) + geom_line() + geom_point()
print(gpuGraph)
