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
library(stringr)
library(anytime)
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
# https://www.kaggle.com/datasets/iliassekkaf/computerparts/
gpu = read.csv("archive/ALL_GPUs.csv")
# https://www.kaggle.com/datasets/iliassekkaf/computerparts/

# Remove rows where max_power is null and removed rows with release_date that states "Unknown Release Date"
gpuCleaned <- gpu %>% filter(Max_Power != '') %>% filter(!str_detect(Release_Date, "Unknown Release Date"))
# Remove the words "watts" in Max_Power column
gpuCleaned <- gpuCleaned %>% mutate(Max_Power = str_remove_all(Max_Power, " Watts"))
# Convert the Max_power column to numeric
gpuCleaned <- transform(gpuCleaned, Max_Power = as.numeric(Max_Power))
# Convert the Release_Date column to date class instead of char class
gpuCleaned$Release_Date <- anydate(gpuCleaned$Release_Date)

gpuGraph <- gpuCleaned %>% ggplot(aes(x = Release_Date, y = Max_Power)) + geom_point() + labs(y = 'Max Power of GPU (Watts)', x = 'Release Date', title = 'GPU Max Power over time')
print(gpuGraph)

chipData = read.csv("archive/chip_dataset.csv")
# https://www.kaggle.com/datasets/michaelbryantds/cpu-and-gpu-product-data

# TDP = Thermal design power.  Tells the maximum heat a computer chip can use in watts.  More watts = more performance but more power consumption
# Filter type to CPU
cpuData <- chipData %>% filter(Type == "CPU")
# Convert release date to date object instead of char
cpuData$Release.Date <- anydate(cpuData$Release.Date)
cpuGraph <- cpuData %>% ggplot(aes(x = Release.Date, y = TDP..W.)) + geom_point() + labs(y = 'TDP (watts)', x = 'Release Date', title = "CPU TDP over time")
print(cpuGraph)


