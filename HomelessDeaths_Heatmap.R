install.packages("maps")
install.packages("mapdata")
install.packages("maptools")
install.packages("rgdal")
install.packages("ggmap")
install.packages("rgeos")
install.packages("broom")
install.packages("plyr")

library(tidyverse)
library(psych)
library(data.table)
library(maps)
library(mapdata)
library(maptools)
library(rgdal)
library(ggmap)
library(ggplot2)
library(rgeos)
library(broom)
library(plyr)


setwd("D:/")

### Data sources and Methodology Approach
##Level of homelessness: https://www.gov.uk/government/statistics/statutory-homelessness-in-england-financial-year-2020-21
##Shelters method of counting homeless pops: https://england.shelter.org.uk/media/press_release/274000_people_in_england_are_homeless_with_thousands_more_likely_to_lose_their_homes
## LAU1 Populations: (https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationestimatesforukenglandandwalesscotlandandnorthernireland)


### Loading shape file

shapefile <- readOGR(dsn="D:/XXXXXXXXXXXX/XXXXXXXXXX/XXXXX/England and Wales Heatmap/Map shapefiles", layer="Local_Administrative_Units_Level_1_(December_2015)_Boundaries")
#Reshape for ggplot2 using the Broom package
mapdata <- tidy(shapefile, region="lau115nm")

head(mapdata)

### Building map graph

gg <- ggplot() + geom_polygon(data = mapdata, aes(x = long, y = lat, group = group), color = "#c4c4c4", size = 0.02)
gg <- gg + coord_fixed(1) #This gives the map a 1:1 aspect ratio to prevent the map from appearing squashed
print(gg)


### Loading data set

setwd("D:/XXXXXXXXXXXX/XXXXXXXX/XXXXXXXX/England and Wales Heatmap/")

DeathsLAU1 <- read_tsv(file = "ONS Homeless Deaths Data - Table 7.tsv", col_names = TRUE)

DeathsLAU12020 <- data.table(DeathsLAU1$`Area code`, DeathsLAU1$`Area name`, DeathsLAU1$`2020 - Estimated deaths`)
colnames(DeathsLAU12020) <- c("Code", "id", "Estimated_Deaths")
## Determining deaths per 100,000 of total population within Local administrative units level 1s (Districts)

LAU1HomelessPops <- read_tsv(file = "Statutory Homelessness Simplified - A2P_Apr_20-Mar_21.tsv")

colnames(LAU1HomelessPops)[2] <- "id" #rename area name to id to join
colnames(LAU1HomelessPops)[4] <- "Total_Relief_Duty"
colnames(LAU1HomelessPops)[3] <- "Total_Prevention_Duty"
colnames(LAU1HomelessPops)[5] <- "Total_Homeless_Households"

LAU1HomelessPops[LAU1HomelessPops == ".."] <- NA

LAU1HomelessPops = subset(LAU1HomelessPops, select = -c(Total_Homeless_Households))



str(LAU1HomelessPops$Total_Prevention_Duty) # The data is characters not numerical values.

LAU1HomelessPops$Total_Prevention_Duty <- as.numeric(LAU1HomelessPops$Total_Prevention_Duty)
LAU1HomelessPops$Total_Relief_Duty <- as.numeric(LAU1HomelessPops$Total_Relief_Duty)


LAU1HomelessPops$Total_Homeless_Households <- (LAU1HomelessPops$Total_Prevention_Duty + LAU1HomelessPops$Total_Relief_Duty)

DeathsPopsLAU12020 <- join(LAU1HomelessPops, DeathsLAU12020, by = "id" )


## Time to calculate rate of death per 100 homeless households in each local authority - this way data is standardised and can be easily compared.

#DeathsPopsLAU12020$Total_Homeless_Duty <- as.numeric(DeathsPopsLAU12020$Total_Homeless_Households)

DeathsPopsLAU12020$Deaths_Per_100 <- (DeathsPopsLAU12020$Estimated_Deaths/DeathsPopsLAU12020$`Total_Homeless_Households`) * 100
DeathsPopsLAU12020$Deaths_Per_100 <- round(DeathsPopsLAU12020$Deaths_Per_100, digits = 2)
DeathsPopsLAU12020$Deaths_Per_Household <- (DeathsPopsLAU12020$Estimated_Deaths/DeathsPopsLAU12020$Total_Homeless_Households)
DeathsPopsLAU12020$Deaths_Per_Household <- round(DeathsPopsLAU12020$Deaths_Per_Household, digits = 2)

DeathsPopsLAU12020$Deaths_Per_HouseholdScaled <- scale(x = DeathsPopsLAU12020$Deaths_Per_Household)
mapdata <- join(mapdata, DeathsPopsLAU12020, by="id") #merge the two datasets.

### Using pp data to find geography 
LAU1Pops <- read_tsv(file = "LAU1  Populations - Sheet1.tsv")
colnames(LAU1Pops)[2] <- "id" #rename area name to id to join
LAU1Pops_Clean <- data.table(LAU1Pops$id, LAU1Pops$Geography)
colnames(LAU1Pops_Clean) <- c("id", "Geography")

mapdata <- join(mapdata,  LAU1Pops_Clean, by="id")

#separating London boroughs from england and wales

mapdatalondon <- subset(mapdata, Geography == "London Borough")
mapdataNonLondon <- subset(mapdata, Geography!="London Borough")

# Standard London Graph

ggLondon <- ggplot() + geom_polygon(data = mapdatalondon, aes(x = long, y = lat, group = group), color = "#c4c4c4", size = 0.02)
ggLondon <- ggLondon + coord_fixed(1) #This gives the map a 1:1 aspect ratio to prevent the map from appearing squashed
print(ggLondon)

#######

# Standard NonLondon Graph

ggNonLondon <- ggplot() + geom_polygon(data = mapdataNonLondon, aes(x = long, y = lat, group = group), color = "#c4c4c4", size = 0.02)
ggNonLondon <- ggNonLondon + coord_fixed(1) #This gives the map a 1:1 aspect ratio to prevent the map from appearing squashed
print(ggNonLondon)

# Converting mapdata to heatmap

#ggmap <- ggplot() + geom_polygon(data = mapdata, aes(x = long, y = lat, group = group, fill = Deaths_Per_100), color = "#c4c4c4", size = 0.02)
#ggmap <- ggmap + scale_fill_gradient2(low = "darkred", mid = "orange", high = "yellow", na.value = "gray50", name = "Deaths per 100 Homeless Households")
#ggmap <- ggmap + coord_fixed(1)
#ggmap <- ggmap + theme_minimal()
#ggmap <- ggmap + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.title = element_text(size = 14), plot.title = element_text(size = 18))
#ggmap <- ggmap + theme(axis.title.x=element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
#ggmap <- ggmap + theme(axis.title.y=element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())
#ggmap <- ggmap + labs(title = "Deaths of homeless people in England, 2020, per 100 Homeless Households")
#print(ggmap)



# converting NonLondon to heatmap


ggNonLondon <- ggplot() + geom_polygon(data = mapdataNonLondon, aes(x = long, y = lat, group = group, fill = Deaths_Per_100), color = "#c4c4c4", size = 0.02)
ggNonLondon <- ggNonLondon + scale_fill_gradient2(low = "darkred", mid = "orange", high = "yellow", na.value = "gray50", name = "Deaths per 100 Homeless Households", midpoint = 4.17)
ggNonLondon <- ggNonLondon + coord_fixed(1)
ggNonLondon <- ggNonLondon + theme_minimal()
ggNonLondon <- ggNonLondon + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.title = element_text(size = 14), plot.title = element_text(size = 18))
ggNonLondon <- ggNonLondon + theme(axis.title.x=element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(), legend.position = "none")
ggNonLondon <- ggNonLondon + theme(axis.title.y=element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())
ggNonLondon <- ggNonLondon + labs(title = "Deaths of homeless people in England, 2020, per 100 Homeless Households in Local Authority Districts")
print(ggNonLondon)

# converting London to heatmap

ggLondon <- ggplot() + geom_polygon(data = mapdatalondon, aes(x = long, y = lat, group = group, fill = Deaths_Per_100), color = "#c4c4c4", size = 0.02)
ggLondon <- ggLondon + scale_fill_gradient2(low = "darkred", mid = "orange", high = "yellow", na.value = "grey50", name = "Deaths per 100 Homeless Households", midpoint = 4.17)
ggLondon <- ggLondon + coord_fixed(1)
ggLondon <- ggLondon + theme_minimal()
ggLondon <- ggLondon + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.title = element_text(size = 14), legend.position = "left")
ggLondon <- ggLondon + theme(axis.title.x=element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
ggLondon <- ggLondon + theme(axis.title.y=element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())
print(ggLondon)

## Writing out data frame for deaths per LAU1
setwd("D:/XXXXXXXXXX/XXXXXX/XXXXXXXX")

write_tsv(x = DeathsPopsLAU12020, file = "Clean Data/Deaths_per_100k.tsv", col_names = TRUE, na = "NA", quote = "none")

#combining original data frame with additional info about homeless households

Deaths_Clean <- read_tsv(file = "Clean Data/Deaths_CleanCombined.tsv")

HomelessPops2 <- data.table(LAU1HomelessPops$id, LAU1HomelessPops$Total_Homeless_Households)
colnames(HomelessPops2) <- c("Local Authority Districts", "Total Homeless Households")

Deaths_Clean <- join(Deaths_Clean,  HomelessPops2, by="Local Authority Districts")

colnames(Deaths_Clean)[4] <- "Deaths_Per_100k"

Deaths_Clean <- subset(Deaths_Clean, select = -c(Deaths_Per_100k))

Deaths <- data.table(DeathsPopsLAU12020$id, DeathsPopsLAU12020$Deaths_Per_100)
colnames(Deaths) <- c("Local Authority Districts", "Deaths per 100 Homeless Households")

Deaths_Clean <- join(Deaths_Clean, Deaths, by = "Local Authority Districts")

write_tsv(x = Deaths_Clean, file = "Clean Data/Deaths_Clean_Final.tsv", col_names = TRUE, na = "NA", quote = "none")
