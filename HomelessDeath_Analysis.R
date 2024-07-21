### Install Packages ###

install.packages("tidyverse")
install.packages("psych")
install.packages("data.table")
install.packages("ggplot2")
install.packages("corrplot")
install.packages("scales")
install.packages("devtools")
install.packages("ggthemes")



### Load Packages ###
library(tidyverse)
library(psych)
library(data.table)
library(ggplot2)
library(corrplot)
library(scales)
library(devtools)
library(ggthemes)

install_github("hrbrmstr/waffle")

library(waffle)

### Homeless Deaths 2013 - 2020 ###

# the data: https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/deathsofhomelesspeopleinenglandandwales


## Reading tab-seperated file (Import data) ##

Deaths_Raw <- read_tsv(file = "Raw data/Deaths_Raw.tsv", col_names = TRUE)

## Flipping and Cleaning Table for Graph ##

Deaths_Clean <- data.table(Deaths_Raw$`Year of death registration`, Deaths_Raw$`Identified deaths`, Deaths_Raw$`Estimated deaths`, Deaths_Raw$`Standard error`)
colnames(Deaths_Clean) <- c('Year of Death Registration', 'Identified Deaths', 'Estimated Deaths', 'Standard Error')

Deaths_Flipped <- read_tsv(file = "Raw data/Deaths_Clean Flipped.tsv", col_names = TRUE)
colnames(Deaths_Flipped) <- c('Deaths', 'Data', 'Year', 'SE')

## The Graph ##

legend_title <- "Death Registration Data"

p1 <- ggplot(Deaths_Flipped, aes(x=Year, y=Deaths, group=Data, color=Data)) + scale_color_manual(values=c("#FF8080", "#809FFF")) +
  geom_pointrange(aes(ymin=Deaths-SE, ymax=Deaths+SE), size = 0.9) + scale_x_continuous(breaks = seq(2013,2020)) + scale_y_continuous(limits = c(380, 820), breaks = seq(380, 820, by = 20)) +
  geom_line(aes(linetype=Data), size = 1.3) + geom_point() + scale_linetype_manual(values=c("dashed", "solid")) + labs(caption = "Note: The vertical lines above and below the estimated data points are the standard error. The standard error represents the uncertainty around estimating registered deaths. ", title = "Registered deaths of homeless people in England and Wales", subtitle = "2013 - 2020") + 
  theme(axis.line = element_line(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text = element_text(size = 16), axis.title = element_text(size = 18), legend.text = element_text(size = 18), legend.title = element_blank(), plot.caption = element_text(size = 16), plot.title = element_text(size = 20), plot.subtitle = element_text(size = 18))

p1

### Homeless Deaths by Gender and Age 2013 - 2020 ###


Gender_Raw <- read_tsv(file = "Raw data/ONS Homeless Deaths Data - Table 2.tsv", col_names = TRUE)

# Creating a total of deaths from all years

Gender_Raw$total <- (Gender_Raw$`2020` + Gender_Raw$`2019` + Gender_Raw$`2018` + Gender_Raw$`2017` + Gender_Raw$`2016` + Gender_Raw$`2015` + Gender_Raw$`2014` + Gender_Raw$`2013`)

# Extracting only Identified deaths and creating a subset data frame
# (Ages 15 - 74 to remove deaths associated with institutionalisation due to old age, following ONS methodology)

Gender_IDMen <- Gender_Raw %>% slice(2:13)
Gender_IDWomen <- Gender_Raw %>% slice(15:26)

# Sum of totals across ages. The total total.

totalMen <- sum(Gender_IDMen$total)
totalWomen <-sum(Gender_IDWomen$total)

# Creating table 

df <- data.frame(Sex=rep(c('Men', 'Women'), each = 1))
df2 <- data.frame(Deaths=rep(c(totalMen, totalWomen), each = 1))
totalcombinedID <- data.table(df, df2)


## the square pie chart



vals <- c(3320, 455)
val_names <- sprintf("%s (%s)", c("Men", "Women"), scales::percent(round(vals/sum(vals), 2)))
names(vals) <- val_names


p2 <- ggplot(totalcombinedID, aes(fill = Sex, values = Deaths/25)) +
  geom_waffle(n_rows = 8, colour = "white") +
  scale_fill_manual(name=NULL, values = c("#FF8080", "#809FFF"), labels = val_names) + 
  theme(legend.text = element_text(size = 18), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.1, size = 20),
        plot.subtitle = element_text(hjust = 0.047, size = 18),
        plot.caption = element_text(size = 16)) + 
  labs(caption = "Note: Each square represents 151 people.", title = "Identified registered deaths of homeless men and women in England and Wales", subtitle = "2013 - 2020") + coord_equal()

p2


## top 10 causes of deaths

under_deaths <- read_tsv(file = "Raw data/ONS Homeless Deaths Data - Table 3.tsv", col_names = TRUE)

under_deaths2020 <- data.table(under_deaths$`ICD-10 Codes`, under_deaths$`ICD10 sub chapter grouping`, under_deaths$`Type of death`, under_deaths$`2020`)
colnames(under_deaths2020) <- c("ICD-10_Codes", "ICD10_sub-chapter_grouping", "Type_of_Death", "Deaths")

## extracting Identified deaths only

under_deaths2020 <- subset(under_deaths2020, Type_of_Death == "Identified deaths")

#sorting table by identified value

under_deaths2020 <- under_deaths2020 %>% arrange(desc(under_deaths2020$Deaths)) # 70.20% of deaths caused by top 5!

under_deaths2020top5 <- subset(under_deaths2020)[1:5] # TOP 5

#V01-X59 transport accidents to accidental exposure - essentially all accidents thinkable. This why it appears to large.
#K70-K77 liver deaths - all causes

### difficult to assess due to range of causes within each sub group of underlying causes of death - e.g. what kind of transport accident, what disease of the liver? Makes it very difficult to compare to the general population. It would need to be converted into rates.

write_tsv(under_deaths2020top5, file = "Clean Data/Underlying Deaths in 2020 top 5.tsv", col_names = TRUE, quote = "none", na = "NA")
