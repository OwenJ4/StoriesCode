library(tidyverse)
library(psych)
library(data.table)
library(ggmap)
library(ggplot2)
library(broom)
library(plyr)
library(dplyr)
library(jtools)


## Data used for Deprivation Statistics : https://www.ons.gov.uk/peoplepopulationandcommunity/personalandhouseholdfinances/incomeandwealth/datasets/mappingincomedeprivationatalocalauthoritylevel
## This is to test correlations between homeless households, deaths and socioeconomic measures
## Note there are difficulties in assessing socioeconomic status within local authority districts. 
#Please see this visualisation of differences even between streets (https://www.ons.gov.uk/visualisations/dvc1371/#/E07000223)
# Data will also be posted on the github page.

Deaths_Clean <- read_tsv(file = "Clean Data/Deaths_Clean_Final.tsv", col_names = TRUE)
#Renaming variables so I can actually use them
colnames(Deaths_Clean)[4] <- "Income_Deprivation_Average"
colnames(Deaths_Clean)[6] <- "Deprivation_Gap"
colnames(Deaths_Clean)[8] <- "Total_Homeless_Households"
colnames(Deaths_Clean)[9] <-"Deaths_Per_100_Household"

plot(Deaths_Per_100_Household ~ Income_Deprivation_Average, data = Deaths_Clean)
plot(Deaths_Per_100_Household ~ Deprivation_Gap, data = Deaths_Clean)
plot(Total_Homeless_Households ~ Income_Deprivation_Average, data = Deaths_Clean)
plot(Total_Homeless_Households ~ Deprivation_Gap, data = Deaths_Clean)

# Testing normality

shapiro.test(Deaths_Clean$Income_Deprivation_Average)
shapiro.test(Deaths_Clean$Deprivation_Gap)
shapiro.test(Deaths_Clean$Total_Homeless_Households)
shapiro.test(Deaths_Clean$Deaths_Per_100_Household)

# All variables are non-parametric, therefore the best correlation method to choose is spearman.
# The downside is that spearmans assesses a correlational relationship using a monotonic function.
# i.e. as one variable increases so does the other, and vice versa. Unlike pearson's it can be linear or non-linear.
# To test if linear, a lm model can also be used later.
               
corrIncomeDeath <- corr.test(x = Deaths_Clean$Income_Deprivation_Average, y = Deaths_Clean$Deaths_Per_100_Household, method = "spearman")
print(corrIncomeDeath, short = FALSE)
print(corrIncomeDeath$p, short = FALSE)
corrIncomeHouse <- corr.test(x = Deaths_Clean$Income_Deprivation_Average, y = Deaths_Clean$Total_Homeless_Households, method = "spearman")
print(corrIncomeHouse, short = FALSE)
print(corrIncomeHouse$p, short= FALSE)

corrDepGapDeath <- corr.test(x = Deaths_Clean$Deprivation_Gap, y = Deaths_Clean$Deaths_Per_100_Household, method = "spearman")
print(corrDepGapDeath, short = FALSE)
print(corrDepGapDeath$p, short = FALSE)

corrDepGapHouse <- corr.test(x = Deaths_Clean$Deprivation_Gap, y = Deaths_Clean$Total_Homeless_Households, method = "spearman")
print(corrDepGapHouse, short = FALSE)
print(corrDepGapHouse$p, short = FALSE)


## All were significant, and the correlation coefficient was positive. This suggests as the x variable increases so does y. 
# However, this does not suggest a cause, only that as one increases so does the other, they may have variables in common.
# It also does not imply linearity, the association may be more complex. If it is exponential then it would suggest as x variable increases then y increases more so for each increase in x.

# lets plot lines of best fit:

# fit a non-linear regression line

p1 <- ggplot(Deaths_Clean, aes(Income_Deprivation_Average, Deaths_Per_100_Household)) + geom_point() + geom_smooth()
p1
p2 <- ggplot(Deaths_Clean, aes(Deprivation_Gap, Deaths_Per_100_Household)) + geom_point() + geom_smooth()
p2
p3 <- ggplot(Deaths_Clean, aes(Income_Deprivation_Average, Total_Homeless_Households)) + geom_point() + geom_smooth()
p3
p4 <- ggplot(Deaths_Clean, aes(Deprivation_Gap, Total_Homeless_Households)) + geom_point() + geom_smooth()
p4


# fit a linear regression lines - or lines of best-fit

p1l <- ggplot(Deaths_Clean, aes(x=Income_Deprivation_Average, y=Deaths_Per_100_Household)) +
  geom_point() + geom_smooth(method=lm, se=FALSE)
p1l
p2l <- ggplot(Deaths_Clean, aes(x=Deprivation_Gap, y=Deaths_Per_100_Household)) +
  geom_point() + geom_smooth(method=lm, se=FALSE)
p2l
p3l <- ggplot(Deaths_Clean, aes(x=Income_Deprivation_Average, y=Total_Homeless_Households)) +
  geom_point() + geom_smooth(method=lm, se=FALSE)
p3l

p4l <- ggplot(Deaths_Clean, aes(x=Deprivation_Gap, y=Total_Homeless_Households)) +
  geom_point() + geom_smooth(method=lm, se=FALSE)
p4l

## p3 and p4 seem to best explained by a linear relationship between variables

#lm.model: linear regression models do not ormally distributed variables
lm.model1 <- lm(Deaths_Per_100_Household ~ Income_Deprivation_Average + Deprivation_Gap, data = Deaths_Clean)
summary(lm.model1)
lm.model2 <- lm(Total_Homeless_Households ~ Income_Deprivation_Average + Deprivation_Gap, data = Deaths_Clean)
summary(lm.model2)

#lm.model1 is not significant, suggesting there is not a linear association between Deaths and variables.
#This would lead me to question the correlational relationship for these variables the graphs were very flat, perhaps the relationship is driven by specific outliers?
#City of London, Liverpool and Oxford, what is special about them?


# Is there an interaction between Income_Deprivation_Average and Deprivation_Gap on total homeless households?
lm.model3 <- lm(Total_Homeless_Households ~ Income_Deprivation_Average * Deprivation_Gap, data = Deaths_Clean)
summary(lm.model3)

# There is not an interaction effect in lm.model3 which suggests that Deprivation Gap and Income Deprivation Average are seperately associated with total homeless households. 


summary(Deaths_Clean$`Total Homeless Households`)
