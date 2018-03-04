load.libraries <- c('gdata', 'ggplot2', 'xlsx', 'MNP', 'psych', 'foreign', 'nnet', 'reshape2', 'xtable', 'XML')
install.lib <- load.libraries[!load.libraries %in% installed.packages()]
for(libs in install.lib) install.packages(libs, dependencies = TRUE)
sapply(load.libraries, require, character = TRUE)



# Import databases to merge
data <- read.xlsx('database-whr17.xlsx', 1)
ginibase <- read.xlsx('database-ginimultisources.xlsx', 1)
# Merge databases
finaldata <- merge(data, ginibase, by.x=c('Country', 'Year'), by.y=c('Country', 'Year'))
# Select columns to keep
finaldata <- finaldata[, c(1:2, 4:5, 10)]
# Remove duplicates
duplicates <- which(duplicated(finaldata))
finaldata <- finaldata[-duplicates,]

co2base <- read.csv('database-co2.csv')
finaldata <- merge(finaldata, co2base, by.x=c('Country', 'Year'), by.y=c('Entity', 'Year'))
finaldata <- rename.vars(finaldata, c('COâ...emissions.per.capita..per.year...tonnes.per.year.', 'GINIR'), c('CO2', 'GINI'))
finaldata <- remove.vars(finaldata, 'Code')

write.xlsx(finaldata, file = 'database-whr17-v2.xlsx', col.names = TRUE, row.names = FALSE)



cpwbase <- read.csv('database-cpw.csv')

finaldata <- merge(finaldata, cpwbase, by.x=c('Country', 'Year'), by.y=c('Entity', 'Year'))
finaldata <- finaldata[, c(1:7, 9)]
finaldata <- rename.vars(finaldata, c('Estimates..1950...2015..Demographic.Indicators...Total.fertility..live.births.per.woman...live.births.per.woman.'), c('CPW'))

write.xlsx(finaldata, file = 'database-whr17-v4.xlsx', col.names = TRUE, row.names = FALSE)

# Remove the NAs
finaldata <- read.xlsx('database-whr17-v4.xlsx', 1)
finaldata <- na.omit(finaldata)
write.xlsx(finaldata, file = 'database-whr17-v5.xlsx', col.names = TRUE, row.names = FALSE)

# merge Continent3-Continent4 and Continent5-Continent6
# remove RLadder3 and RLadder8
finaldata <- read.xlsx('database-whr17-v5.xlsx', 1)
finaldata <- subset(finaldata, RLadder != 3)
finaldata <- subset(finaldata, RLadder != 8)
finaldata$Continent[finaldata$Continent==3] <- 4
finaldata$Continent[finaldata$Continent==5] <- 6


write.xlsx(finaldata, file = 'database-whr17-v6.xlsx', col.names = TRUE, row.names = FALSE)

# V8
## Adding real GDP and renaming GDP by logGDP
finaldata <- read.xlsx('database-whr17-v6.xlsx', 1)
finaldata <- rename.vars(finaldata, 'GDP', 'logGDP')
GDP <- exp(finaldata$logGDP)
finaldata <- data.frame(finaldata, GDP)
## Defining RLadder & Continent as factor terms
finaldata$RLadder <- as.factor(finaldata$RLadder)
finaldata$Continent <- as.factor(finaldata$Continent)
## Writing new dataset
write.xlsx(finaldata, file = 'database-whr17-v7.xlsx', col.names = TRUE, row.names = FALSE)


# Real V8-9
## Add of a new variable:
### Europe = 0 : not being in Europe
### Europe = 1 : being in West Europe
### Europe = 2 : being in East Europe
### Continent = 1 : Europe
### Continent = 2 : Africa
### Continent = 3 : 
### Continent = 4 : America
### Continent = 5 : 
### Continent = 6 : Asia
### Continent = 7 : Middle East
finaldata <- read.xlsx('database-whr17-v8.xlsx', 1)
finaldata <- finaldata[, c(1:8, 10)]

### Continent = 1 : West Europe
### Continent = 2 : East Europe
### Continent = 3 : Africa
### Continent = 4 : Middle East
### Continent = 5 : Asia
### Continent = 6 : America
NContinent <- 0
finaldata <- cbind(finaldata, NContinent)
finaldata[finaldata$Europe == 1, which(colnames(finaldata)=="NContinent")] <- 1
finaldata[finaldata$Europe == 2, which(colnames(finaldata)=="NContinent")] <- 2
finaldata[finaldata$Continent == 2, which(colnames(finaldata)=="NContinent")] <- 3
finaldata[finaldata$Continent == 7, which(colnames(finaldata)=="NContinent")] <- 4
finaldata[finaldata$Continent == 6, which(colnames(finaldata)=="NContinent")] <- 5
finaldata[finaldata$Continent == 4, which(colnames(finaldata)=="NContinent")] <- 6

finaldata$Continent <- finaldata$NContinent
finaldata <- finaldata[, c(1:8)]
finaldata$Continent <- as.factor(finaldata$Continent)

WestEurope <- 0
EastEurope <- 0
Africa <- 0
MiddleEast <- 0
AsiaOceania <- 0
America <- 0
finaldata <- cbind(finaldata, WestEurope, EastEurope, Africa, MiddleEast, AsiaOceania, America)
finaldata[finaldata$Continent == 1, which(colnames(finaldata)=="WestEurope")] <- 1
finaldata[finaldata$Continent == 2, which(colnames(finaldata)=="EastEurope")] <- 1
finaldata[finaldata$Continent == 3, which(colnames(finaldata)=="Africa")] <- 1
finaldata[finaldata$Continent == 4, which(colnames(finaldata)=="MiddleEast")] <- 1
finaldata[finaldata$Continent == 5, which(colnames(finaldata)=="AsiaOceania")] <- 1
finaldata[finaldata$Continent == 6, which(colnames(finaldata)=="America")] <- 1
finaldata <- finaldata[, c(1:6, 8:14)]

write.xlsx(finaldata, file = 'database-whr17-v9.xlsx', col.names = TRUE, row.names = FALSE)
