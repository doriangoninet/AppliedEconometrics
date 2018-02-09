load.libraries <- c('gdata', 'ggplot2', 'xlsx', 'MNP', 'psych', 'foreign', 'nnet', 'reshape2', 'xtable')
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
