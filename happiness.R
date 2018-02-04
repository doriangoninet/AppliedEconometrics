load.libraries <- c('gdata', 'ggplot2', 'xlsx', 'MNP', 'psych')
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




# Load final database
finaldata <- read.xlsx('database-whr17-v5.xlsx', 1)

# Statistics
summary(finaldata)
describe(finaldata)


# Multi-nominal probit
mnp <- mnp(RLadder ~ GDP + GINI + CO2, finaldata)
summary(mnp)



lm <- lm(Ladder2 ~ GDP + GINI, data2)
summary(lm)

plot(Ladder2 ~ GDP, data2)
abline(lm(Ladder2 ~ GDP, data2))

ggplot(data = data2, x=GDP, y=Ladder2)

summary(data)
summary(data$year)
dim(data$year)
str(data)

data2 <- na.exclude(data)
summary(data2$Country)

glm <- glm(Ladder2 ~ GDP + GINI, data2, family=binomial(link = "probit"))
Ladder2 <-data2$Ladder*0.1