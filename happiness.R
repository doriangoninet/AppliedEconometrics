load.libraries <- c('gdata', 'ggplot2', 'xlsx', 'MNP', 'psych', 'foreign', 'nnet', 'reshape2')
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

finaldata$RLadder <- as.factor(finaldata$RLadder)
finaldata$Continent <- as.factor(finaldata$Continent)

# Descriptive Statistics
summary(finaldata)
describe(finaldata)
summary(finaldata$RLadder)
summary(finaldata$Continent)



plot(RLadder ~ GDP, finaldata)
abline(lm(RLadder ~ GDP, finaldata))


ggplot(finaldata) +
  geom_histogram(mapping = aes(x = Continent, fill = RLadder), stat = "count", binwidth = 5) +
  scale_fill_grey()


# Multinomial Logistic Regression - from the nnet package
## Choosen because it does not require the data to be reshaped

### First, we choose the level of our outcome that we wish to use as our baseline
finaldata$RLadder2 <- relevel(finaldata$RLadder, ref ="3")

### Run our model
test <- multinom(RLadder2 ~ GDP + GINI + CO2 + Continent + CPW, data = finaldata)
summary(test)

### Calculate P-Values
z <- summary(test)$coefficients/summary(test)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) *2
p

head(fitted(test))