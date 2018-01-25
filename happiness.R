load.libraries <- c('gdata', 'plm', 'ggplot2')
install.lib <- load.libraries[!load.libraries %in% installed.packages()]
for(libs in install.lib) install.packages(libs, dependencies = TRUE)
sapply(load.libraries, require, character = TRUE)


data <- read.xls('database-whr17.xlsx')
ginibase <- read.xls('database-ginimultisources.xlsx')

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