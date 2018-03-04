load.libraries <- c('gdata', 'ggplot2', 'xlsx', 'MNP', 'psych', 'foreign', 'nnet', 'reshape2', 'xtable', 'margins', 'stargazer')
install.lib <- load.libraries[!load.libraries %in% installed.packages()]
for(libs in install.lib) install.packages(libs, dependencies = TRUE)
sapply(load.libraries, require, character = TRUE)



finaldata <- read.xlsx('database-whr17-v7.xlsx', 1)



# Descriptive Statistics
summary(finaldata)
describe(finaldata)
summary(finaldata$RLadder)
summary(finaldata$Continent)

# First Plots
plot(RLadder ~ GDP, finaldata)
abline(lm(RLadder ~ GDP, finaldata))

ggplot(finaldata) +
  geom_histogram(mapping = aes(x = Continent, fill = RLadder), stat = "count", binwidth = 5) +
  scale_fill_grey()



# Multinomial Logistic Regression - from the nnet package
## Choosen because it does not require the data to be reshaped
### First, we choose the level of our outcome that we wish to use as our baseline
finaldata$RLadder2 <- relevel(finaldata$RLadder, ref ="4")

### Run our models
test1a <- multinom(RLadder2 ~ logGDP, data = finaldata)
test1b <- multinom(RLadder2 ~ GDP, data = finaldata)
test2 <- multinom(RLadder2 ~ logGDP + GINI + CO2 + Continent + CPW, data = finaldata)

### Calculate P-Values
z <- summary(test1a)$coefficients/summary(test1a)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) *2
p

### Predicting obs.
head(fitted(test))

### Other method
dlogGDP <- data.frame(Continent = rep(c("1", "2", "4", "6", "7"), each = 700), logGDP = rep(c(6:12), 5), GINI = rep(c(0:1), 5), CO2 = rep(c(0:9), 5), CPW = rep(c(0:4), 5))
pplogGDP <- cbind(dlogGDP, predict(test2, newdata = dlogGDP, type  ="probs", se = TRUE))
by <- by(pplogGDP[, 6:9], pplogGDP$Continent, colMeans)

pp2logGDP <- data.frame(pplogGDP[, 1:2], pplogGDP[, 6:9])

lpp <- melt(pp2logGDP, id.vars = c("Continent", "logGDP"), value.name = "probability")
head(lpp)

ggplot(lpp, aes(x = logGDP, y = probability, colour = Continent)) +
  geom_line() +
  facet_grid(variable ~ ., scales = "free")


### entre 7 et 8, la proba majoritaire passe du goupe 4 au groupe 5
### proba de passer dans le groupe 5 c'est avec GDP = 7,89, 2670$
### proba de passer dans le groupe 6 c'est avec GDP = 9,51, 13494$
### proba de passer dans le groupe 7 c'est avec GDP = 
### proba de passer dans le groupe 4, c'est ace GDP = 

coef <- summary(test1a)$coefficients

gr56 <- (coef[2,1] - coef[1,1])/(coef[1,2] - coef[2,2])
gr67 <- (coef[3,1] - coef[2,1])/(coef[2,2] - coef[3,2])
gr45 <- - coef[1,1]/coef[1,2]

f4 <- function(x) 1/(1+(exp(coef[1,1] + coef[1,2]*x)+exp(coef[2,1] + coef[2,2]*x)+exp(coef[3,1] + coef[3,2]*x)))
f5 <- function(x) exp(coef[1,1] + coef[1,2]*x)/(1+(exp(coef[1,1] + coef[1,2]*x)+exp(coef[2,1] + coef[2,2]*x)+exp(coef[3,1] + coef[3,2]*x)))
f6 <- function(x) exp(coef[2,1] + coef[2,2]*x)/(1+(exp(coef[1,1] + coef[1,2]*x)+exp(coef[2,1] + coef[2,2]*x)+exp(coef[3,1] + coef[3,2]*x)))
f7 <- function(x) exp(coef[3,1] + coef[3,2]*x)/(1+(exp(coef[1,1] + coef[1,2]*x)+exp(coef[2,1] + coef[2,2]*x)+exp(coef[3,1] + coef[3,2]*x)))


x <- seq(6, 12, length.out = 10000)
dl <- data.frame(x, f4(x), f5(x), f6(x), f7(x))

ggplot(dl) +
  geom_line(aes(x, f4.x.), linetype = "dotted", color = "gray70", size = 1) +
  geom_line(aes(x, f5.x.), linetype = "twodash", color = "gray60", size = 1) +
  geom_line(aes(x, f6.x.), linetype =  "longdash", color = "gray40", size = 1) +
  geom_line(aes(x, f7.x.), linetype = "solid", color = "gray20", size = 1) +
  geom_vline(xintercept = gr56) +
  geom_vline(xintercept = gr67) +
  geom_vline(xintercept = gr45) +
  geom_text_repel(x=gr56-0.25, y=-0.02, label=round(gr56, 2)) +
  geom_text(x=gr67+0.30, y=-0.02, label=round(gr67, 2)) +
  geom_text(x=gr45-0.20, y=-0.02, label=round(gr45, 2)) +
  ylab("Prob. to reach a specific level of happiness") +
  xlab("log of GDP per capita") +

mg <- gr45

a <- coef[1,1] + coef[1,2]*mg
b <- coef[2,1] + coef[2,2]*mg
c <- coef[3,1] + coef[3,2]*mg

d <- exp(a)/(1+(exp(a)+exp(b)+exp(c)))
e <- exp(b)/(1+(exp(a)+exp(b)+exp(c)))
f <- exp(c)/(1+(exp(a)+exp(b)+exp(c)))
g <- 1 - d - e - f

c(g, d, e, f)


##
coef2 <- summary(test2)$coefficients

a2 <- 0
a3 <- mean(finaldata$GINI)
a4 <- mean(finaldata$CO2)
a5 <- 1
a6 <- 0
a7 <- 0
a8 <- 0
a9 <- mean(finaldata$CPW)

logGDPgr56 <- (  (coef2[2,1] - coef2[1,1])    + 
                 (coef2[2,2] - coef2[1,2])*a2 + 
                 (coef2[2,3] - coef2[1,3])*a3 + 
                 (coef2[2,4] - coef2[1,4])*a4 + 
                 (coef2[2,5] - coef2[1,5])*a5 + 
                 (coef2[2,6] - coef2[1,6])*a6 + 
                 (coef2[2,7] - coef2[1,7])*a7 + 
                 (coef2[2,8] - coef2[1,8])*a8 + 
                 (coef2[2,9] - coef2[1,9])*a9    ) / (coef2[1,2] - coef2[2,2])
logGDPgr67 <- (  (coef2[3,1] - coef2[2,1])    + 
                 (coef2[3,2] - coef2[2,2])*a2 + 
                 (coef2[3,3] - coef2[2,3])*a3 + 
                 (coef2[3,4] - coef2[2,4])*a4 + 
                 (coef2[3,5] - coef2[2,5])*a5 + 
                 (coef2[3,6] - coef2[2,6])*a6 + 
                 (coef2[3,7] - coef2[2,7])*a7 + 
                 (coef2[3,8] - coef2[2,8])*a8 + 
                 (coef2[3,9] - coef2[2,9])*a9    ) / (coef2[2,2] - coef2[3,2])
logGDPgr45 <- (  (- coef2[1,1])    + 
                 (- coef2[1,2])*a2 + 
                 (- coef2[1,3])*a3 + 
                 (- coef2[1,4])*a4 + 
                 (- coef2[1,5])*a5 + 
                 (- coef2[1,6])*a6 + 
                 (- coef2[1,7])*a7 + 
                 (- coef2[1,8])*a8 + 
                 (- coef2[1,9])*a9    ) / (coef2[1,2])


mg <- logGDPgr45

a <- coef2[1,1] + coef2[1,2]*mg + coef2[1,3]*a3 + coef2[1,4]*a4 + coef2[1,9]*a9
b <- coef2[2,1] + coef2[2,2]*mg + coef2[2,3]*a3 + coef2[2,4]*a4 + coef2[2,9]*a9
c <- coef2[3,1] + coef2[3,2]*mg + coef2[3,3]*a3 + coef2[3,4]*a4 + coef2[3,9]*a9

d <- exp(a)/(1+(exp(a)+exp(b)+exp(c)))
e <- exp(b)/(1+(exp(a)+exp(b)+exp(c)))
f <- exp(c)/(1+(exp(a)+exp(b)+exp(c)))
g <- 1 - d - e - f

c(g, d, e, f)

### log of GDP per capita

a2 <- 0
a3 <- mean(finaldata$GINI)
a4 <- mean(finaldata$CO2)
a5 <- 1
a6 <- 0
a7 <- 0
a8 <- 0
a9 <- mean(finaldata$CPW)

fb4 <- function(x) 1/(1+(exp(coef2[1,1] + coef2[1,2]*x + coef2[1,3]*a3 + coef2[1,4]*a4 + coef2[1,5]*a5 + coef2[1,6]*a6 + coef2[1,7]*a7 + coef2[1,8]*a8 + coef2[1,9]*a9)+exp(coef2[2,1] + coef2[2,2]*x + coef2[2,3]*a3 + coef2[2,4]*a4 + coef2[2,5]*a5 + coef2[2,6]*a6 + coef2[2,7]*a7 + coef2[2,8]*a8 + coef2[2,9]*a9)+exp(coef2[3,1] + coef2[3,2]*x + coef2[3,3]*a3 + coef2[3,4]*a4 +  coef2[3,5]*a5 + coef2[3,6]*a6 + coef2[3,7]*a7 + coef2[3,8]*a8 + coef2[3,9]*a9)))
fb5 <- function(x) exp(coef2[1,1] + coef2[1,2]*x + coef2[1,3]*a3 + coef2[1,4]*a4 + coef2[1,5]*a5 + coef2[1,6]*a6 + coef2[1,7]*a7 + coef2[1,8]*a8 + coef2[1,9]*a9)/(1+(exp(coef2[1,1] + coef2[1,2]*x + coef2[1,3]*a3 + coef2[1,4]*a4 + coef2[1,5]*a5 + coef2[1,6]*a6 + coef2[1,7]*a7 + coef2[1,8]*a8 + coef2[1,9]*a9)+exp(coef2[2,1] + coef2[2,2]*x + coef2[2,3]*a3 + coef2[2,4]*a4 + coef2[2,5]*a5 + coef2[2,6]*a6 + coef2[2,7]*a7 + coef2[2,8]*a8 + coef2[2,9]*a9)+exp(coef2[3,1] + coef2[3,2]*x + coef2[3,3]*a3 + coef2[3,4]*a4 +  coef2[3,5]*a5 + coef2[3,6]*a6 + coef2[3,7]*a7 + coef2[3,8]*a8 + coef2[3,9]*a9)))
fb6 <- function(x) exp(coef2[2,1] + coef2[2,2]*x + coef2[2,3]*a3 + coef2[2,4]*a4 + coef2[2,5]*a5 + coef2[2,6]*a6 + coef2[2,7]*a7 + coef2[2,8]*a8 + coef2[2,9]*a9)/(1+(exp(coef2[1,1] + coef2[1,2]*x + coef2[1,3]*a3 + coef2[1,4]*a4 + coef2[1,5]*a5 + coef2[1,6]*a6 + coef2[1,7]*a7 + coef2[1,8]*a8 + coef2[1,9]*a9)+exp(coef2[2,1] + coef2[2,2]*x + coef2[2,3]*a3 + coef2[2,4]*a4 + coef2[2,5]*a5 + coef2[2,6]*a6 + coef2[2,7]*a7 + coef2[2,8]*a8 + coef2[2,9]*a9)+exp(coef2[3,1] + coef2[3,2]*x + coef2[3,3]*a3 + coef2[3,4]*a4 +  coef2[3,5]*a5 + coef2[3,6]*a6 + coef2[3,7]*a7 + coef2[3,8]*a8 + coef2[3,9]*a9)))
fb7 <- function(x) exp(coef2[3,1] + coef2[3,2]*x + coef2[3,3]*a3 + coef2[3,4]*a4 + coef2[3,5]*a5 + coef2[3,6]*a6 + coef2[3,7]*a7 + coef2[3,8]*a8 + coef2[3,9]*a9)/(1+(exp(coef2[1,1] + coef2[1,2]*x + coef2[1,3]*a3 + coef2[1,4]*a4 + coef2[1,5]*a5 + coef2[1,6]*a6 + coef2[1,7]*a7 + coef2[1,8]*a8 + coef2[1,9]*a9)+exp(coef2[2,1] + coef2[2,2]*x + coef2[2,3]*a3 + coef2[2,4]*a4 + coef2[2,5]*a5 + coef2[2,6]*a6 + coef2[2,7]*a7 + coef2[2,8]*a8 + coef2[2,9]*a9)+exp(coef2[3,1] + coef2[3,2]*x + coef2[3,3]*a3 + coef2[3,4]*a4 +  coef2[3,5]*a5 + coef2[3,6]*a6 + coef2[3,7]*a7 + coef2[3,8]*a8 + coef2[3,9]*a9)))

x <- seq(6, 12, length.out = 10000)
dl2 <- data.frame(x, fb4(x), fb5(x), fb6(x), fb7(x))

ggplot(dl2) +
  geom_line(aes(x, fb4.x.), linetype = "dotted", color = "gray70", size = 1) +
  geom_line(aes(x, fb5.x.), linetype = "twodash", color = "gray60", size = 1) +
  geom_line(aes(x, fb6.x.), linetype =  "longdash", color = "gray40", size = 1) +
  geom_line(aes(x, fb7.x.), linetype = "solid", color = "gray20", size = 1) +
  #geom_vline(xintercept = logGDPgr56) +
  #geom_vline(xintercept = logGDPgr67) +
  #geom_vline(xintercept = logGDPgr45) +
  #geom_text(x=logGDPgr56-0.25, y=-0.02, label=round(logGDPgr56, 2)) +
  #geom_text(x=logGDPgr67+0.25, y=-0.02, label=round(logGDPgr67, 2)) +
  #geom_text(x=logGDPgr45-0.25, y=-0.02, label=round(logGDPgr45, 2)) +
  ylab("Prob. to reach a specific level of happiness") +
  xlab("log of GDP per capita")


### GINI Index

finaldata$GINI <- round(finaldata$GINI, 0)

a2 <- mean(finaldata$logGDP)
a3 <- 0
a4 <- mean(finaldata$CO2)
a5 <- 0
a6 <- 0
a7 <- 0
a8 <- 0
a9 <- mean(finaldata$CPW)

fb4 <- function(x) 1/(1+(exp(coef2[1,1] + coef2[1,2]*a2 + coef2[1,3]*x + coef2[1,4]*a4 + coef2[1,9]*a9)+exp(coef2[2,1] + coef2[2,2]*a2 + coef2[2,3]*x + coef2[2,4]*a4 + coef2[2,9]*a9)+exp(coef2[3,1] + coef2[3,2]*a2 + coef2[3,3]*x + coef2[3,4]*a4 + coef2[3,9]*a9)))
fb5 <- function(x) exp(coef2[1,1] + coef2[1,2]*a2 + coef2[1,3]*x + coef2[1,4]*a4 + coef2[1,9]*a9)/(1+(exp(coef2[1,1] + coef2[1,2]*a2 + coef2[1,3]*x + coef2[1,4]*a4 + coef2[1,9]*a9)+exp(coef2[2,1] + coef2[2,2]*a2 + coef2[2,3]*x + coef2[2,4]*a4 + coef2[2,9]*a9)+exp(coef2[3,1] + coef2[3,2]*a2 + coef2[3,3]*x + coef2[3,4]*a4 + coef2[3,9]*a9)))
fb6 <- function(x) exp(coef2[2,1] + coef2[2,2]*a2 + coef2[2,3]*x + coef2[2,4]*a4 + coef2[2,9]*a9)/(1+(exp(coef2[1,1] + coef2[1,2]*a2 + coef2[1,3]*x + coef2[1,4]*a4 + coef2[1,9]*a9)+exp(coef2[2,1] + coef2[2,2]*a2 + coef2[2,3]*x + coef2[2,4]*a4 + coef2[2,9]*a9)+exp(coef2[3,1] + coef2[3,2]*a2 + coef2[3,3]*x + coef2[3,4]*a4 + coef2[3,9]*a9)))
fb7 <- function(x) exp(coef2[3,1] + coef2[3,2]*a2 + coef2[3,3]*x + coef2[3,4]*a4 + coef2[3,9]*a9)/(1+(exp(coef2[1,1] + coef2[1,2]*a2 + coef2[1,3]*x + coef2[1,4]*a4 + coef2[1,9]*a9)+exp(coef2[2,1] + coef2[2,2]*a2 + coef2[2,3]*x + coef2[2,4]*a4 + coef2[2,9]*a9)+exp(coef2[3,1] + coef2[3,2]*a2 + coef2[3,3]*x + coef2[3,4]*a4 + coef2[3,9]*a9)))

x <- seq(0, 1, length.out = 10000)
dl2 <- data.frame(x, fb4(x), fb5(x), fb6(x), fb7(x))

ggplot(dl2) +
  geom_line(aes(x, fb4.x.), linetype = "dotted", color = "gray70", size = 1) +
  geom_line(aes(x, fb5.x.), linetype = "twodash", color = "gray60", size = 1) +
  geom_line(aes(x, fb6.x.), linetype =  "longdash", color = "gray40", size = 1) +
  geom_line(aes(x, fb7.x.), linetype = "solid", color = "gray20", size = 1) +
  geom_vline(xintercept = GINIgr56) +
  #geom_vline(xintercept = GINIgr67) +
  geom_vline(xintercept = GINIgr45) +
  geom_text(x=GINIgr56-0.05, y=-0.02, label=round(GINIgr56, 2)) +
  #geom_text(x=GINIgr67+0.25, y=-0.02, label=round(GINIgr67, 2)) +
  geom_text(x=GINIgr45-0.05, y=-0.02, label=round(GINIgr45, 2)) +
  ylab("Prob. to reach a specific level of happiness") +
  xlab("GINI Index")

GINIgr56 <- (  (coef2[2,1] - coef2[1,1])    + 
                   (coef2[2,2] - coef2[1,2])*a2 + 
                   (coef2[2,3] - coef2[1,3])*a3 + 
                   (coef2[2,4] - coef2[1,4])*a4 + 
                   (coef2[2,5] - coef2[1,5])*a5 + 
                   (coef2[2,6] - coef2[1,6])*a6 + 
                   (coef2[2,7] - coef2[1,7])*a7 + 
                   (coef2[2,8] - coef2[1,8])*a8 + 
                   (coef2[2,9] - coef2[1,9])*a9    ) / (coef2[1,3] - coef2[2,3])
GINIgr67 <- (  (coef2[3,1] - coef2[2,1])    + 
                   (coef2[3,2] - coef2[2,2])*a2 + 
                   (coef2[3,3] - coef2[2,3])*a3 + 
                   (coef2[3,4] - coef2[2,4])*a4 + 
                   (coef2[3,5] - coef2[2,5])*a5 + 
                   (coef2[3,6] - coef2[2,6])*a6 + 
                   (coef2[3,7] - coef2[2,7])*a7 + 
                   (coef2[3,8] - coef2[2,8])*a8 + 
                   (coef2[3,9] - coef2[2,9])*a9    ) / (coef2[2,3] - coef2[3,3])
GINIgr45 <- (  (- coef2[1,1])    + 
                   (- coef2[1,2])*a2 + 
                   (- coef2[1,3])*a3 + 
                   (- coef2[1,4])*a4 + 
                   (- coef2[1,5])*a5 + 
                   (- coef2[1,6])*a6 + 
                   (- coef2[1,7])*a7 + 
                   (- coef2[1,8])*a8 + 
                   (- coef2[1,9])*a9    ) / (coef2[1,3])



### GDP but Africa

coef2 <- summary(test2)$coefficients

a2 <- 0
a3 <- mean(finaldata$GINI)
a4 <- mean(finaldata$CO2)
a5 <- 1
a6 <- 0
a7 <- 0
a8 <- 0
a9 <- mean(finaldata$CPW)

logGDPgr56 <- (  (coef2[2,1] - coef2[1,1])    + 
                   (coef2[2,2] - coef2[1,2])*a2 + 
                   (coef2[2,3] - coef2[1,3])*a3 + 
                   (coef2[2,4] - coef2[1,4])*a4 + 
                   (coef2[2,5] - coef2[1,5])*a5 + 
                   (coef2[2,6] - coef2[1,6])*a6 + 
                   (coef2[2,7] - coef2[1,7])*a7 + 
                   (coef2[2,8] - coef2[1,8])*a8 + 
                   (coef2[2,9] - coef2[1,9])*a9    ) / (coef2[1,2] - coef2[2,2])
logGDPgr67 <- (  (coef2[3,1] - coef2[2,1])    + 
                   (coef2[3,2] - coef2[2,2])*a2 + 
                   (coef2[3,3] - coef2[2,3])*a3 + 
                   (coef2[3,4] - coef2[2,4])*a4 + 
                   (coef2[3,5] - coef2[2,5])*a5 + 
                   (coef2[3,6] - coef2[2,6])*a6 + 
                   (coef2[3,7] - coef2[2,7])*a7 + 
                   (coef2[3,8] - coef2[2,8])*a8 + 
                   (coef2[3,9] - coef2[2,9])*a9    ) / (coef2[2,2] - coef2[3,2])
logGDPgr45 <- (  (- coef2[1,1])    + 
                   (- coef2[1,2])*a2 + 
                   (- coef2[1,3])*a3 + 
                   (- coef2[1,4])*a4 + 
                   (- coef2[1,5])*a5 + 
                   (- coef2[1,6])*a6 + 
                   (- coef2[1,7])*a7 + 
                   (- coef2[1,8])*a8 + 
                   (- coef2[1,9])*a9    ) / (coef2[1,2])


## ------------------------

finaldata <- read.xlsx('database-whr17-v9.xlsx', 1)


### Relevel
finaldata$RLadder2 <- relevel(finaldata$RLadder, ref ="4")
### Run our models
test1a <- multinom(RLadder2 ~ logGDP, data = finaldata)
test2 <- multinom(RLadder2 ~ logGDP + GINI + CO2 + CPW + WestEurope + EastEurope + Africa + MiddleEast + AsiaOceania + America, data = finaldata)

stargazer(test2, type="html", out="test2.html")
test2.rrr = exp(coef(test2))
stargazer(test2, type="text", coef=list(test2.rrr), p.auto=FALSE, out="test2rrr.txt")
