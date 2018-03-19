sink('happiness.txt')


## SETUP

load.libraries <- c('gdata', 'readxl', 'ggplot2', 'ggpubr', 'xlsx', 'MNP', 'psych', 'foreign', 'nnet', 'reshape2', 'xtable', 'margins', 'stargazer')
install.lib <- load.libraries[!load.libraries %in% installed.packages()]
for(libs in install.lib) install.packages(libs, dependencies = TRUE)
sapply(load.libraries, require, character = TRUE)

finaldata <- read.xlsx('database-whr17-v9.xlsx', 1)

### Table 3. Summary statistic
stat <- cbind(summary(finaldata$logGDP), summary(finaldata$GINI), summary(finaldata$CO2), summary(finaldata$CPW))
colnames(stat) <- c("logGDP", "GINI", "CO2", "CPW")

stargazer(stat, type="latex")
stargazer(stat, type="html", out='table3.html')

### Table 4. Coefficients for the first model
finaldata$RLadder2 <- relevel(finaldata$RLadder, ref ="4")
test1 <- multinom(RLadder2 ~ logGDP, data = finaldata)

stargazer(test1, type="latex")
stargazer(test1, type="html", out='table4.html')


### Table 5. Coefficients for the second model
test2 <- multinom(RLadder2 ~ logGDP + GINI + CO2 + CPW + WestEurope + EastEurope + Africa + MiddleEast + AsiaOceania + America, data = finaldata)

stargazer(test2, type="latex")
stargazer(test2, type="html", out='table5.html')


### Table 6. Coefficients for the third model
finaldata2 <- read_excel("database-whr17-v9.xlsx")
attach(finaldata2)
#Layard $15.000
richL <- ifelse(logGDP >= 9.615805, 1, 0)
poorL <- ifelse(logGDP < 9.615805, 1, 0)
logGDPL <- logGDP - 9.615805
richGDPL <- richL*logGDPL
poorGDPL <- poorL*logGDPL
test3a <- lm(RLadder ~ richGDPL + poorGDPL)
#Layard $20.000
richL2 <- ifelse(logGDP >= 9.903488, 1, 0)
poorL2 <- ifelse(logGDP < 9.903488, 1, 0)
logGDPL2 <- logGDP - 9.903488
richGDPL2 <- richL2*logGDPL2
poorGDPL2 <- poorL2*logGDPL2
test3b <- lm(RLadder ~ richGDPL2 + poorGDPL2)
#Frey $10.000
richF <- ifelse(logGDP >= 9.210340 , 1, 0)
poorF <- ifelse(logGDP < 9.210340 , 1, 0)
logGDPF <- logGDP - 9.210340
richGDPF <- richF*logGDPF
poorGDPF <- poorF*logGDPF
test3c <- lm(RLadder ~ richGDPF + poorGDPF)

stargazer(test3a, test3b, test3c,
          t.auto = TRUE,
          summary=TRUE,
          type="latex")


finaldata <- read.xlsx('database-whr17-v9.xlsx', 1)
attach(finaldata)
finaldata$RLadder2 <- relevel(finaldata$RLadder, ref ="4")

stargazer(test3a, test3b, test3c,
          type="html", out='table6.html')

### Figure 1. Model 1
coef <- summary(test1)$coefficients
gr56 <- (coef[2,1] - coef[1,1])/(coef[1,2] - coef[2,2])
gr67 <- (coef[3,1] - coef[2,1])/(coef[2,2] - coef[3,2])
gr45 <- - coef[1,1]/coef[1,2]
f4 <- function(x) 1/(1+(exp(coef[1,1] + coef[1,2]*x)+exp(coef[2,1] + coef[2,2]*x)+exp(coef[3,1] + coef[3,2]*x)))
f5 <- function(x) exp(coef[1,1] + coef[1,2]*x)/(1+(exp(coef[1,1] + coef[1,2]*x)+exp(coef[2,1] + coef[2,2]*x)+exp(coef[3,1] + coef[3,2]*x)))
f6 <- function(x) exp(coef[2,1] + coef[2,2]*x)/(1+(exp(coef[1,1] + coef[1,2]*x)+exp(coef[2,1] + coef[2,2]*x)+exp(coef[3,1] + coef[3,2]*x)))
f7 <- function(x) exp(coef[3,1] + coef[3,2]*x)/(1+(exp(coef[1,1] + coef[1,2]*x)+exp(coef[2,1] + coef[2,2]*x)+exp(coef[3,1] + coef[3,2]*x)))
x <- seq(6, 12, length.out = 10000)
dl <- data.frame(x, f4(x), f5(x), f6(x), f7(x))

plotmodeltest1 <- ggplot(dl) +
  geom_line(aes(x, f4.x., color = "a"), size = 1) +
  geom_line(aes(x, f5.x., color = "b"), size = 1) +
  geom_line(aes(x, f6.x., color = "c"), size = 1) +
  geom_line(aes(x, f7.x., color = "d"), size = 1) +
  geom_vline(xintercept = gr56) +
  geom_vline(xintercept = gr67) +
  geom_vline(xintercept = gr45) +
  geom_text(x=gr56-0.30, y=-0.02, label=round(gr56, 2)) +
  geom_text(x=gr67+0.35, y=-0.02, label=round(gr67, 2)) +
  geom_text(x=gr45-0.30, y=-0.02, label=round(gr45, 2)) +
  ylab("probability") +
  xlab("log of GDP per capita") +
  scale_color_manual(name = "Levels of Happiness", 
                     values = c("a" = "darkred", "b" = "purple", "c" = "darkblue", "d" = "darkgreen"),
                     labels = c("4", "5", "6", "7"))
print(plotmodeltest1)

### Figure 2. Model 2 (with/without GINI Index)
coef2 <- summary(test2)$coefficients

### NoContwG
a3 <- mean(finaldata$GINI)
a4 <- mean(finaldata$CO2)
a5 <- mean(finaldata$CPW)
a6 <- 0
a7 <- 0
a8 <- 0
a9 <- 0
a10 <- 0
a11 <- 0

fb4 <- function(x) 1 / (1 + exp(coef2[1,1] + coef2[1,2]*x + coef2[1,3]*a3 + coef2[1,4]*a4 + coef2[1,5]*a5 + coef2[1,6]*a6 + coef2[1,7]*a7 + coef2[1,8]*a8 + coef2[1,9]*a9 + coef2[1,10]*a10 + coef2[1,11]*a11) + exp(coef2[2,1] + coef2[2,2]*x + coef2[2,3]*a3 + coef2[2,4]*a4 + coef2[2,5]*a5 + coef2[2,6]*a6 + coef2[2,7]*a7 + coef2[2,8]*a8 + coef2[2,9]*a9 + coef2[2,10]*a10 + coef2[2,11]*a11) + exp(coef2[3,1] + coef2[3,2]*x + coef2[3,3]*a3 + coef2[3,4]*a4 + coef2[3,5]*a5 + coef2[3,6]*a6 + coef2[3,7]*a7 + coef2[3,8]*a8 + coef2[3,9]*a9 + coef2[3,10]*a10 + coef2[3,11]*a11))
fb5 <- function(x) exp(coef2[1,1] + coef2[1,2]*x + coef2[1,3]*a3 + coef2[1,4]*a4 + coef2[1,5]*a5 + coef2[1,6]*a6 + coef2[1,7]*a7 + coef2[1,8]*a8 + coef2[1,9]*a9 + coef2[1,10]*a10 + coef2[1,11]*a11) / (1 + (exp(coef2[1,1] + coef2[1,2]*x + coef2[1,3]*a3 + coef2[1,4]*a4 + coef2[1,5]*a5 + coef2[1,6]*a6 + coef2[1,7]*a7 + coef2[1,8]*a8 + coef2[1,9]*a9 + coef2[1,10]*a10 + coef2[1,11]*a11) + exp(coef2[2,1] + coef2[2,2]*x + coef2[2,3]*a3 + coef2[2,4]*a4 + coef2[2,5]*a5 + coef2[2,6]*a6 + coef2[2,7]*a7 + coef2[2,8]*a8 + coef2[2,9]*a9 + coef2[2,10]*a10 + coef2[2,11]*a11) + exp(coef2[3,1] + coef2[3,2]*x + coef2[3,3]*a3 + coef2[3,4]*a4 + coef2[3,5]*a5 + coef2[3,6]*a6 + coef2[3,7]*a7 + coef2[3,8]*a8 + coef2[3,9]*a9 + coef2[3,10]*a10 + coef2[3,11]*a11)))
fb6 <- function(x) exp(coef2[2,1] + coef2[2,2]*x + coef2[2,3]*a3 + coef2[2,4]*a4 + coef2[2,5]*a5 + coef2[2,6]*a6 + coef2[2,7]*a7 + coef2[2,8]*a8 + coef2[2,9]*a9 + coef2[2,10]*a10 + coef2[2,11]*a11) / (1 + (exp(coef2[1,1] + coef2[1,2]*x + coef2[1,3]*a3 + coef2[1,4]*a4 + coef2[1,5]*a5 + coef2[1,6]*a6 + coef2[1,7]*a7 + coef2[1,8]*a8 + coef2[1,9]*a9 + coef2[1,10]*a10 + coef2[1,11]*a11) + exp(coef2[2,1] + coef2[2,2]*x + coef2[2,3]*a3 + coef2[2,4]*a4 + coef2[2,5]*a5 + coef2[2,6]*a6 + coef2[2,7]*a7 + coef2[2,8]*a8 + coef2[2,9]*a9 + coef2[2,10]*a10 + coef2[2,11]*a11) + exp(coef2[3,1] + coef2[3,2]*x + coef2[3,3]*a3 + coef2[3,4]*a4 + coef2[3,5]*a5 + coef2[3,6]*a6 + coef2[3,7]*a7 + coef2[3,8]*a8 + coef2[3,9]*a9 + coef2[3,10]*a10 + coef2[3,11]*a11)))
fb7 <- function(x) exp(coef2[3,1] + coef2[3,2]*x + coef2[3,3]*a3 + coef2[3,4]*a4 + coef2[3,5]*a5 + coef2[3,6]*a6 + coef2[3,7]*a7 + coef2[3,8]*a8 + coef2[3,9]*a9 + coef2[3,10]*a10 + coef2[3,11]*a11) / (1 + (exp(coef2[1,1] + coef2[1,2]*x + coef2[1,3]*a3 + coef2[1,4]*a4 + coef2[1,5]*a5 + coef2[1,6]*a6 + coef2[1,7]*a7 + coef2[1,8]*a8 + coef2[1,9]*a9 + coef2[1,10]*a10 + coef2[1,11]*a11) + exp(coef2[2,1] + coef2[2,2]*x + coef2[2,3]*a3 + coef2[2,4]*a4 + coef2[2,5]*a5 + coef2[2,6]*a6 + coef2[2,7]*a7 + coef2[2,8]*a8 + coef2[2,9]*a9 + coef2[2,10]*a10 + coef2[2,11]*a11) + exp(coef2[3,1] + coef2[3,2]*x + coef2[3,3]*a3 + coef2[3,4]*a4 + coef2[3,5]*a5 + coef2[3,6]*a6 + coef2[3,7]*a7 + coef2[3,8]*a8 + coef2[3,9]*a9 + coef2[3,10]*a10 + coef2[3,11]*a11)))

x <- seq(0, 12, length.out = 10000)
dl2 <- data.frame(x, fb4(x), fb5(x), fb6(x), fb7(x))

NoContwG <- ggplot(dl2) +
  geom_line(aes(x, fb4.x., color = "a"), size = 1) +
  geom_line(aes(x, fb5.x., color = "b"), size = 1) +
  geom_line(aes(x, fb6.x., color = "c"), size = 1) +
  geom_line(aes(x, fb7.x., color = "d"), size = 1) +
  ylab("probability") +
  xlab("log of GDP per capita") +
  scale_color_manual(name = "Levels of Happiness", 
                     values = c("a" = "darkred", "b" = "purple", "c" = "darkblue", "d" = "darkgreen"),
                     labels = c("4", "5", "6", "7"))

### NoContnoG
a3 <- 0
a4 <- mean(finaldata$CO2)
a5 <- mean(finaldata$CPW)
a6 <- 0
a7 <- 0
a8 <- 0
a9 <- 0
a10 <- 0
a11 <- 0

fb4 <- function(x) 1 / (1 + exp(coef2[1,1] + coef2[1,2]*x + coef2[1,3]*a3 + coef2[1,4]*a4 + coef2[1,5]*a5 + coef2[1,6]*a6 + coef2[1,7]*a7 + coef2[1,8]*a8 + coef2[1,9]*a9 + coef2[1,10]*a10 + coef2[1,11]*a11) + exp(coef2[2,1] + coef2[2,2]*x + coef2[2,3]*a3 + coef2[2,4]*a4 + coef2[2,5]*a5 + coef2[2,6]*a6 + coef2[2,7]*a7 + coef2[2,8]*a8 + coef2[2,9]*a9 + coef2[2,10]*a10 + coef2[2,11]*a11) + exp(coef2[3,1] + coef2[3,2]*x + coef2[3,3]*a3 + coef2[3,4]*a4 + coef2[3,5]*a5 + coef2[3,6]*a6 + coef2[3,7]*a7 + coef2[3,8]*a8 + coef2[3,9]*a9 + coef2[3,10]*a10 + coef2[3,11]*a11))
fb5 <- function(x) exp(coef2[1,1] + coef2[1,2]*x + coef2[1,3]*a3 + coef2[1,4]*a4 + coef2[1,5]*a5 + coef2[1,6]*a6 + coef2[1,7]*a7 + coef2[1,8]*a8 + coef2[1,9]*a9 + coef2[1,10]*a10 + coef2[1,11]*a11) / (1 + (exp(coef2[1,1] + coef2[1,2]*x + coef2[1,3]*a3 + coef2[1,4]*a4 + coef2[1,5]*a5 + coef2[1,6]*a6 + coef2[1,7]*a7 + coef2[1,8]*a8 + coef2[1,9]*a9 + coef2[1,10]*a10 + coef2[1,11]*a11) + exp(coef2[2,1] + coef2[2,2]*x + coef2[2,3]*a3 + coef2[2,4]*a4 + coef2[2,5]*a5 + coef2[2,6]*a6 + coef2[2,7]*a7 + coef2[2,8]*a8 + coef2[2,9]*a9 + coef2[2,10]*a10 + coef2[2,11]*a11) + exp(coef2[3,1] + coef2[3,2]*x + coef2[3,3]*a3 + coef2[3,4]*a4 + coef2[3,5]*a5 + coef2[3,6]*a6 + coef2[3,7]*a7 + coef2[3,8]*a8 + coef2[3,9]*a9 + coef2[3,10]*a10 + coef2[3,11]*a11)))
fb6 <- function(x) exp(coef2[2,1] + coef2[2,2]*x + coef2[2,3]*a3 + coef2[2,4]*a4 + coef2[2,5]*a5 + coef2[2,6]*a6 + coef2[2,7]*a7 + coef2[2,8]*a8 + coef2[2,9]*a9 + coef2[2,10]*a10 + coef2[2,11]*a11) / (1 + (exp(coef2[1,1] + coef2[1,2]*x + coef2[1,3]*a3 + coef2[1,4]*a4 + coef2[1,5]*a5 + coef2[1,6]*a6 + coef2[1,7]*a7 + coef2[1,8]*a8 + coef2[1,9]*a9 + coef2[1,10]*a10 + coef2[1,11]*a11) + exp(coef2[2,1] + coef2[2,2]*x + coef2[2,3]*a3 + coef2[2,4]*a4 + coef2[2,5]*a5 + coef2[2,6]*a6 + coef2[2,7]*a7 + coef2[2,8]*a8 + coef2[2,9]*a9 + coef2[2,10]*a10 + coef2[2,11]*a11) + exp(coef2[3,1] + coef2[3,2]*x + coef2[3,3]*a3 + coef2[3,4]*a4 + coef2[3,5]*a5 + coef2[3,6]*a6 + coef2[3,7]*a7 + coef2[3,8]*a8 + coef2[3,9]*a9 + coef2[3,10]*a10 + coef2[3,11]*a11)))
fb7 <- function(x) exp(coef2[3,1] + coef2[3,2]*x + coef2[3,3]*a3 + coef2[3,4]*a4 + coef2[3,5]*a5 + coef2[3,6]*a6 + coef2[3,7]*a7 + coef2[3,8]*a8 + coef2[3,9]*a9 + coef2[3,10]*a10 + coef2[3,11]*a11) / (1 + (exp(coef2[1,1] + coef2[1,2]*x + coef2[1,3]*a3 + coef2[1,4]*a4 + coef2[1,5]*a5 + coef2[1,6]*a6 + coef2[1,7]*a7 + coef2[1,8]*a8 + coef2[1,9]*a9 + coef2[1,10]*a10 + coef2[1,11]*a11) + exp(coef2[2,1] + coef2[2,2]*x + coef2[2,3]*a3 + coef2[2,4]*a4 + coef2[2,5]*a5 + coef2[2,6]*a6 + coef2[2,7]*a7 + coef2[2,8]*a8 + coef2[2,9]*a9 + coef2[2,10]*a10 + coef2[2,11]*a11) + exp(coef2[3,1] + coef2[3,2]*x + coef2[3,3]*a3 + coef2[3,4]*a4 + coef2[3,5]*a5 + coef2[3,6]*a6 + coef2[3,7]*a7 + coef2[3,8]*a8 + coef2[3,9]*a9 + coef2[3,10]*a10 + coef2[3,11]*a11)))

x <- seq(0, 12, length.out = 10000)
dl2 <- data.frame(x, fb4(x), fb5(x), fb6(x), fb7(x))

NoContnoG <- ggplot(dl2) +
  geom_line(aes(x, fb4.x., color = "a"), size = 1) +
  geom_line(aes(x, fb5.x., color = "b"), size = 1) +
  geom_line(aes(x, fb6.x., color = "c"), size = 1) +
  geom_line(aes(x, fb7.x., color = "d"), size = 1) +
  ylab("probability") +
  xlab("log of GDP per capita") +
  scale_color_manual(name = "Levels of Happiness", 
                     values = c("a" = "darkred", "b" = "purple", "c" = "darkblue", "d" = "darkgreen"),
                     labels = c("4", "5", "6", "7"))

GINIcomp <- ggarrange(NoContwG, NoContnoG,
                      labels = c("with GINI", "without GINI"),
                      common.legend = TRUE, legend = "bottom",
                      ncol = 1, nrow = 2)

print(GINIcomp)

### Figrue 3. Model 2 (group by continent)
coef2 <- summary(test2)$coefficients

a2 <- 0
a3 <- 0
a4 <- 0
a5 <- 0
a6 <- 0
a7 <- 0
a8 <- 0
a9 <- 0
a10 <- 0
a11 <- 0

logGDPgr56 <- (  (coef2[2,1] - coef2[1,1])    + 
                   (coef2[2,2] - coef2[1,2])*a2 + 
                   (coef2[2,3] - coef2[1,3])*a3 + 
                   (coef2[2,4] - coef2[1,4])*a4 + 
                   (coef2[2,5] - coef2[1,5])*a5 + 
                   (coef2[2,6] - coef2[1,6])*a6 + 
                   (coef2[2,7] - coef2[1,7])*a7 + 
                   (coef2[2,8] - coef2[1,8])*a8 + 
                   (coef2[2,9] - coef2[1,9])*a9 +
                   (coef2[2,10] - coef2[1,10])*a10 +
                   (coef2[2,11] - coef2[1,11])*a11 ) / (coef2[1,2] - coef2[2,2])
logGDPgr67 <- (  (coef2[3,1] - coef2[2,1])    + 
                   (coef2[3,2] - coef2[2,2])*a2 + 
                   (coef2[3,3] - coef2[2,3])*a3 + 
                   (coef2[3,4] - coef2[2,4])*a4 + 
                   (coef2[3,5] - coef2[2,5])*a5 + 
                   (coef2[3,6] - coef2[2,6])*a6 + 
                   (coef2[3,7] - coef2[2,7])*a7 + 
                   (coef2[3,8] - coef2[2,8])*a8 + 
                   (coef2[3,9] - coef2[2,9])*a9 +
                   (coef2[3,10] - coef2[2,10])*a10 +
                   (coef2[3,11] - coef2[2,11])*a11   ) / (coef2[2,2] - coef2[3,2])
logGDPgr45 <- (  (- coef2[1,1])    + 
                   (- coef2[1,2])*a2 + 
                   (- coef2[1,3])*a3 + 
                   (- coef2[1,4])*a4 + 
                   (- coef2[1,5])*a5 + 
                   (- coef2[1,6])*a6 + 
                   (- coef2[1,7])*a7 + 
                   (- coef2[1,8])*a8 + 
                   (- coef2[1,9])*a9 +
                   (- coef2[1,10])*a10 +
                   (- coef2[1,11])*a11   ) / (coef2[1,2])


### WestEurope
a3 <- mean(finaldata$GINI)#[finaldata$WestEurope==1])
a4 <- mean(finaldata$CO2)#[finaldata$WestEurope==1])
a5 <- mean(finaldata$CPW)#[finaldata$WestEurope==1])
a6 <- 1
a7 <- 0
a8 <- 0
a9 <- 0
a10 <- 0
a11 <- 0

fb4 <- function(x) 1 / (1 + exp(coef2[1,1] + coef2[1,2]*x + coef2[1,3]*a3 + coef2[1,4]*a4 + coef2[1,5]*a5 + coef2[1,6]*a6 + coef2[1,7]*a7 + coef2[1,8]*a8 + coef2[1,9]*a9 + coef2[1,10]*a10 + coef2[1,11]*a11) + exp(coef2[2,1] + coef2[2,2]*x + coef2[2,3]*a3 + coef2[2,4]*a4 + coef2[2,5]*a5 + coef2[2,6]*a6 + coef2[2,7]*a7 + coef2[2,8]*a8 + coef2[2,9]*a9 + coef2[2,10]*a10 + coef2[2,11]*a11) + exp(coef2[3,1] + coef2[3,2]*x + coef2[3,3]*a3 + coef2[3,4]*a4 + coef2[3,5]*a5 + coef2[3,6]*a6 + coef2[3,7]*a7 + coef2[3,8]*a8 + coef2[3,9]*a9 + coef2[3,10]*a10 + coef2[3,11]*a11))
fb5 <- function(x) exp(coef2[1,1] + coef2[1,2]*x + coef2[1,3]*a3 + coef2[1,4]*a4 + coef2[1,5]*a5 + coef2[1,6]*a6 + coef2[1,7]*a7 + coef2[1,8]*a8 + coef2[1,9]*a9 + coef2[1,10]*a10 + coef2[1,11]*a11) / (1 + (exp(coef2[1,1] + coef2[1,2]*x + coef2[1,3]*a3 + coef2[1,4]*a4 + coef2[1,5]*a5 + coef2[1,6]*a6 + coef2[1,7]*a7 + coef2[1,8]*a8 + coef2[1,9]*a9 + coef2[1,10]*a10 + coef2[1,11]*a11) + exp(coef2[2,1] + coef2[2,2]*x + coef2[2,3]*a3 + coef2[2,4]*a4 + coef2[2,5]*a5 + coef2[2,6]*a6 + coef2[2,7]*a7 + coef2[2,8]*a8 + coef2[2,9]*a9 + coef2[2,10]*a10 + coef2[2,11]*a11) + exp(coef2[3,1] + coef2[3,2]*x + coef2[3,3]*a3 + coef2[3,4]*a4 + coef2[3,5]*a5 + coef2[3,6]*a6 + coef2[3,7]*a7 + coef2[3,8]*a8 + coef2[3,9]*a9 + coef2[3,10]*a10 + coef2[3,11]*a11)))
fb6 <- function(x) exp(coef2[2,1] + coef2[2,2]*x + coef2[2,3]*a3 + coef2[2,4]*a4 + coef2[2,5]*a5 + coef2[2,6]*a6 + coef2[2,7]*a7 + coef2[2,8]*a8 + coef2[2,9]*a9 + coef2[2,10]*a10 + coef2[2,11]*a11) / (1 + (exp(coef2[1,1] + coef2[1,2]*x + coef2[1,3]*a3 + coef2[1,4]*a4 + coef2[1,5]*a5 + coef2[1,6]*a6 + coef2[1,7]*a7 + coef2[1,8]*a8 + coef2[1,9]*a9 + coef2[1,10]*a10 + coef2[1,11]*a11) + exp(coef2[2,1] + coef2[2,2]*x + coef2[2,3]*a3 + coef2[2,4]*a4 + coef2[2,5]*a5 + coef2[2,6]*a6 + coef2[2,7]*a7 + coef2[2,8]*a8 + coef2[2,9]*a9 + coef2[2,10]*a10 + coef2[2,11]*a11) + exp(coef2[3,1] + coef2[3,2]*x + coef2[3,3]*a3 + coef2[3,4]*a4 + coef2[3,5]*a5 + coef2[3,6]*a6 + coef2[3,7]*a7 + coef2[3,8]*a8 + coef2[3,9]*a9 + coef2[3,10]*a10 + coef2[3,11]*a11)))
fb7 <- function(x) exp(coef2[3,1] + coef2[3,2]*x + coef2[3,3]*a3 + coef2[3,4]*a4 + coef2[3,5]*a5 + coef2[3,6]*a6 + coef2[3,7]*a7 + coef2[3,8]*a8 + coef2[3,9]*a9 + coef2[3,10]*a10 + coef2[3,11]*a11) / (1 + (exp(coef2[1,1] + coef2[1,2]*x + coef2[1,3]*a3 + coef2[1,4]*a4 + coef2[1,5]*a5 + coef2[1,6]*a6 + coef2[1,7]*a7 + coef2[1,8]*a8 + coef2[1,9]*a9 + coef2[1,10]*a10 + coef2[1,11]*a11) + exp(coef2[2,1] + coef2[2,2]*x + coef2[2,3]*a3 + coef2[2,4]*a4 + coef2[2,5]*a5 + coef2[2,6]*a6 + coef2[2,7]*a7 + coef2[2,8]*a8 + coef2[2,9]*a9 + coef2[2,10]*a10 + coef2[2,11]*a11) + exp(coef2[3,1] + coef2[3,2]*x + coef2[3,3]*a3 + coef2[3,4]*a4 + coef2[3,5]*a5 + coef2[3,6]*a6 + coef2[3,7]*a7 + coef2[3,8]*a8 + coef2[3,9]*a9 + coef2[3,10]*a10 + coef2[3,11]*a11)))

x <- seq(6, 12, length.out = 10000)
dl2 <- data.frame(x, fb4(x), fb5(x), fb6(x), fb7(x))

WestEurope <- ggplot(dl2) +
  geom_line(aes(x, fb4.x., color = "a"), size = 1) +
  geom_line(aes(x, fb5.x., color = "b"), size = 1) +
  geom_line(aes(x, fb6.x., color = "c"), size = 1) +
  geom_line(aes(x, fb7.x., color = "d"), size = 1) +
  geom_hline(yintercept = 0.9) +
  geom_vline(xintercept = 10.59, linetype = "longdash") +
  geom_text(x=10.59+0.6, y=0.30, label="10.59") +
  ylab("prob.") +
  xlab("log of GDP per capita") +
  scale_color_manual(name = "Levels of Happiness", 
                     values = c("a" = "darkred", "b" = "purple", "c" = "darkblue", "d" = "darkgreen"),
                     labels = c("4", "5", "6", "7"))

### EastEurope
a3 <- mean(finaldata$GINI)#[finaldata$EastEurope==1])
a4 <- mean(finaldata$CO2)#[finaldata$EastEurope==1])
a5 <- mean(finaldata$CPW)#[finaldata$EastEurope==1])
a6 <- 0
a7 <- 1
a8 <- 0
a9 <- 0
a10 <- 0
a11 <- 0

fb4 <- function(x) 1 / (1 + exp(coef2[1,1] + coef2[1,2]*x + coef2[1,3]*a3 + coef2[1,4]*a4 + coef2[1,5]*a5 + coef2[1,6]*a6 + coef2[1,7]*a7 + coef2[1,8]*a8 + coef2[1,9]*a9 + coef2[1,10]*a10 + coef2[1,11]*a11) + exp(coef2[2,1] + coef2[2,2]*x + coef2[2,3]*a3 + coef2[2,4]*a4 + coef2[2,5]*a5 + coef2[2,6]*a6 + coef2[2,7]*a7 + coef2[2,8]*a8 + coef2[2,9]*a9 + coef2[2,10]*a10 + coef2[2,11]*a11) + exp(coef2[3,1] + coef2[3,2]*x + coef2[3,3]*a3 + coef2[3,4]*a4 + coef2[3,5]*a5 + coef2[3,6]*a6 + coef2[3,7]*a7 + coef2[3,8]*a8 + coef2[3,9]*a9 + coef2[3,10]*a10 + coef2[3,11]*a11))
fb5 <- function(x) exp(coef2[1,1] + coef2[1,2]*x + coef2[1,3]*a3 + coef2[1,4]*a4 + coef2[1,5]*a5 + coef2[1,6]*a6 + coef2[1,7]*a7 + coef2[1,8]*a8 + coef2[1,9]*a9 + coef2[1,10]*a10 + coef2[1,11]*a11) / (1 + (exp(coef2[1,1] + coef2[1,2]*x + coef2[1,3]*a3 + coef2[1,4]*a4 + coef2[1,5]*a5 + coef2[1,6]*a6 + coef2[1,7]*a7 + coef2[1,8]*a8 + coef2[1,9]*a9 + coef2[1,10]*a10 + coef2[1,11]*a11) + exp(coef2[2,1] + coef2[2,2]*x + coef2[2,3]*a3 + coef2[2,4]*a4 + coef2[2,5]*a5 + coef2[2,6]*a6 + coef2[2,7]*a7 + coef2[2,8]*a8 + coef2[2,9]*a9 + coef2[2,10]*a10 + coef2[2,11]*a11) + exp(coef2[3,1] + coef2[3,2]*x + coef2[3,3]*a3 + coef2[3,4]*a4 + coef2[3,5]*a5 + coef2[3,6]*a6 + coef2[3,7]*a7 + coef2[3,8]*a8 + coef2[3,9]*a9 + coef2[3,10]*a10 + coef2[3,11]*a11)))
fb6 <- function(x) exp(coef2[2,1] + coef2[2,2]*x + coef2[2,3]*a3 + coef2[2,4]*a4 + coef2[2,5]*a5 + coef2[2,6]*a6 + coef2[2,7]*a7 + coef2[2,8]*a8 + coef2[2,9]*a9 + coef2[2,10]*a10 + coef2[2,11]*a11) / (1 + (exp(coef2[1,1] + coef2[1,2]*x + coef2[1,3]*a3 + coef2[1,4]*a4 + coef2[1,5]*a5 + coef2[1,6]*a6 + coef2[1,7]*a7 + coef2[1,8]*a8 + coef2[1,9]*a9 + coef2[1,10]*a10 + coef2[1,11]*a11) + exp(coef2[2,1] + coef2[2,2]*x + coef2[2,3]*a3 + coef2[2,4]*a4 + coef2[2,5]*a5 + coef2[2,6]*a6 + coef2[2,7]*a7 + coef2[2,8]*a8 + coef2[2,9]*a9 + coef2[2,10]*a10 + coef2[2,11]*a11) + exp(coef2[3,1] + coef2[3,2]*x + coef2[3,3]*a3 + coef2[3,4]*a4 + coef2[3,5]*a5 + coef2[3,6]*a6 + coef2[3,7]*a7 + coef2[3,8]*a8 + coef2[3,9]*a9 + coef2[3,10]*a10 + coef2[3,11]*a11)))
fb7 <- function(x) exp(coef2[3,1] + coef2[3,2]*x + coef2[3,3]*a3 + coef2[3,4]*a4 + coef2[3,5]*a5 + coef2[3,6]*a6 + coef2[3,7]*a7 + coef2[3,8]*a8 + coef2[3,9]*a9 + coef2[3,10]*a10 + coef2[3,11]*a11) / (1 + (exp(coef2[1,1] + coef2[1,2]*x + coef2[1,3]*a3 + coef2[1,4]*a4 + coef2[1,5]*a5 + coef2[1,6]*a6 + coef2[1,7]*a7 + coef2[1,8]*a8 + coef2[1,9]*a9 + coef2[1,10]*a10 + coef2[1,11]*a11) + exp(coef2[2,1] + coef2[2,2]*x + coef2[2,3]*a3 + coef2[2,4]*a4 + coef2[2,5]*a5 + coef2[2,6]*a6 + coef2[2,7]*a7 + coef2[2,8]*a8 + coef2[2,9]*a9 + coef2[2,10]*a10 + coef2[2,11]*a11) + exp(coef2[3,1] + coef2[3,2]*x + coef2[3,3]*a3 + coef2[3,4]*a4 + coef2[3,5]*a5 + coef2[3,6]*a6 + coef2[3,7]*a7 + coef2[3,8]*a8 + coef2[3,9]*a9 + coef2[3,10]*a10 + coef2[3,11]*a11)))

x <- seq(6, 12, length.out = 10000)
dl2 <- data.frame(x, fb4(x), fb5(x), fb6(x), fb7(x))

EastEurope <- ggplot(dl2) +
  geom_line(aes(x, fb4.x., color = "a"), size = 1) +
  geom_line(aes(x, fb5.x., color = "b"), size = 1) +
  geom_line(aes(x, fb6.x., color = "c"), size = 1) +
  geom_line(aes(x, fb7.x., color = "d"), size = 1) +
  geom_hline(yintercept = 0.9) +
  geom_vline(xintercept = 10.77, linetype = "longdash") +
  geom_text(x=10.77+0.6, y=0.30, label="10.77") +
  ylab("prob.") +
  xlab("log of GDP per capita") +
  scale_color_manual(name = "Levels of Happiness", 
                     values = c("a" = "darkred", "b" = "purple", "c" = "darkblue", "d" = "darkgreen"),
                     labels = c("4", "5", "6", "7"))

### Africa
a3 <- mean(finaldata$GINI)#[finaldata$Africa==1])
a4 <- mean(finaldata$CO2)#[finaldata$Africa==1])
a5 <- mean(finaldata$CPW)#[finaldata$Africa==1])
a6 <- 0
a7 <- 0
a8 <- 1
a9 <- 0
a10 <- 0
a11 <- 0

fb4 <- function(x) 1 / (1 + exp(coef2[1,1] + coef2[1,2]*x + coef2[1,3]*a3 + coef2[1,4]*a4 + coef2[1,5]*a5 + coef2[1,6]*a6 + coef2[1,7]*a7 + coef2[1,8]*a8 + coef2[1,9]*a9 + coef2[1,10]*a10 + coef2[1,11]*a11) + exp(coef2[2,1] + coef2[2,2]*x + coef2[2,3]*a3 + coef2[2,4]*a4 + coef2[2,5]*a5 + coef2[2,6]*a6 + coef2[2,7]*a7 + coef2[2,8]*a8 + coef2[2,9]*a9 + coef2[2,10]*a10 + coef2[2,11]*a11) + exp(coef2[3,1] + coef2[3,2]*x + coef2[3,3]*a3 + coef2[3,4]*a4 + coef2[3,5]*a5 + coef2[3,6]*a6 + coef2[3,7]*a7 + coef2[3,8]*a8 + coef2[3,9]*a9 + coef2[3,10]*a10 + coef2[3,11]*a11))
fb5 <- function(x) exp(coef2[1,1] + coef2[1,2]*x + coef2[1,3]*a3 + coef2[1,4]*a4 + coef2[1,5]*a5 + coef2[1,6]*a6 + coef2[1,7]*a7 + coef2[1,8]*a8 + coef2[1,9]*a9 + coef2[1,10]*a10 + coef2[1,11]*a11) / (1 + (exp(coef2[1,1] + coef2[1,2]*x + coef2[1,3]*a3 + coef2[1,4]*a4 + coef2[1,5]*a5 + coef2[1,6]*a6 + coef2[1,7]*a7 + coef2[1,8]*a8 + coef2[1,9]*a9 + coef2[1,10]*a10 + coef2[1,11]*a11) + exp(coef2[2,1] + coef2[2,2]*x + coef2[2,3]*a3 + coef2[2,4]*a4 + coef2[2,5]*a5 + coef2[2,6]*a6 + coef2[2,7]*a7 + coef2[2,8]*a8 + coef2[2,9]*a9 + coef2[2,10]*a10 + coef2[2,11]*a11) + exp(coef2[3,1] + coef2[3,2]*x + coef2[3,3]*a3 + coef2[3,4]*a4 + coef2[3,5]*a5 + coef2[3,6]*a6 + coef2[3,7]*a7 + coef2[3,8]*a8 + coef2[3,9]*a9 + coef2[3,10]*a10 + coef2[3,11]*a11)))
fb6 <- function(x) exp(coef2[2,1] + coef2[2,2]*x + coef2[2,3]*a3 + coef2[2,4]*a4 + coef2[2,5]*a5 + coef2[2,6]*a6 + coef2[2,7]*a7 + coef2[2,8]*a8 + coef2[2,9]*a9 + coef2[2,10]*a10 + coef2[2,11]*a11) / (1 + (exp(coef2[1,1] + coef2[1,2]*x + coef2[1,3]*a3 + coef2[1,4]*a4 + coef2[1,5]*a5 + coef2[1,6]*a6 + coef2[1,7]*a7 + coef2[1,8]*a8 + coef2[1,9]*a9 + coef2[1,10]*a10 + coef2[1,11]*a11) + exp(coef2[2,1] + coef2[2,2]*x + coef2[2,3]*a3 + coef2[2,4]*a4 + coef2[2,5]*a5 + coef2[2,6]*a6 + coef2[2,7]*a7 + coef2[2,8]*a8 + coef2[2,9]*a9 + coef2[2,10]*a10 + coef2[2,11]*a11) + exp(coef2[3,1] + coef2[3,2]*x + coef2[3,3]*a3 + coef2[3,4]*a4 + coef2[3,5]*a5 + coef2[3,6]*a6 + coef2[3,7]*a7 + coef2[3,8]*a8 + coef2[3,9]*a9 + coef2[3,10]*a10 + coef2[3,11]*a11)))
fb7 <- function(x) exp(coef2[3,1] + coef2[3,2]*x + coef2[3,3]*a3 + coef2[3,4]*a4 + coef2[3,5]*a5 + coef2[3,6]*a6 + coef2[3,7]*a7 + coef2[3,8]*a8 + coef2[3,9]*a9 + coef2[3,10]*a10 + coef2[3,11]*a11) / (1 + (exp(coef2[1,1] + coef2[1,2]*x + coef2[1,3]*a3 + coef2[1,4]*a4 + coef2[1,5]*a5 + coef2[1,6]*a6 + coef2[1,7]*a7 + coef2[1,8]*a8 + coef2[1,9]*a9 + coef2[1,10]*a10 + coef2[1,11]*a11) + exp(coef2[2,1] + coef2[2,2]*x + coef2[2,3]*a3 + coef2[2,4]*a4 + coef2[2,5]*a5 + coef2[2,6]*a6 + coef2[2,7]*a7 + coef2[2,8]*a8 + coef2[2,9]*a9 + coef2[2,10]*a10 + coef2[2,11]*a11) + exp(coef2[3,1] + coef2[3,2]*x + coef2[3,3]*a3 + coef2[3,4]*a4 + coef2[3,5]*a5 + coef2[3,6]*a6 + coef2[3,7]*a7 + coef2[3,8]*a8 + coef2[3,9]*a9 + coef2[3,10]*a10 + coef2[3,11]*a11)))

x <- seq(6, 12, length.out = 10000)
dl2 <- data.frame(x, fb4(x), fb5(x), fb6(x), fb7(x))

Africa <- ggplot(dl2) +
  geom_line(aes(x, fb4.x., color = "a"), size = 1) +
  geom_line(aes(x, fb5.x., color = "b"), size = 1) +
  geom_line(aes(x, fb6.x., color = "c"), size = 1) +
  geom_line(aes(x, fb7.x., color = "d"), size = 1) +
  geom_hline(yintercept = 0.9) +
  geom_vline(xintercept = 10.96, linetype = "longdash") +
  geom_text(x=10.96+0.6, y=0.30, label="10.96") +
  ylab("prob.") +
  xlab("log of GDP per capita") +
  scale_color_manual(name = "Levels of Happiness", 
                     values = c("a" = "darkred", "b" = "purple", "c" = "darkblue", "d" = "darkgreen"),
                     labels = c("4", "5", "6", "7"))

### MiddleEast
a3 <- mean(finaldata$GINI)#[finaldata$MiddleEast==1])
a4 <- mean(finaldata$CO2)#[finaldata$MiddleEast==1])
a5 <- mean(finaldata$CPW)#[finaldata$MiddleEast==1])
a6 <- 0
a7 <- 0
a8 <- 0
a9 <- 1
a10 <- 0
a11 <- 0

fb4 <- function(x) 1 / (1 + exp(coef2[1,1] + coef2[1,2]*x + coef2[1,3]*a3 + coef2[1,4]*a4 + coef2[1,5]*a5 + coef2[1,6]*a6 + coef2[1,7]*a7 + coef2[1,8]*a8 + coef2[1,9]*a9 + coef2[1,10]*a10 + coef2[1,11]*a11) + exp(coef2[2,1] + coef2[2,2]*x + coef2[2,3]*a3 + coef2[2,4]*a4 + coef2[2,5]*a5 + coef2[2,6]*a6 + coef2[2,7]*a7 + coef2[2,8]*a8 + coef2[2,9]*a9 + coef2[2,10]*a10 + coef2[2,11]*a11) + exp(coef2[3,1] + coef2[3,2]*x + coef2[3,3]*a3 + coef2[3,4]*a4 + coef2[3,5]*a5 + coef2[3,6]*a6 + coef2[3,7]*a7 + coef2[3,8]*a8 + coef2[3,9]*a9 + coef2[3,10]*a10 + coef2[3,11]*a11))
fb5 <- function(x) exp(coef2[1,1] + coef2[1,2]*x + coef2[1,3]*a3 + coef2[1,4]*a4 + coef2[1,5]*a5 + coef2[1,6]*a6 + coef2[1,7]*a7 + coef2[1,8]*a8 + coef2[1,9]*a9 + coef2[1,10]*a10 + coef2[1,11]*a11) / (1 + (exp(coef2[1,1] + coef2[1,2]*x + coef2[1,3]*a3 + coef2[1,4]*a4 + coef2[1,5]*a5 + coef2[1,6]*a6 + coef2[1,7]*a7 + coef2[1,8]*a8 + coef2[1,9]*a9 + coef2[1,10]*a10 + coef2[1,11]*a11) + exp(coef2[2,1] + coef2[2,2]*x + coef2[2,3]*a3 + coef2[2,4]*a4 + coef2[2,5]*a5 + coef2[2,6]*a6 + coef2[2,7]*a7 + coef2[2,8]*a8 + coef2[2,9]*a9 + coef2[2,10]*a10 + coef2[2,11]*a11) + exp(coef2[3,1] + coef2[3,2]*x + coef2[3,3]*a3 + coef2[3,4]*a4 + coef2[3,5]*a5 + coef2[3,6]*a6 + coef2[3,7]*a7 + coef2[3,8]*a8 + coef2[3,9]*a9 + coef2[3,10]*a10 + coef2[3,11]*a11)))
fb6 <- function(x) exp(coef2[2,1] + coef2[2,2]*x + coef2[2,3]*a3 + coef2[2,4]*a4 + coef2[2,5]*a5 + coef2[2,6]*a6 + coef2[2,7]*a7 + coef2[2,8]*a8 + coef2[2,9]*a9 + coef2[2,10]*a10 + coef2[2,11]*a11) / (1 + (exp(coef2[1,1] + coef2[1,2]*x + coef2[1,3]*a3 + coef2[1,4]*a4 + coef2[1,5]*a5 + coef2[1,6]*a6 + coef2[1,7]*a7 + coef2[1,8]*a8 + coef2[1,9]*a9 + coef2[1,10]*a10 + coef2[1,11]*a11) + exp(coef2[2,1] + coef2[2,2]*x + coef2[2,3]*a3 + coef2[2,4]*a4 + coef2[2,5]*a5 + coef2[2,6]*a6 + coef2[2,7]*a7 + coef2[2,8]*a8 + coef2[2,9]*a9 + coef2[2,10]*a10 + coef2[2,11]*a11) + exp(coef2[3,1] + coef2[3,2]*x + coef2[3,3]*a3 + coef2[3,4]*a4 + coef2[3,5]*a5 + coef2[3,6]*a6 + coef2[3,7]*a7 + coef2[3,8]*a8 + coef2[3,9]*a9 + coef2[3,10]*a10 + coef2[3,11]*a11)))
fb7 <- function(x) exp(coef2[3,1] + coef2[3,2]*x + coef2[3,3]*a3 + coef2[3,4]*a4 + coef2[3,5]*a5 + coef2[3,6]*a6 + coef2[3,7]*a7 + coef2[3,8]*a8 + coef2[3,9]*a9 + coef2[3,10]*a10 + coef2[3,11]*a11) / (1 + (exp(coef2[1,1] + coef2[1,2]*x + coef2[1,3]*a3 + coef2[1,4]*a4 + coef2[1,5]*a5 + coef2[1,6]*a6 + coef2[1,7]*a7 + coef2[1,8]*a8 + coef2[1,9]*a9 + coef2[1,10]*a10 + coef2[1,11]*a11) + exp(coef2[2,1] + coef2[2,2]*x + coef2[2,3]*a3 + coef2[2,4]*a4 + coef2[2,5]*a5 + coef2[2,6]*a6 + coef2[2,7]*a7 + coef2[2,8]*a8 + coef2[2,9]*a9 + coef2[2,10]*a10 + coef2[2,11]*a11) + exp(coef2[3,1] + coef2[3,2]*x + coef2[3,3]*a3 + coef2[3,4]*a4 + coef2[3,5]*a5 + coef2[3,6]*a6 + coef2[3,7]*a7 + coef2[3,8]*a8 + coef2[3,9]*a9 + coef2[3,10]*a10 + coef2[3,11]*a11)))

x <- seq(6, 12, length.out = 10000)
dl2 <- data.frame(x, fb4(x), fb5(x), fb6(x), fb7(x))

MiddleEast <- ggplot(dl2) +
  geom_line(aes(x, fb4.x., color = "a"), size = 1) +
  geom_line(aes(x, fb5.x., color = "b"), size = 1) +
  geom_line(aes(x, fb6.x., color = "c"), size = 1) +
  geom_line(aes(x, fb7.x., color = "d"), size = 1) +
  geom_hline(yintercept = 0.9) +
  geom_vline(xintercept = 10.40, linetype = "longdash") +
  geom_text(x=10.40+0.6, y=0.30, label="10.40") +
  ylab("prob.") +
  xlab("log of GDP per capita") +
  scale_color_manual(name = "Levels of Happiness", 
                     values = c("a" = "darkred", "b" = "purple", "c" = "darkblue", "d" = "darkgreen"),
                     labels = c("4", "5", "6", "7"))

### AsiaOceania
a3 <- mean(finaldata$GINI)#[finaldata$AsiaOceania==1])
a4 <- mean(finaldata$CO2)#[finaldata$AsiaOceania==1])
a5 <- mean(finaldata$CPW)#[finaldata$AsiaOceania==1])
a6 <- 0
a7 <- 0
a8 <- 0
a9 <- 0
a10 <- 1
a11 <- 0

fb4 <- function(x) 1 / (1 + exp(coef2[1,1] + coef2[1,2]*x + coef2[1,3]*a3 + coef2[1,4]*a4 + coef2[1,5]*a5 + coef2[1,6]*a6 + coef2[1,7]*a7 + coef2[1,8]*a8 + coef2[1,9]*a9 + coef2[1,10]*a10 + coef2[1,11]*a11) + exp(coef2[2,1] + coef2[2,2]*x + coef2[2,3]*a3 + coef2[2,4]*a4 + coef2[2,5]*a5 + coef2[2,6]*a6 + coef2[2,7]*a7 + coef2[2,8]*a8 + coef2[2,9]*a9 + coef2[2,10]*a10 + coef2[2,11]*a11) + exp(coef2[3,1] + coef2[3,2]*x + coef2[3,3]*a3 + coef2[3,4]*a4 + coef2[3,5]*a5 + coef2[3,6]*a6 + coef2[3,7]*a7 + coef2[3,8]*a8 + coef2[3,9]*a9 + coef2[3,10]*a10 + coef2[3,11]*a11))
fb5 <- function(x) exp(coef2[1,1] + coef2[1,2]*x + coef2[1,3]*a3 + coef2[1,4]*a4 + coef2[1,5]*a5 + coef2[1,6]*a6 + coef2[1,7]*a7 + coef2[1,8]*a8 + coef2[1,9]*a9 + coef2[1,10]*a10 + coef2[1,11]*a11) / (1 + (exp(coef2[1,1] + coef2[1,2]*x + coef2[1,3]*a3 + coef2[1,4]*a4 + coef2[1,5]*a5 + coef2[1,6]*a6 + coef2[1,7]*a7 + coef2[1,8]*a8 + coef2[1,9]*a9 + coef2[1,10]*a10 + coef2[1,11]*a11) + exp(coef2[2,1] + coef2[2,2]*x + coef2[2,3]*a3 + coef2[2,4]*a4 + coef2[2,5]*a5 + coef2[2,6]*a6 + coef2[2,7]*a7 + coef2[2,8]*a8 + coef2[2,9]*a9 + coef2[2,10]*a10 + coef2[2,11]*a11) + exp(coef2[3,1] + coef2[3,2]*x + coef2[3,3]*a3 + coef2[3,4]*a4 + coef2[3,5]*a5 + coef2[3,6]*a6 + coef2[3,7]*a7 + coef2[3,8]*a8 + coef2[3,9]*a9 + coef2[3,10]*a10 + coef2[3,11]*a11)))
fb6 <- function(x) exp(coef2[2,1] + coef2[2,2]*x + coef2[2,3]*a3 + coef2[2,4]*a4 + coef2[2,5]*a5 + coef2[2,6]*a6 + coef2[2,7]*a7 + coef2[2,8]*a8 + coef2[2,9]*a9 + coef2[2,10]*a10 + coef2[2,11]*a11) / (1 + (exp(coef2[1,1] + coef2[1,2]*x + coef2[1,3]*a3 + coef2[1,4]*a4 + coef2[1,5]*a5 + coef2[1,6]*a6 + coef2[1,7]*a7 + coef2[1,8]*a8 + coef2[1,9]*a9 + coef2[1,10]*a10 + coef2[1,11]*a11) + exp(coef2[2,1] + coef2[2,2]*x + coef2[2,3]*a3 + coef2[2,4]*a4 + coef2[2,5]*a5 + coef2[2,6]*a6 + coef2[2,7]*a7 + coef2[2,8]*a8 + coef2[2,9]*a9 + coef2[2,10]*a10 + coef2[2,11]*a11) + exp(coef2[3,1] + coef2[3,2]*x + coef2[3,3]*a3 + coef2[3,4]*a4 + coef2[3,5]*a5 + coef2[3,6]*a6 + coef2[3,7]*a7 + coef2[3,8]*a8 + coef2[3,9]*a9 + coef2[3,10]*a10 + coef2[3,11]*a11)))
fb7 <- function(x) exp(coef2[3,1] + coef2[3,2]*x + coef2[3,3]*a3 + coef2[3,4]*a4 + coef2[3,5]*a5 + coef2[3,6]*a6 + coef2[3,7]*a7 + coef2[3,8]*a8 + coef2[3,9]*a9 + coef2[3,10]*a10 + coef2[3,11]*a11) / (1 + (exp(coef2[1,1] + coef2[1,2]*x + coef2[1,3]*a3 + coef2[1,4]*a4 + coef2[1,5]*a5 + coef2[1,6]*a6 + coef2[1,7]*a7 + coef2[1,8]*a8 + coef2[1,9]*a9 + coef2[1,10]*a10 + coef2[1,11]*a11) + exp(coef2[2,1] + coef2[2,2]*x + coef2[2,3]*a3 + coef2[2,4]*a4 + coef2[2,5]*a5 + coef2[2,6]*a6 + coef2[2,7]*a7 + coef2[2,8]*a8 + coef2[2,9]*a9 + coef2[2,10]*a10 + coef2[2,11]*a11) + exp(coef2[3,1] + coef2[3,2]*x + coef2[3,3]*a3 + coef2[3,4]*a4 + coef2[3,5]*a5 + coef2[3,6]*a6 + coef2[3,7]*a7 + coef2[3,8]*a8 + coef2[3,9]*a9 + coef2[3,10]*a10 + coef2[3,11]*a11)))

x <- seq(6, 12, length.out = 10000)
dl2 <- data.frame(x, fb4(x), fb5(x), fb6(x), fb7(x))

AsiaOceania <- ggplot(dl2) +
  geom_line(aes(x, fb4.x., color = "a"), size = 1) +
  geom_line(aes(x, fb5.x., color = "b"), size = 1) +
  geom_line(aes(x, fb6.x., color = "c"), size = 1) +
  geom_line(aes(x, fb7.x., color = "d"), size = 1) +
  geom_hline(yintercept = 0.9) +
  geom_vline(xintercept = 10.64, linetype = "longdash") +
  geom_text(x=10.64+0.6, y=0.30, label="10.64") +
  ylab("prob.") +
  xlab("log of GDP per capita") +
  scale_color_manual(name = "Levels of Happiness", 
                     values = c("a" = "darkred", "b" = "purple", "c" = "darkblue", "d" = "darkgreen"),
                     labels = c("4", "5", "6", "7"))

### America
a3 <- mean(finaldata$GINI)#[finaldata$America==1])
a4 <- mean(finaldata$CO2)#[finaldata$America==1])
a5 <- mean(finaldata$CPW)#[finaldata$America==1])
a6 <- 0
a7 <- 0
a8 <- 0
a9 <- 0
a10 <- 0
a11 <- 1

fb4 <- function(x) 1 / (1 + exp(coef2[1,1] + coef2[1,2]*x + coef2[1,3]*a3 + coef2[1,4]*a4 + coef2[1,5]*a5 + coef2[1,6]*a6 + coef2[1,7]*a7 + coef2[1,8]*a8 + coef2[1,9]*a9 + coef2[1,10]*a10 + coef2[1,11]*a11) + exp(coef2[2,1] + coef2[2,2]*x + coef2[2,3]*a3 + coef2[2,4]*a4 + coef2[2,5]*a5 + coef2[2,6]*a6 + coef2[2,7]*a7 + coef2[2,8]*a8 + coef2[2,9]*a9 + coef2[2,10]*a10 + coef2[2,11]*a11) + exp(coef2[3,1] + coef2[3,2]*x + coef2[3,3]*a3 + coef2[3,4]*a4 + coef2[3,5]*a5 + coef2[3,6]*a6 + coef2[3,7]*a7 + coef2[3,8]*a8 + coef2[3,9]*a9 + coef2[3,10]*a10 + coef2[3,11]*a11))
fb5 <- function(x) exp(coef2[1,1] + coef2[1,2]*x + coef2[1,3]*a3 + coef2[1,4]*a4 + coef2[1,5]*a5 + coef2[1,6]*a6 + coef2[1,7]*a7 + coef2[1,8]*a8 + coef2[1,9]*a9 + coef2[1,10]*a10 + coef2[1,11]*a11) / (1 + (exp(coef2[1,1] + coef2[1,2]*x + coef2[1,3]*a3 + coef2[1,4]*a4 + coef2[1,5]*a5 + coef2[1,6]*a6 + coef2[1,7]*a7 + coef2[1,8]*a8 + coef2[1,9]*a9 + coef2[1,10]*a10 + coef2[1,11]*a11) + exp(coef2[2,1] + coef2[2,2]*x + coef2[2,3]*a3 + coef2[2,4]*a4 + coef2[2,5]*a5 + coef2[2,6]*a6 + coef2[2,7]*a7 + coef2[2,8]*a8 + coef2[2,9]*a9 + coef2[2,10]*a10 + coef2[2,11]*a11) + exp(coef2[3,1] + coef2[3,2]*x + coef2[3,3]*a3 + coef2[3,4]*a4 + coef2[3,5]*a5 + coef2[3,6]*a6 + coef2[3,7]*a7 + coef2[3,8]*a8 + coef2[3,9]*a9 + coef2[3,10]*a10 + coef2[3,11]*a11)))
fb6 <- function(x) exp(coef2[2,1] + coef2[2,2]*x + coef2[2,3]*a3 + coef2[2,4]*a4 + coef2[2,5]*a5 + coef2[2,6]*a6 + coef2[2,7]*a7 + coef2[2,8]*a8 + coef2[2,9]*a9 + coef2[2,10]*a10 + coef2[2,11]*a11) / (1 + (exp(coef2[1,1] + coef2[1,2]*x + coef2[1,3]*a3 + coef2[1,4]*a4 + coef2[1,5]*a5 + coef2[1,6]*a6 + coef2[1,7]*a7 + coef2[1,8]*a8 + coef2[1,9]*a9 + coef2[1,10]*a10 + coef2[1,11]*a11) + exp(coef2[2,1] + coef2[2,2]*x + coef2[2,3]*a3 + coef2[2,4]*a4 + coef2[2,5]*a5 + coef2[2,6]*a6 + coef2[2,7]*a7 + coef2[2,8]*a8 + coef2[2,9]*a9 + coef2[2,10]*a10 + coef2[2,11]*a11) + exp(coef2[3,1] + coef2[3,2]*x + coef2[3,3]*a3 + coef2[3,4]*a4 + coef2[3,5]*a5 + coef2[3,6]*a6 + coef2[3,7]*a7 + coef2[3,8]*a8 + coef2[3,9]*a9 + coef2[3,10]*a10 + coef2[3,11]*a11)))
fb7 <- function(x) exp(coef2[3,1] + coef2[3,2]*x + coef2[3,3]*a3 + coef2[3,4]*a4 + coef2[3,5]*a5 + coef2[3,6]*a6 + coef2[3,7]*a7 + coef2[3,8]*a8 + coef2[3,9]*a9 + coef2[3,10]*a10 + coef2[3,11]*a11) / (1 + (exp(coef2[1,1] + coef2[1,2]*x + coef2[1,3]*a3 + coef2[1,4]*a4 + coef2[1,5]*a5 + coef2[1,6]*a6 + coef2[1,7]*a7 + coef2[1,8]*a8 + coef2[1,9]*a9 + coef2[1,10]*a10 + coef2[1,11]*a11) + exp(coef2[2,1] + coef2[2,2]*x + coef2[2,3]*a3 + coef2[2,4]*a4 + coef2[2,5]*a5 + coef2[2,6]*a6 + coef2[2,7]*a7 + coef2[2,8]*a8 + coef2[2,9]*a9 + coef2[2,10]*a10 + coef2[2,11]*a11) + exp(coef2[3,1] + coef2[3,2]*x + coef2[3,3]*a3 + coef2[3,4]*a4 + coef2[3,5]*a5 + coef2[3,6]*a6 + coef2[3,7]*a7 + coef2[3,8]*a8 + coef2[3,9]*a9 + coef2[3,10]*a10 + coef2[3,11]*a11)))

x <- seq(6, 12, length.out = 10000)
dl2 <- data.frame(x, fb4(x), fb5(x), fb6(x), fb7(x))

America <- ggplot(dl2) +
  geom_line(aes(x, fb4.x., color = "a"), size = 1) +
  geom_line(aes(x, fb5.x., color = "b"), size = 1) +
  geom_line(aes(x, fb6.x., color = "c"), size = 1) +
  geom_line(aes(x, fb7.x., color = "d"), size = 1) +
  geom_hline(yintercept = 0.9) +
  geom_vline(xintercept = 9.90, linetype = "longdash") +
  geom_text(x=9.90+0.6, y=0.30, label="9.90") +
  ylab("prob.") +
  xlab("log of GDP per capita") +
  scale_color_manual(name = "Levels of Happiness", 
                     values = c("a" = "darkred", "b" = "purple", "c" = "darkblue", "d" = "darkgreen"),
                     labels = c("4", "5", "6", "7"))

plotmodeltest2b <- ggarrange(WestEurope, EastEurope, Africa, MiddleEast, AsiaOceania, America,
                             labels = c("A", "B", "C", "D", "E", "F"),
                             common.legend = TRUE, legend = "bottom",
                             ncol = 2, nrow = 3)

print(plotmodeltest2b)

sink()