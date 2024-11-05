covid_data <- read.csv('D:/covid data.csv')
attach(covid_data)
View(covid_data)
#table1,2
#Africa
length(covid_data$WHO.Region[WHO.Region=='Africa'])
mean(covid_data$Death_cumulative.total[WHO.Region=='Africa'])
sd(covid_data$Death_cumulative.total[WHO.Region=='Africa'])
mean(covid_data$PERSONS_VACCINATED_1PLUS_DOSE[WHO.Region=='Africa'])
sd(covid_data$PERSONS_VACCINATED_1PLUS_DOSE[WHO.Region=='Africa'])

#Americas
length(covid_data$WHO.Region[WHO.Region=='Americas'])
mean(covid_data$Death_cumulative.total[WHO.Region=='Americas'])
sd(covid_data$Death_cumulative.total[WHO.Region=='Americas'])
mean(covid_data$PERSONS_VACCINATED_1PLUS_DOSE[WHO.Region=='Americas'])
sd(covid_data$PERSONS_VACCINATED_1PLUS_DOSE[WHO.Region=='Americas'])

#Eastern Mediterranean
length(covid_data$WHO.Region[WHO.Region=='Eastern Mediterranean'])
mean(covid_data$Death_cumulative.total[WHO.Region=='Eastern Mediterranean'])
sd(covid_data$Death_cumulative.total[WHO.Region=='Eastern Mediterranean'])
mean(covid_data$PERSONS_VACCINATED_1PLUS_DOSE[WHO.Region=='Eastern Mediterranean'])
sd(covid_data$PERSONS_VACCINATED_1PLUS_DOSE[WHO.Region=='Eastern Mediterranean'])

#Europe
length(covid_data$WHO.Region[WHO.Region=='Europe'])
mean(covid_data$Death_cumulative.total[WHO.Region=='Europe'])
sd(covid_data$Death_cumulative.total[WHO.Region=='Europe'])
mean(covid_data$PERSONS_VACCINATED_1PLUS_DOSE[WHO.Region=='Europe'])
sd(covid_data$PERSONS_VACCINATED_1PLUS_DOSE[WHO.Region=='Europe'])

#South-east Asia
length(covid_data$WHO.Region[WHO.Region=='South-East Asia'])
mean(covid_data$Death_cumulative.total[WHO.Region=='South-East Asia'])
sd(covid_data$Death_cumulative.total[WHO.Region=='South-East Asia'])
mean(covid_data$PERSONS_VACCINATED_1PLUS_DOSE[WHO.Region=='South-East Asia'])
sd(covid_data$PERSONS_VACCINATED_1PLUS_DOSE[WHO.Region=='South-East Asia'])

#Western Pacific
length(covid_data$WHO.Region[WHO.Region=='Western Pacific'])
mean(covid_data$Death_cumulative.total[WHO.Region=='Western Pacific'])
sd(covid_data$Death_cumulative.total[WHO.Region=='Western Pacific'])
mean(covid_data$PERSONS_VACCINATED_1PLUS_DOSE[WHO.Region=='Western Pacific'])
sd(covid_data$PERSONS_VACCINATED_1PLUS_DOSE[WHO.Region=='Western Pacific'])

#boxplot
boxplot(covid_data$Death_cumulative.total~WHO.Region,col = "darkorchid1",horizontal = T)

#ANOVA table3
out <-aov(Death_cumulative.total~WHO.Region,data=covid_data)
summary(out)

#Tukey table4
TukeyHSD(out)
plot(TukeyHSD(out))

#part3
plot(PERSONS_VACCINATED_1PLUS_DOSE,Death_cumulative.total,pch=16,col="blue")

#table5
reg <- lm(PERSONS_VACCINATED_1PLUS_DOSE~WHO.Region)
summary(reg)

#table6
reg1 <- lm(PERSONS_VACCINATED_1PLUS_DOSE~Death_cumulative.total)
summary(reg1)
anova(reg1)

cor.test(PERSONS_VACCINATED_1PLUS_DOSE,Death_cumulative.total)
xDead <- c(6455057)
yVac <- c(5331660437)
Sxy <- sum(xDead*yVac)-sum(xDead)*sum(yVac)/223
Sxx <- sum(xDead*xDead)-sum(xDead)*sum(xDead)/223
Syy <- sum(yVac*yVac)-sum(yVac)*sum(yVac)/223
b1 <- Sxy/Sxx
b0 <- mean(yVac)-b1*mean(xDead)
s <- sqrt(Syy-((Sxy*Sxy)/Sxx)/223-2)
tb0 <- (b0-0)/(s*(sqrt(sum(xDead)*sum(xDead)/223*Sxx)))
tb1 <- (b1-0)/(s*(sqrt(Sxx)))