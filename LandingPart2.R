## CLEANING DATA

library(readxl)
library(tidyr)
library(dplyr)
library(ggplot2)

FAA1 <- read_excel("~/SPRING/StatModelling/FAA1.xls")
FAA2 <- read_excel("~/SPRING/StatModelling/FAA2.xls")

FAA <- merge(FAA1, FAA2, all = T)

duplicates <- duplicated(FAA)
sum(duplicates)

abnormal <- c(which(FAA$speed_ground < 30 | FAA$speed_ground > 140),
              which(FAA$duration <= 40),
              which(FAA$distance > 6000),
              which(FAA$height < 6)
)

length(abnormal)

FAA <- FAA[-abnormal,]
summary(FAA)
str(FAA)

# STEP 1

# dropping distance
FAA$aircraft_type <- ifelse(FAA$aircraft =="airbus", 1, 0)
FAA <- FAA[, -1]


FAA$long.landing <- ifelse(FAA$distance > 2500, 1, 0)
FAA$risky.landing <- ifelse(FAA$distance > 3000, 1, 0)
FAA <- FAA[, -6]

attach(FAA)

#STEP 2

hist(long.landing)


# STEP 3
# Regression using a single variable
# table should contain 
# the names of variables, 
# the size of the regression coefficient, 
# the odds ratio, 
# the direction of the regression coefficient (positive or negative), 
# and the p-value.



## function for extracting size of regression coef
sizeOfCoef<- function(x){
  summary(glm(long.landing ~ x, family=binomial,data = FAA))$coefficients[2,"Estimate"]
}

regress <- sapply(FAA[,1:7], sizeOfCoef)
regress

## function for extracting direction of coef
dirOfCoef<- function(x){
  sign(summary(glm(long.landing ~ x, family=binomial,data = FAA))$coefficients[2,"Estimate"])
}

direction <- sapply(FAA[,1:7], dirOfCoef)
direction


## function for extracting pvalue
pval<- function(x){
  summary(glm(long.landing ~ x, family=binomial,data = FAA))$coefficients[2,4]
}

pValue <- sapply(FAA[,1:7], pval)
pValue

## function to extracting odds
oddsRatio <- exp(regress)

table1 <- data.frame(names(pValue), abs(regress), direction, pValue, oddsRatio)

colnames(table1) <- c("name", "sizeOfCoef" , "directionOfCoef", "pValue", "OddsRatio") 
table1 <- table1[order(-table1$sizeOfCoef),]
table1
write.csv(table1, file = "boston.csv")
## 

## standardized coefficients

std_faa<- as.data.frame(scale(x = dplyr::select(FAA, -c(long.landing, risky.landing)), center = TRUE, scale = TRUE))

CoefStd<- function(x){
  summary(glm(long.landing ~ x, family=binomial,data = std_faa))$coefficients[2,"Estimate"]
}


regressStd <- sapply(std_faa[,1:7], CoefStd)
regressStd
odds <- exp(regressStd)
table2 <- data.frame(names(pValue), abs(regressStd), direction, pValue, odds)
colnames(table2) <- c("name", "StdCoef" , "directionOfCoef", "pValue", "OddsRatio") 
table2 <- table2[order(-table2$StdCoef),]
table2


#######################################################################
#######################################################################
# PLOTS #

par(mfrow = c(2,2))
plot(as.factor(long.landing) ~ speed_ground)
plot(as.factor(long.landing) ~ speed_air)
plot(as.factor(long.landing) ~ aircraft_type)
plot(as.factor(long.landing) ~ pitch)

plot(jitter(long.landing,0.1)~jitter(speed_ground), FAA, 
     xlab="speed_ground", ylab="Long Landing", pch=".")

plot(jitter(long.landing,0.1)~jitter(speed_air), FAA, 
     xlab="speed_air", ylab="Long Landing", pch=".")

plot(jitter(long.landing,0.1)~jitter(aircraft_type), FAA, 
     xlab="aircraft_type", ylab="Long Landing", pch=".")

plot(jitter(long.landing,0.1)~jitter(height), FAA, 
     xlab="height", ylab="Long Landing", pch=".")

plot1 <- ggplot(FAA, aes(x=speed_ground, fill = factor(long.landing))) + 
  geom_histogram(position = "dodge")
plot2 <- ggplot(FAA, aes(x=speed_air, fill = factor(long.landing))) + 
  geom_histogram(position = "dodge")
plot3 <- ggplot(FAA, aes(x=(aircraft_type), fill = as.factor(long.landing))) + 
  geom_histogram(position = "dodge")
plot4 <- ggplot(FAA, aes(x=height, fill = factor(long.landing))) + 
  geom_histogram(position = "dodge")

grid.arrange(plot1, plot2, plot3, plot4, nrow=2, ncol=2)

#####################################################################
#####################################################################

# step 5 

## full model with three variables

full <- glm(long.landing ~ speed_ground + aircraft_type + pitch , family=binomial,data = FAA)
summary(full)

# step 6

# STEPWISE 
nullmodel<- glm(long.landing~1, family=binomial, data=FAA)
fullmodel <- glm(long.landing ~ speed_ground + aircraft_type + pitch + height +duration + no_pasg, family=binomial, data=FAA)

# FORWARD selection using AIC
model.step.forward<- step(nullmodel, scope= ~speed_ground + aircraft_type + pitch + height +duration + no_pasg, direction='forward')
summary(model.step.forward)

forward.model <- glm(formula = long.landing ~ speed_ground + aircraft_type + height + pitch, family = binomial, data = FAA)
summary(forward.model)           


# FORWARD selection using BIC
model.step.BIC<- step(nullmodel, scope=list(lower=nullmodel, upper=fullmodel), direction='forward', k=log(nrow(FAA)))
summary(model.step.BIC)

forward.model.BIC.long <- glm(formula = long.landing ~ speed_ground + aircraft_type + height, family = binomial, data = FAA)
summary(forward.model.BIC.long)  


Final.model <- glm(long.landing ~ speed_ground + aircraft_type + height, family = binomial)
m <- summary(Final.model)
exp(m$coefficients[,1])

#########################################*************************************##############################################
################################################# RISKY.LANDING ###########################################################
#######################################......................................##############################################

FAA %>% 
  ggplot(aes(x=risky.landing)) + geom_histogram()


## function for extracting size of regression coef
Coef<- function(x){
  summary(glm(risky.landing ~ x, family=binomial,data = FAA))$coefficients[2,"Estimate"]
}

regress <- sapply(FAA[,1:7], Coef)
regress

## function for extracting direction of coef
directn<- function(x){
  sign(summary(glm(risky.landing ~ x, family=binomial,data = FAA))$coefficients[2,"Estimate"])
}

direction <- sapply(FAA[,1:7], directn)
direction


## function for extracting pvalue
pval<- function(x){
  summary(glm(risky.landing ~ x, family=binomial,data = FAA))$coefficients[2,4]
}

pValue <- sapply(FAA[,1:7], pval)
pValue

## function to extracting odds
odds <- exp(regress)

table1 <- data.frame(abs(regress), direction, pValue, oddsRatio)

colnames(table1) <- c("sizeOfCoef" , "directionOfCoef", "pValue", "OddsRatio") 
table1 <- table1[order(-table1$sizeOfCoef),]
table1
write.csv(table1, file = "risky.csv")
## 

## standardized coefficients

std_faa_risky<- as.data.frame(scale(x = dplyr::select(FAA, -c(long.landing, risky.landing)), center = TRUE, scale = TRUE))

CoefStd<- function(x){
  summary(glm(risky.landing ~ x, family=binomial,data = std_faa_risky))$coefficients[2,"Estimate"]
}

regressStd <- sapply(std_faa[,1:7], CoefStd)
regressStd
oddsRisky <- exp(regressStd)
table2 <- data.frame(abs(regressStd), direction, pValue, odds)
colnames(table2) <- c("StdCoef" , "directionOfCoef", "pValue", "OddsRatio") 
table2 <- table2[order(-table2$StdCoef),]
table2
write.csv(table2, file = "risky.csv")

#######################################################################
#######################################################################
# PLOTS #

par(mfrow = c(2,2))
plot(as.factor(risky.landing) ~ speed_ground)
plot(as.factor(risky.landing) ~ speed_air)
plot(as.factor(risky.landing) ~ aircraft_type)
plot(as.factor(risky.landing) ~ pitch)

plot(jitter(risky.landing,0.1)~jitter(speed_ground), FAA, 
     xlab="speed_ground", ylab="Risky Landing", pch=".")

plot(jitter(risky.landing,0.1)~jitter(speed_air), FAA, 
     xlab="speed_air", ylab="Risky Landing", pch=".")

plot(jitter(risky.landing,0.1)~jitter(aircraft_type), FAA, 
     xlab="aircraft_type", ylab="Risky Landing", pch=".")

plot(jitter(risky.landing,0.1)~jitter(height), FAA, 
     xlab="height", ylab="Risky Landing", pch=".")

plot1 <- ggplot(FAA, aes(x=speed_ground, fill = factor(risky.landing))) + 
  geom_histogram(position = "dodge")
plot2 <- ggplot(FAA, aes(x=speed_air, fill = factor(risky.landing))) + 
  geom_histogram(position = "dodge")
plot3 <- ggplot(FAA, aes(x=(aircraft_type), fill = as.factor(risky.landing))) + 
  geom_histogram(position = "dodge")
plot4 <- ggplot(FAA, aes(x=height, fill = factor(risky.landing))) + 
  geom_histogram(position = "dodge")

grid.arrange(plot1, plot2, plot3, plot4, nrow=2, ncol=2)

#####################################################################
#####################################################################

# significant factors identified
# * Speed_ground
# * Speed_air
# * Aircraft_type

# step 5 

## full model for significant variables

full.risky <- glm(risky.landing ~. , family=binomial,data = FAA)
summary(full.risky)

# step 6

# STEPWISE 
nullmodel<- glm(risky.landing~1, family=binomial, data=FAA)
fullmodel <- glm(risky.landing ~ speed_ground + aircraft_type + pitch + height +duration + no_pasg, family=binomial, data=FAA)

# FORWARD selection using AIC
model.step.forward<- step(nullmodel, scope= ~speed_ground + aircraft_type + pitch + height +duration + no_pasg, direction='forward')
summary(model.step.forward)

forward.model <- glm(formula = risky.landing ~ speed_ground + aircraft_type + height + pitch, family = binomial, data = FAA)
summary(forward.model)           


# FORWARD selection using BIC
model.step.BIC<- step(nullmodel, scope=list(lower=nullmodel, upper=fullmodel), direction='forward', k=log(nrow(FAA)))
summary(model.step.BIC)

forward.model.BIC <- glm(formula = risky.landing ~ speed_ground + aircraft_type, family = binomial, data = FAA)
summary(forward.model.BIC)  


Final.model <- glm(risky.landing ~ speed_ground + aircraft_type, family = binomial)
m <- summary(Final.model)
exp(m$coefficients[,1]) # odds

#####################################################################
######################  ROC  ########################################
#####################################################################



install.packages("AUC")
library(AUC)
plot(roc(as.vector(predict(forward.model.BIC.long,FAA,type = "response")),factor(FAA$long.landing)),col="red")
plot(roc(as.vector(predict(forward.model.BIC,FAA,type = "response")),factor(FAA$risky.landing)),col="green",add=T,type="p")


auc(roc(as.vector(predict(forward.model.BIC.long,FAA,type = "response")),factor(FAA$long.landing)))
auc(roc(as.vector(predict(forward.model.BIC,FAA,type = "response")),factor(FAA$risky.landing)))

# STEP 13
new.data <- data.frame(aircraft_type =0, no_pasg=80, speed_ground=115, speed_air=120,height=40, pitch=4)
predict(forward.model.BIC.long,newdata=new.data,type="response",se=T)
predict(forward.model.BIC,newdata=new.data,type="response",se=T)


# STEP 14

model.log <- glm(risky.landing ~ speed_ground+aircraft_type, family = binomial, FAA)
model.probit <- glm(risky.landing ~ speed_ground+aircraft_type, family = binomial(link=probit), FAA)
model.cloglog <- glm(risky.landing ~ speed_ground+aircraft_type, family = binomial(link=cloglog), FAA)
round(coef(model.log),3)
round(coef(model.probit),3)
round(coef(model.cloglog),3)

# cauchit > logit > probit

# STEP 15

library(verification)

pred.prob.logit <- predict(model.log, type = "response")
pred.prob.probit <- predict(model.probit, type = "response")
pred.prob.cloglog <- predict(model.cloglog, type = "response")

pred.logit <- prediction(pred.prob.logit,risky.landing)
pred.probit <- prediction(pred.prob.probit,risky.landing)
pred.cloglog <- prediction(pred.prob.cloglog,risky.landing)

perf_logit <- performance(pred.logit, "tpr", "fpr")
perf_probit <- performance(pred.probit, "tpr", "fpr")
perf_cloglog <- performance(pred.cloglog, "tpr", "fpr")

plot(perf_logit, col = "blue")
plot(perf_probit, add = TRUE, col="red")
plot(perf_cloglog, add = TRUE, col="green")

# STEP 16

head(sort(fitted(model.log), decreasing = TRUE))
head(sort(fitted(model.probit), decreasing = TRUE))
head(sort(fitted(model.cloglog), decreasing = TRUE))
FAA[c(521,788,832,686,229,800), c(2,7)]
FAA[c(56,64,135,177,180,309),c(1:6,8)]
FAA[c(19,29,30,56,64,91),c(1:6,8)]

#
predict(model.probit,newdata=new.data,type="response",se=T)
predict(model.cloglog,newdata=new.data,type="response",se=T)




#####################################################################


model1 <- lm(distance ~ duration)
summary(model1)

model2 <- lm(distance ~ no_pasg)
summary(model2)

model3 <- lm(distance ~ speed_air)
summary(model3)

model4 <- lm(distance ~ speed_ground)
summary(model4)

model5 <- lm(distance ~ height)
summary(model5)

model6 <- lm(distance ~ pitch)
summary(model6)

model7 <- lm(distance ~ aircraft_type)
summary(model7)


write.csv(summary(Boston), file = "boston.csv")



