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

FAA$dist <- ifelse(FAA$distance <1000 , 1, ifelse(FAA$distance <2500, 2, 3))
FAA <- FAA[ , -7]

attach(FAA)


# transforming aircraft and dist as factors ; dist is an ordered factor
FAA$aircraft <- as.factor(FAA$aircraft)
FAA$dist <- factor(FAA$dist, ordered = TRUE)

plot(dist ~ speed_ground)
plot(dist ~ height)



#Visualizing relationship between different predictors and response variable Y

#Aircraft vs Dist
with(FAA, table(dist, aircraft))
ggplot(FAA, aes(x = dist, fill = aircraft)) + 
  geom_bar(position = "dodge", width = 0.5) + 
  ggtitle("Aircraft vs Distance Category") +
  xlab("Distance Category")

#Speed_ground vs Dist
FAA %>% ggplot(aes(x = dist, y = speed_ground, fill = dist)) + 
  geom_boxplot() + 
  ggtitle("Speed Ground vs Distance Category") +
  xlab("Distance Category")

#Height vs Dist
FAA %>% ggplot(aes(x = dist, y = height, fill = dist)) + 
  geom_boxplot() + 
  ggtitle("Height vs Distance Category") +
  xlab("Distance Category")

#no_pasg vs Dist
FAA %>% ggplot(aes(x = dist, y = no_pasg, fill = dist)) + 
  geom_boxplot() + 
  ggtitle("Number of passengers vs Distance Category") +
  xlab("Distance Category")

# model 
library(VGAM)
nmod<-vglm(dist ~ .-speed_air, family=cumulative(parallel=TRUE), FAA)
summary(nmod)

# fitting new model on the selected variables
new_model <- vglm(dist ~ speed_ground + height + aircraft, family=cumulative(parallel=TRUE), FAA)
summary(new_model)


################
## QUESTION 2 ##
################

#Fitting a GLM model with Poisson distribution
model_pass <- glm(no_pasg ~ .,family = poisson, data = FAA)
summary(model_pass)

# goodness of fit measure 
gof <- sum(residuals(model_pass, type = "pearson") ^ 2)
gof
# 158.9949 

df.residual(model_pass)
# 187

pchisq(gof, df.residual(model_pass), lower = F)

#updating the model
dp <- gof/model_no_pasg$df.res
summary(model_no_pasg,dp)

#Checking correlation
round(cor(FAA[c(2,3,4,6,7)], use = 'pairwise.complete.obs'), 4)

