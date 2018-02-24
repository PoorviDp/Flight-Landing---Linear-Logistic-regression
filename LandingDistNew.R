library(readxl)
FAA1 <- read_excel("~/SPRING/StatModelling/FAA1.xls")
FAA2 <- read_excel("~/SPRING/StatModelling/FAA2.xls")

str(FAA1)
str(FAA2)

FAA <- merge(FAA1, FAA2, all = T)

str(FAA)
duplicates <- duplicated(FAA)
sum(duplicates)
summary(FAA)


abnormal <- c(which(FAA$speed_ground < 30 | FAA$speed_ground > 140),
              which(FAA$duration <= 40),
              which(FAA$distance > 6000),
              which(FAA$height < 6)
)

length(unique(abnormal))

FAA_New <- FAA[-abnormal,]
summary(FAA_New)
str(FAA_New)

library(Hmisc)
par(mfrow = c(2,2))
hist.data.frame(FAA_New)

faa<- FAA_New[, -1]
correlation <- cor(faa, use = "complete.obs")[,6]
correlation

tab <- data.frame(names(correlation), abs(correlation), sign(correlation))
colnames(tab) <- c("col name", "size of correlation" , "direction of correlation") 
tab
tab <- tab[order(tab$`size of correlation`),]
plot(faa)

faa$type <- ifelse(FAA_New$aircraft =="airbus", 1, 0)

plot(faa)

reg<- function(x){
  summary(lm(faa$distance ~ x))$coefficients[2,4]
}

regress <- apply(faa, 2, reg)
regress

rege<- function(z){
  summary(lm(faa$distance ~ z))$coefficients[2,1]
}

regressCoef <- apply(faa, 2, rege)
regressCoef

# l1 <- lm(distance~pitch, faa)
# sum1 <- summary(l1)
# str(sum1)
# sum1$coefficients[2,4]

table2 <-  data.frame(names(regress), abs(regress), sign(regressCoef))
colnames(table2) <- c("col name", "size of pvalue" , "direction of regression coefficient") 
table2 <- table2[order(table2$`size of pvalue`),]


std_faa<- as.data.frame(scale(x = dplyr::select(faa, -distance), center = TRUE, scale = TRUE))
summary(std_faa)
sd(std_faa, na.rm = TRUE)

std_faa <- cbind(std_faa, faa$distance)
str(std_faa)
colnames(std_faa)[8] <- "distance"
summary(std_faa)


## Regression on all variables
reg1<- function(y){
  summary(lm(std_faa$distance ~ y))$coefficients[2,1]
}

regress1 <- apply(std_faa, 2, reg1)
regress1


table3 <- data.frame(names(regress1), abs(regress1), sign(regress1))
colnames(table3) <- c("variable", "size of regression coef" , "direction of regression coef") 
table3 <- table3[order(table3$`size of regression coef`),]
table3


model1 <- lm(distance ~ speed_ground, faa)
summary(model1)

model2 <- lm(distance ~ speed_air, faa)
summary(model2)

model3 <- lm(distance ~ speed_ground+speed_air, faa)
summary(model3)

detach(faa)
attach(faa)
model1 <- lm(distance ~ speed_air)

model2 <- lm(distance ~ speed_air+type)

model3 <- lm(distance ~ speed_air+type+height)

model4 <- lm(distance ~ speed_air+type+height+duration)

model5 <- lm(distance ~ speed_air+type+height+duration+pitch)

model6 <- lm(distance ~ speed_air+type+height+duration+pitch+no_pasg)

r.squared <- c(summary(model1)$r.squared, summary(model2)$r.squared, summary(model3)$r.squared, summary(model4)$r.squared,
               summary(model5)$r.squared, summary(model6)$r.squared)


adj.r.squared <- c(summary(model1)$adj.r.squared, summary(model2)$adj.r.squared, 
                   summary(model3)$adj.r.squared, summary(model4)$adj.r.squared,
                   summary(model5)$adj.r.squared, summary(model6)$adj.r.squared)

aic <- AIC(model1, model2, model3, model4, model5, model6)$AIC

plot(1:6, r.squared, type = "l")
plot(1:6, adj.r.squared, type = "l")
plot(1:6, aic, type = "l")

faa <- na.omit(faa)

final_model <- lm(distance~., data = faa)

install.packages("MASS")
library(MASS)
step <- stepAIC(model6, direction = "both")
step$anova
summary(step)
