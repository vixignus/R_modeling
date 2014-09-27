setwd("~/Dropbox/Staples/Backup_files")
#install.packages("orcutt")
#install.packages("nlme")
library(nlme)
library(sqldf)
library(car)
library(MASS)

smd=read.csv('staples_modeling_data.txt',sep='\t')
head(smd)

#modifying BuyFlag (indicates if there was a "Buy 2 get 1" kind of an offer) 
smd$BuyFlag1[smd$BuyFlag=='Buy 1 get 1'] <- 1
smd$BuyFlag1[smd$BuyFlag=='Buy 2 get 1'] <- 2
smd$BuyFlag1[is.na(smd$BuyFlag)] <- 0
drops <- c('BuyFlag')
smd <- data.frame(smd[,!(names(smd) %in% drops)])
rownames(smd) <- paste(smd$WeekNum,'-',smd$Year, '-', smd$productkey)

data_371792 <- smd[which(smd$productkey == 371792),]
sales_peaks <- sqldf("select WeekNum, sum(netsales) as sales from data_371792 group by WeekNum order by sum(netsales)")
barplot(sales_peaks$sales)
data_371792$seasonal <- ifelse(data_371792$WeekNum %in% seq(44,52),1,0)

drops <- c('WeekNum','productkey','Year','grosssales', 'unitvolumn','X.SalesSpike','X.VolumeSpike')
data_371792_mod <- data.frame(data_371792[,!(names(data_371792) %in% drops)])

#Add centering for interaction 
data_371792_mod$PDint <- data_371792_mod$PD - mean(data_371792_mod$PD)
data_371792_mod$ERint <- data_371792_mod$ER - mean(data_371792_mod$ER)
data_371792_mod$OPint <- data_371792_mod$OriginalPrice - mean(data_371792_mod$OriginalPrice)

cor(data_371792_mod)

#build the Regression model

#with no interactions
fit <- lm(netsales ~ PD+ER+OriginalPrice+seasonal,data_371792_mod)
step_fit <- step(fit,k=log(nrow(data_371792_mod)), direction = c("both"))

fit <- lm(netsales ~ PDint + seasonal + OPint + seasonal:OPint, data_371792_mod)

anova(step_fit,fit)

#check the final model
plot(fit)
plot(predict(fit, data_371792_mod),fit$res)
plot(predict(fit, data_371792_mod),data_371792_mod$netsales)

plot(fitted(fit),residuals(fit))
abline(h=0)
#run diagnostics/plot the predicted vs. actual
summary(fit)
#non-constancy of variance
ncvTest(fit) #passed
vif(fit) #passed
qqPlot(fit, main="QQ Plot") #asymptotically normal
durbinWatsonTest(fit, max.lag=4) #failed
acf(fit$res) #additional checks
#remedial measures - generalized linear squares
fit <- gls(netsales ~ PDint + OPint + seasonal + OPint:seasonal,data_371792_mod)
ac_fit1 <- gls(netsales ~ PDint + OPint + seasonal + OPint:seasonal,correlation = corAR1(form=~1),data_371792_mod)
ac_fit1.1 <- update(ac_fit1, correlation=corARMA(p=1))
ac_fit1.2 <- update(ac_fit1, correlation=corARMA(p=2))
ac_fit1.3 <- update(ac_fit1, correlation=corARMA(p=3))
ac_fit1.4 <- update(ac_fit1, correlation=corARMA(q=1))
ac_fit1.5 <- update(ac_fit1, correlation=corARMA(q=2))
ac_fit1.6 <- update(ac_fit1, correlation=corARMA(q=3))
ac_fit1.7 <- update(ac_fit1, correlation=corARMA(p=1,q=1))
ac_fit1.8 <- update(ac_fit1, correlation=corARMA(p=1,q=2))
ac_fit1.9 <- update(ac_fit1, correlation=corARMA(p=2,q=1))

ac_fit1 <- update(ac_fit1, correlation=NULL)

anova(ac_fit1,ac_fit1.1)
anova(ac_fit1,ac_fit1.2)
anova(ac_fit1,ac_fit1.3)
anova(ac_fit1,ac_fit1.4)
anova(ac_fit1,ac_fit1.5)
anova(ac_fit1,ac_fit1.6)
anova(ac_fit1,ac_fit1.7)
anova(ac_fit1,ac_fit1.8)
anova(ac_fit1,ac_fit1.9)

#choose ac_fit1.1 based on BIC as the final model
fit <- ac_fit1.1
summary(fit)

plot(fitted(fit),residuals(fit))
abline(h=0)
plot(predict(fit, data_371792_mod),data_371792_mod$netsales)


data_463710 <- smd[which(smd$productkey == 463710),]
sales_peaks <- sqldf("select WeekNum, sum(netsales) as sales from data_463710 group by WeekNum order by sum(netsales)")
barplot(sales_peaks$sales)
sales_peaks
data_463710$seasonal <- ifelse(data_463710$WeekNum %in% seq(32,34),1,0)
drops <- c('WeekNum','productkey','Year','grosssales', 'unitvolumn','X.SalesSpike','X.VolumeSpike')
data_463710_mod <- data.frame(data_463710[,!(names(data_463710) %in% drops)])
#Add centering for interaction 
data_463710_mod$PDint <- data_463710_mod$PD - mean(data_463710_mod$PD)
data_463710_mod$ERint <- data_463710_mod$ER - mean(data_463710_mod$ER)
data_463710_mod$OPint <- data_463710_mod$OriginalPrice - mean(data_463710_mod$OriginalPrice)

cor(data_463710_mod)

#build the Regression model

#with no interactions
fullfit <- lm(netsales ~ PD+ER+OriginalPrice+seasonal,data_463710_mod)
step_fit <- step(fullfit,k=log(nrow(data_463710_mod)), direction = c("both"))

fit <- lm(netsales ~ PDint + ERint + seasonal + OPint + PDint:OPint ,data_463710_mod)

#check the final model
plot(predict(fit, data_463710_mod),fit$res)
plot(predict(fit, data_463710_mod),data_463710_mod$netsales)

#run diagnostics/plot the predicted vs. actual

#non-constancy of variance
ncvTest(fit) #passed
vif(fit) #passed
vif(step_fit)
crPlots(fit)
ceresPlots(fit)
durbinWatsonTest(fit, max.lag=10) #failed
qqPlot(fit, main="QQ Plot")

acf(fit$res) #additional checks

#remedial measures
fit <- gls(netsales ~ PDint + ERint + seasonal + OPint + PDint:OPint ,data_463710_mod)
ac_fit1 <- gls(netsales ~ PDint + ERint + seasonal + OPint + PDint:OPint ,correlation = corAR1(form=~1),data_463710_mod)
ac_fit1.1 <- update(ac_fit1, correlation=corARMA(p=1))
ac_fit1.2 <- update(ac_fit1, correlation=corARMA(p=2))
ac_fit1.3 <- update(ac_fit1, correlation=corARMA(p=3))
ac_fit1.4 <- update(ac_fit1, correlation=corARMA(q=1))
ac_fit1.5 <- update(ac_fit1, correlation=corARMA(q=2))
ac_fit1.6 <- update(ac_fit1, correlation=corARMA(q=3))
ac_fit1.7 <- update(ac_fit1, correlation=corARMA(p=1,q=1))
ac_fit1.8 <- update(ac_fit1, correlation=corARMA(p=1,q=2))
ac_fit1.9 <- update(ac_fit1, correlation=corARMA(p=2,q=1))

ac_fit1 <- update(ac_fit1, correlation=NULL)

anova(ac_fit1,ac_fit1.1)
anova(ac_fit1,ac_fit1.2)
anova(ac_fit1,ac_fit1.3)
anova(ac_fit1,ac_fit1.4)
anova(ac_fit1,ac_fit1.5)
anova(ac_fit1,ac_fit1.6)
anova(ac_fit1,ac_fit1.7)
anova(ac_fit1,ac_fit1.8)
anova(ac_fit1,ac_fit1.9)

#based on BIC ARMA(1,1) has been chosen

ac_fit <- ac_fit1.7
summary(ac_fit)
summary(fit)
anova(fit,ac_fit)

plot(fitted(ac_fit),residuals(ac_fit))
abline(h=0)
plot(predict(ac_fit, data_463710_mod),data_463710_mod$netsales)


data_480548 <- smd[which(smd$productkey == 480548),]
sales_peaks <- sqldf("select WeekNum, sum(netsales) as sales from data_480548 group by WeekNum order by sum(netsales)")
sales_peaks
barplot(sales_peaks$sales)
data_480548$seasonal <- ifelse(data_480548$WeekNum %in% c(52,1,2),1,0)
#delete the first 3 ID columns
drops <- c('WeekNum','productkey','Year','grosssales', 'unitvolumn','X.SalesSpike','X.VolumeSpike')
data_480548_mod <- data.frame(data_480548[,!(names(data_480548) %in% drops)])
#build the Regression model
data_480548_mod$PDint <- data_480548_mod$PD - mean(data_480548_mod$PD)
data_480548_mod$CCint <- data_480548_mod$CC - mean(data_480548_mod$CC)
data_480548_mod$OPint <- data_480548_mod$OriginalPrice - mean(data_480548_mod$OriginalPrice)

cor(data_480548_mod)

#build the Regression model

#with no interactions
fullfit <- lm(netsales ~ PD+CC+factor(BuyFlag1)+OriginalPrice+seasonal,data_480548_mod)
step_fit <- step(fullfit,k=log(nrow(data_480548_mod)), direction = c("both"))
# with interactions only with seasonal flag
fit <- lm(formula = netsales ~ PDint + seasonal + PDint:seasonal, data = data_480548_mod)
#check the final model
plot(predict(fit, data_480548_mod),fit$res)
plot(predict(fit, data_480548_mod),data_480548_mod$netsales)

#run diagnostics/plot the predicted vs. actual
#non-constancy of variance
ncvTest(fit)#failed
outlierTest(fit)
vif(fit)
vif(step_fit)
crPlots(fit)
ceresPlots(fit)
durbinWatsonTest(fit) #passed
qqPlot(fit, main="QQ Plot")

fit3 <- lm(log(netsales) ~ (PDint+CCint+factor(BuyFlag1))*seasonal+(PDint+CCint+factor(BuyFlag1))*OPint+OPint*seasonal,data_480548_mod)
step_fit3 <- step(fit3,k=log(nrow(data_480548_mod)), direction = c("both"))

summary(fit)
summary(step_fit3)

BIC(fit)
BIC(step_fit3) #much better BIC and almost same R-sq

fit <- step_fit3

outlierTest(fit)
ncvTest(fit)#passed
outlierTest(fit)
vif(fit)
vif(step_fit)
crPlots(fit)
ceresPlots(fit)
durbinWatsonTest(fit) #FAILED



#remove the 2 outliers and reconstruct the model

data_480548_mod_removed <- data_480548_mod[!(rownames(data_480548_mod) %in% c("1 - 2013 - 480548","52 - 2012 - 480548")),]
fit4 <- lm(log(netsales) ~ (PDint+CCint+factor(BuyFlag1))*seasonal+(PDint+CCint+factor(BuyFlag1))*OPint+OPint*seasonal,data_480548_mod_removed)
step_fit4 <- step(fit4,k=log(nrow(data_480548_mod_removed)), direction = c("both"))

fit<- step_fit4

outlierTest(fit)
ncvTest(fit)#passed
outlierTest(fit)
vif(fit)
vif(step_fit)
crPlots(fit)
ceresPlots(fit)
durbinWatsonTest(fit) #FAILED

summary(fit)

#now applying GLS
fit <- gls(log(netsales) ~ PDint + seasonal + OPint + PDint:seasonal ,data_480548_mod_removed)
ac_fit1 <- gls(log(netsales) ~ PDint + seasonal + OPint + PDint:seasonal ,correlation = corAR1(form=~1),data_480548_mod_removed)
ac_fit1.1 <- update(ac_fit1, correlation=corARMA(p=1))
ac_fit1.2 <- update(ac_fit1, correlation=corARMA(p=2))
ac_fit1.3 <- update(ac_fit1, correlation=corARMA(p=3))
ac_fit1.4 <- update(ac_fit1, correlation=corARMA(q=1))
ac_fit1.5 <- update(ac_fit1, correlation=corARMA(q=2))
ac_fit1.6 <- update(ac_fit1, correlation=corARMA(q=3))
ac_fit1.7 <- update(ac_fit1, correlation=corARMA(p=1,q=1))
ac_fit1.8 <- update(ac_fit1, correlation=corARMA(p=1,q=2))
ac_fit1.9 <- update(ac_fit1, correlation=corARMA(p=2,q=1))

ac_fit1 <- update(ac_fit1, correlation=NULL)

anova(ac_fit1,ac_fit1.1)
anova(ac_fit1,ac_fit1.2)
anova(ac_fit1,ac_fit1.3)
anova(ac_fit1,ac_fit1.4)
anova(ac_fit1,ac_fit1.5)
anova(ac_fit1,ac_fit1.6)
anova(ac_fit1,ac_fit1.7)
anova(ac_fit1,ac_fit1.8)
anova(ac_fit1,ac_fit1.9)

ac_fit <- ac_fit1.2
summary(ac_fit)
summary(fit)
anova(fit,ac_fit)

plot(ac_fit)
plot(fitted(ac_fit),residuals(ac_fit))
plot(predict(ac_fit, data_480548_mod_removed),data_480548_mod_removed$netsales)



data_513096 <- smd[which(smd$productkey == 513096),]
sales_peaks <- sqldf("select WeekNum, sum(netsales) as sales from data_513096 group by WeekNum order by sum(netsales)")
sales_peaks
barplot(sales_peaks$sales)
data_513096$seasonal <- ifelse(data_513096$WeekNum %in% c(52,1,35),1,0)

#delete the first 3 ID columns
drops <- c('WeekNum','productkey','Year','grosssales', 'unitvolumn','X.SalesSpike','X.VolumeSpike')
data_513096_mod <- data.frame(data_513096[,!(names(data_513096) %in% drops)])
cor(data_513096_mod)
#build the Regression model
#Add centering for interaction 
data_513096_mod$PDint <- data_513096_mod$PD - mean(data_513096_mod$PD)
data_513096_mod$CCint <- data_513096_mod$CC - mean(data_513096_mod$CC)
data_513096_mod$ERint <- data_513096_mod$ER - mean(data_513096_mod$ER)
data_513096_mod$BMSMint <- data_513096_mod$BMSM - mean(data_513096_mod$BMSM)
data_513096_mod$OPint <- data_513096_mod$OriginalPrice - mean(data_513096_mod$OriginalPrice)

cor(data_513096_mod)

#build the Regression model
#with no interactions
fit <- lm(netsales ~ PD+ER+CC+BMSM+OriginalPrice+seasonal+factor(BuyFlag1),data_513096_mod)
step_fit <- step(fit,k=log(nrow(data_513096_mod)), direction = c("both"))
# with interactions only with seasonal flag
fit2 <- lm(netsales ~ (PDint+ERint+CCint+BMSM+factor(BuyFlag1)+OPint)*seasonal,data_513096_mod)
step_fit2 <- step(fit2,k=log(nrow(data_513096_mod)), direction = c("both"))
# with interactions with seasonal flag & Original Price
fit3 <- lm(netsales ~ (PDint+CCint+BMSM+factor(BuyFlag1))*seasonal+(PDint+CCint+BMSM+factor(BuyFlag1))*OPint+OPint*seasonal,data_513096_mod)
step_fit3 <- step(fit3,k=log(nrow(data_513096_mod)), direction = c("both"))

anova(step_fit,step_fit2, step_fit3)
anova(step_fit3, step_fit)
summary(step_fit)
summary(step_fit2)
summary(step_fit3)
BIC(step_fit3)
BIC(step_fit2)
BIC(step_fit)

sqrt(mean(resid(step_fit)^2))
sqrt(mean(resid(step_fit2)^2))
sqrt(mean(resid(step_fit3)^2))

summary(step_fit3)
fit_test <- lm(netsales ~ PDint + seasonal + PDint:OPint, 
               data = data_513096_mod)
summary(fit_test)
BIC(fit_test)
BIC(step_fit3)
fit <- fit_test
#check the final model
plot(predict(fit, data_513096_mod),fit$res)
plot(predict(fit, data_513096_mod),data_513096_mod$netsales)

#run diagnostics/plot the predicted vs. actual
library(car)
#non-constancy of variance
ncvTest(fit)
vif(fit) #cautious
vif(step_fit)
crPlots(fit)
ceresPlots(fit)
durbinWatsonTest(fit,max.lag=5) #failed
qqPlot(fit, main="QQ Plot")
acf(resid(fit))
fit <- gls(netsales ~ PDint + seasonal + PDint:OPint, data = data_513096_mod)
ac_fit1 <- gls(netsales ~ PDint + seasonal + PDint:OPint, data = data_513096_mod)
ac_fit1.1 <- update(ac_fit1, correlation=corARMA(p=1))
ac_fit1.2 <- update(ac_fit1, correlation=corARMA(p=2))
ac_fit1.3 <- update(ac_fit1, correlation=corARMA(p=3))
ac_fit1.4 <- update(ac_fit1, correlation=corARMA(q=1))
ac_fit1.5 <- update(ac_fit1, correlation=corARMA(q=2))
ac_fit1.6 <- update(ac_fit1, correlation=corARMA(q=3))
ac_fit1.7 <- update(ac_fit1, correlation=corARMA(p=1,q=1))
ac_fit1.8 <- update(ac_fit1, correlation=corARMA(p=1,q=2))
ac_fit1.9 <- update(ac_fit1, correlation=corARMA(p=2,q=1))

ac_fit1 <- update(ac_fit1, correlation=NULL)

anova(ac_fit1,ac_fit1.1)
anova(ac_fit1,ac_fit1.2)
anova(ac_fit1,ac_fit1.3)
anova(ac_fit1,ac_fit1.4)
anova(ac_fit1,ac_fit1.5)
anova(ac_fit1,ac_fit1.6)
anova(ac_fit1,ac_fit1.7)
anova(ac_fit1,ac_fit1.8)
anova(ac_fit1,ac_fit1.9)

#by AIC and principles of parsimony
ac_fit <- ac_fit1.1
summary(ac_fit)
plot(predict(ac_fit, data_513096_mod),residuals(ac_fit))
abline(h=0)
plot(predict(ac_fit, data_513096_mod),data_513096_mod$netsales)


data_781454 <- smd[which(smd$productkey == 781454),]

sales_peaks <- sqldf("select WeekNum, sum(netsales) as sales from data_781454 group by WeekNum order by sum(netsales)")
sales_peaks
barplot(sales_peaks$sales)
data_781454$seasonal <- ifelse(data_781454$WeekNum %in% c(52,1),1,0)

#delete the first 3 ID columns
drops <- c('WeekNum','productkey','Year','grosssales', 'unitvolumn','X.SalesSpike','X.VolumeSpike')
data_781454_mod <- data.frame(data_781454[,!(names(data_781454) %in% drops)])
cor(data_781454_mod)
data_781454_mod$PDint <- data_781454_mod$PD - mean(data_781454_mod$PD)
data_781454_mod$CCint <- data_781454_mod$CC - mean(data_781454_mod$CC)
data_781454_mod$BMSMint <- data_781454_mod$BMSM - mean(data_781454_mod$BMSM)
data_781454_mod$OPint <- data_781454_mod$OriginalPrice - mean(data_781454_mod$OriginalPrice)


#build the Regression model
fit <- lm(netsales ~ PD+CC+BMSM+OriginalPrice+seasonal,data_781454_mod)
step_fit <- step(fit,k=log(nrow(data_781454_mod)), direction = c("both"))
fit2 <- lm(netsales ~ (PDint+CCint+BMSMint+OPint)*seasonal,data_781454_mod)
step_fit2 <- step(fit2,k=log(nrow(data_781454_mod)), direction = c("both"))
fit3 <- lm(netsales ~ (PDint+CCint+BMSMint)*OPint+(PDint+CCint+BMSMint)*seasonal+seasonal*OPint,data_781454_mod)
step_fit3 <- step(fit3,k=log(nrow(data_781454_mod)), direction = c("both"))
anova(fit3,step_fit2)

summary(step_fit)
summary(step_fit2)
summary(step_fit3)

BIC(step_fit)
BIC(step_fit2)
BIC(step_fit3)

fit <- step_fit2

#check the final model
plot(predict(fit, data_781454_mod),fit$res)
plot(predict(fit, data_781454_mod),data_781454_mod$netsales)
#run diagnostics/plot the predicted vs. actual
library(car)
#non-constancy of variance
ncvTest(fit) #passed
vif(fit)
vif(step_fit)
crPlots(fit)
ceresPlots(fit)
durbinWatsonTest(fit, max.lag=5) #failed
qqPlot(fit, main="QQ Plot")
acf(resid(fit))
acf(resid(fit),type="partial")

fit <- gls(netsales ~ PDint + OPint + seasonal + OPint:seasonal ,data_781454_mod)
ac_fit1 <- gls(netsales ~ PDint + OPint + seasonal + OPint:seasonal ,correlation = corAR1(form=~1),data_781454_mod)
ac_fit1.1 <- update(ac_fit1, correlation=corARMA(p=1))
ac_fit1.2 <- update(ac_fit1, correlation=corARMA(p=2))
ac_fit1.3 <- update(ac_fit1, correlation=corARMA(p=3))
ac_fit1.4 <- update(ac_fit1, correlation=corARMA(q=1))
ac_fit1.5 <- update(ac_fit1, correlation=corARMA(q=2))
ac_fit1.6 <- update(ac_fit1, correlation=corARMA(q=3))
ac_fit1.7 <- update(ac_fit1, correlation=corARMA(p=1,q=1))
ac_fit1.8 <- update(ac_fit1, correlation=corARMA(p=1,q=2))
ac_fit1.9 <- update(ac_fit1, correlation=corARMA(p=2,q=1))

ac_fit1 <- update(ac_fit1, correlation=NULL)

anova(ac_fit1,ac_fit1.1)
anova(ac_fit1,ac_fit1.2)
anova(ac_fit1,ac_fit1.3)
anova(ac_fit1,ac_fit1.4)
anova(ac_fit1,ac_fit1.5)
anova(ac_fit1,ac_fit1.6)
anova(ac_fit1,ac_fit1.7)
anova(ac_fit1,ac_fit1.8)
anova(ac_fit1,ac_fit1.9)

ac_fit <- ac_fit1.3
anova(fit,ac_fit)
summary(ac_fit)
plot(predict(ac_fit, data_781454_mod),residuals(ac_fit))
abline(h=0)
plot(predict(ac_fit, data_781454_mod),data_781454_mod$netsales)


data_894633 <- smd[which(smd$productkey == 894633),]

sales_peaks <- sqldf("select WeekNum, sum(netsales) as sales from data_894633 group by WeekNum order by sum(netsales)")
sales_peaks
barplot(sales_peaks$sales)
data_894633$seasonal <- ifelse(data_894633$WeekNum %in% c(seq(1,4),seq(49,52)),1,0)

#delete the first 3 ID columns
drops <- c('WeekNum','productkey','Year','grosssales', 'unitvolumn','X.SalesSpike','X.VolumeSpike')
data_894633_mod <- data.frame(data_894633[,!(names(data_894633) %in% drops)])
cor(data_894633_mod)
data_894633_mod$PDint <- data_894633_mod$PD - mean(data_894633_mod$PD)
data_894633_mod$OPint <- data_894633_mod$OriginalPrice - mean(data_894633_mod$OriginalPrice)

#build the Regression model
fit <- lm(netsales ~ PD+OriginalPrice+seasonal,data_894633_mod)
step_fit <- step(fit,k=log(nrow(data_894633_mod)), direction = c("both"))
fit2 <- lm(netsales ~ (PDint+OPint)*seasonal,data_894633_mod)
step_fit2 <- step(fit2,k=log(nrow(data_894633_mod)), direction = c("both"))
fit3 <- lm(netsales ~ PDint*OPint+PDint*seasonal+seasonal*OPint,data_894633_mod)
step_fit3 <- step(fit3,k=log(nrow(data_894633_mod)), direction = c("both"))
anova(fit3,step_fit2)
fit <- step_fit
#keep deleting the unnecessary columns
summary(fit)
summary(fit3)
#check the final model
plot(predict(fit, data_894633_mod),fit$res)
plot(predict(fit, data_894633_mod),data_894633_mod$netsales)
#run diagnostics/plot the predicted vs. actual
library(car)
#non-constancy of variance
ncvTest(fit)
vif(fit)
vif(step_fit)
crPlots(fit)
ceresPlots(fit)
durbinWatsonTest(fit,max.lag=5)
qqPlot(fit, main="QQ Plot")

acf(resid(fit))

fit <- gls(netsales ~ PDint + OPint + seasonal ,data_894633_mod)
ac_fit1 <- gls(netsales ~ PDint + OPint + seasonal ,correlation = corAR1(form=~1),data_894633_mod)
ac_fit1.1 <- update(ac_fit1, correlation=corARMA(p=1))
ac_fit1.2 <- update(ac_fit1, correlation=corARMA(p=2))
ac_fit1.3 <- update(ac_fit1, correlation=corARMA(p=3))
ac_fit1.4 <- update(ac_fit1, correlation=corARMA(p=1,q=1))
ac_fit1.5 <- update(ac_fit1, correlation=corARMA(p=1,q=2))
ac_fit1.6 <- update(ac_fit1, correlation=corARMA(p=2,q=1))
ac_fit1.7 <- update(ac_fit1, correlation=corARMA(q=1))
ac_fit1.8 <- update(ac_fit1, correlation=corARMA(q=2))
ac_fit1.9 <- update(ac_fit1, correlation=corARMA(q=3))


ac_fit1 <- update(ac_fit1, correlation=NULL)

anova(ac_fit1,ac_fit1.1)
anova(ac_fit1,ac_fit1.2)
anova(ac_fit1,ac_fit1.3)
anova(ac_fit1,ac_fit1.4)
anova(ac_fit1,ac_fit1.5)
anova(ac_fit1,ac_fit1.6)
anova(ac_fit1,ac_fit1.7)
anova(ac_fit1,ac_fit1.8)
anova(ac_fit1,ac_fit1.9)

ac_fit <- ac_fit1.1
anova(fit,ac_fit)

summary(ac_fit)

plot(predict(ac_fit, data_894633_mod),residuals(ac_fit))
abline(h=0)
plot(predict(ac_fit, data_894633_mod),data_894633_mod$netsales)


# distribution of studentized residuals

sresid <- studres(fit) 
hist(sresid, freq=FALSE, 
     main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit)
#fill in the results for the final simple model


factor_cols <- c('WeekNum','Year','productkey')
cols = c(1, 2, 3)    
data_371792[,cols] = apply(data_371792[,cols], 2, function(x) as.factor(x))
cor_data <- data_371792[,c(4,5,6,10,11)]
cor_ <- cor(cor_data[lapply(cor_data, sd)>0])
sapply(data_371792, is.numeric)

full_data <- sqldf("select a.*,
                   case when b.Offer is NULL then 'RegRetail' else b.Offer end as Offer,
                   cast(trim(case when instr(b.Offer,'PD') = 0 or b.Offer is NULL then '' else substr(b.Offer,instr(b.Offer,'PD')-6,5) end,'$') as REAL) as PD,
                   cast(trim(case when instr(b.Offer,'CC') = 0 or b.Offer is NULL then '' else substr(b.Offer,instr(b.Offer,'CC')-6,5) end,'$') as REAL) as CC,
                   cast(trim(case when instr(b.Offer,'ER') = 0 or b.Offer is NULL then '' else substr(b.Offer,instr(b.Offer,'ER')-6,5) end,'$') as REAL) as ER,
                   case when instr(b.Offer,'BMSM') = 0 or b.Offer is NULL then 0.0 else substr(b.Offer,17,4) end as BMSM,
                   case when substr(b.Offer,1,3) = 'Buy' then substr(b.Offer,1,11) else 'NA' end as BuyFlag,
                   case when b.RegRetail is NULL then 0.0 else b.RegRetail end as RegRetail,
                   case when b.AdRetail is NULL then 0.0 else b.AdRetail end as AdRetail
                   case when b.AdRetail is NULL then 0.0 else b.AdRetail end as AdRetail
                   from allsales a left join adcir b 
                   on a.productkey = b.productkey and a.startdate = b.startdate")


