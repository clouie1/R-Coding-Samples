############################################
# Econ 217 Homework 3 R Code
# By Christina Louie
# February 19, 2017
############################################

#====================
#==== Problem 1 =====
#====================

#read dataset
library(foreign)
g <-read.dta("C:/Users/ChristinaL/Documents/Econ 217/Data/gehan.dta")

# -- PART A --
haz_glm <-glm(relapse~group+offset(log(weeks)),family=poisson("log"),data=g)
summary(haz_glm)

# -- PART B --
for (i in 1:nrow(g)){
  gdrop <- g[i,]
  gkeep <- g[-i,]
  haz_glm_fit <-glm(relapse~group+offset(log(weeks)),family=poisson("log"),data=gkeep)
  intercept <- as.numeric(coef(haz_glm_fit)[1])
  group_treat <- as.numeric(coef(haz_glm_fit)[2])
  if(i==1){results<-data.frame(i,intercept,group_treat)}
  if(i>1){results<-rbind(results,data.frame(i,intercept,group_treat))}
}

results
plot(density(results$intercept),main="Distribution of Intercept")
plot(density(results$group_treat),main="Distribution of Coefficients of Group")

# -- PART C --
gamresults <-gam(relapse~s(weeks)+group,data=g)
summary(gamresults)
plot(gamresults,se=TRUE,rug=FALSE,term="s",main="Plot of Gam Results")
abline(v=0)
abline(h=0)

#====================
#==== Problem 2 =====
#====================

#read-in data
wagedata <-read.csv(file="C:/Users/ChristinaL/Documents/Econ 217/Data/WageTimeSeries.csv")

# -- PART A --
fit.loess1 <-loess(wagedata$realwage ~ wagedata$month,span=1,degree=2,family="gaussian")
plot(wagedata$month,predict(fit.loess1),pch=20,lwd=1.5, main="Relationship between Real Wage and Month", 
     xlab="Month",ylab="Predicted Real Wage")

# -- PART B --
library(gam)
gam_wage <-gam(realwage~s(year)+s(month),data=wagedata,family="gaussian")
summary(gam_wage)
par(mfrow=c(1,2)) 
plot(gam_wage,se=TRUE,rug=FALSE,term="s(year)",main="Plot of Smooth Term s(year)")
#abline(v=0)
abline(h=0)
plot(gam_wage,se=TRUE,rug=FALSE,term="s(month)",main="Plot of Smooth Term s(month)")
#abline(v=0)
abline(h=0)

# -- PART C --
for (h in 1:20){
  for (i in 1:nrow(wagedata)){
    rwdrop <- wagedata[i,]
    rwkeep <- wagedata[-i,]
    fit.loess <-loess(realwage~month,data=rwkeep,span=(h/20),degree=2,family="gaussian")
    dropfit.loess <- predict(fit.loess,rwdrop,se=FALSE)
    sqrerr <-(rwdrop$realwage - as.numeric(dropfit.loess))^2
    if (i*h==1){results <-data.frame(h,i,sqrerr)}
    if (i*h>1){results <-rbind(results,data.frame(h,i,sqrerr))}
  }
}
results
tapply(results$sqrerr,results$h,FUN=sum,na.rm=TRUE)

#plot of optimal
fit.loess3 <-loess(wagedata$realwage ~ wagedata$month,span=(19/20),degree=2,family="gaussian")
plot(wagedata$month,predict(fit.loess3),pch=20,lwd=1.5, main="Optimal Plot under Cross-Validation Procedure", 
     xlab="Month",ylab="Predicted Real Wage")


#====================
#==== Problem 3 =====
#====================

#read data
library(foreign)
orgdata <-read.dta("C:/Users/ChristinaL/Documents/Econ 217/Data/org_example.dta")

# -- PART A --
subd <- subset(orgdata, (state=="CA"|state=="NV")&(year==2008|year==2013)) 

ca <- as.numeric(subd$state=="CA")
yr13 <- as.numeric(subd$year=="2013")

reg <- lm(log(rw)~ca+yr13+I(ca*yr13)+educ+age,subd)
summary(reg)

print(confint(reg,4,level=.95))


# -- PART B --
randomSample = function(df,n) {
  return (df[sample(nrow(df),n, replace=TRUE),])
}

form <- as.formula(log(rw)~ca+yr13+I(ca*yr13)+educ+age)
fit.full <- lm(form,subd)
B<-1000
N<-nrow(subd)
resultsB <-matrix(NA,nrow=B,ncol=length(coef(fit.full)+1))
for (iter in 1:B){
  fit.B <- lm(form,randomSample(subd,N))
  coef.B <- as.numeric(coef(fit.B))
  result.coef <- data.frame(iter,t(as.matrix(coef.B)))
  if (iter==1){results <- result.coef}
  if (iter>1){results <-rbind(results,result.coef)}
}

names(results) <-c("rep","Intercept","CA","Yr13","CA_Yr13","educHS","educSomeColl","educColl","educAdv","Age")
results

quantile(results$CA_Yr13,prob=c(0.025,0.975),na.rm=TRUE)


# -- PART C --
subpre <- subset(orgdata, (state=="CA"|state=="NV")&(year==2003|year==2008)) 

ca <- as.numeric(subpre$state=="CA")
yr08 <- as.numeric(subpre$year=="2008")

reg_pre <- lm(log(rw)~ca+yr08+I(ca*yr08)+educ+age,subpre)
summary(reg_pre)

resid.full <-as.numeric(reg_pre$residuals)
predict.full<-as.numeric(reg_pre$fitted.values)

B<-1000
for (rep in 1:B){
  rand.resid <- sample(resid.full,nrow(subpre),replace=TRUE)
  subpre$rw_boot <-predict.full+rand.resid
  fit.B <-lm(rw_boot~ca+yr08+I(ca*yr08)+educ+age,subpre)
  coef.B <- as.numeric(coef(fit.B))
  result.coef <- data.frame(rep,t(as.matrix(coef.B)))
  if (rep==1){results <- result.coef}
  if (rep>1){results <-rbind(results,result.coef)}
}

names(results) <-c("rep","Intercept","CA","Yr08","CA_Yr08","educHS","educSomeColl","educColl","educAdv","Age")
results

quantile(results$CA_Yr08,prob=c(0.025,0.975),na.rm=TRUE)