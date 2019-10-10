


##### LIBRARY & IMPORT  ############################

library(dplyr)
library(tidyverse)
library(car)
library (leaps)
library(MASS)
library(gvlma)

NBA<-read_csv("C:/Users/josec/Dropbox/_CUNEF/Prediccion/nba.csv")

summary(duplicated(NBA$Player))

NBAi<-which(duplicated(NBA$Player))

NBA[NBAi[1],]
NBA[NBAi[2],]

rm(NBAi)

##### ELIMINAR DUPLICADO ####################

NBA_new <- NBA[-226,]

attach(NBA_new)
regfit.full=regsubsets(Salary~.-Player, NBA_new )
reg.summary=summary(regfit.full)
reg.summary

names(NBA_new)


regfit.fwd=regsubsets(Salary~.-Player-NBA_Country-NBA_DraftNumber-Tm,NBA_new,method ="forward")
summary (regfit.fwd)

regfit.bwd=regsubsets(Salary~.-Player-NBA_Country-NBA_DraftNumber-Tm,NBA_new,method ="backward")
summary (regfit.bwd)

NBA_new<-rename(NBA_new,"USG"="USG%", "Par"="3PAr")

regfit.bwd.int=regsubsets(Salary~.-Player-NBA_Country-NBA_DraftNumber-Tm,NBA_new,method ="backward", intercept = FALSE)
summary (regfit.bwd.int)

regfit.fwd.int=regsubsets(Salary~.-Player-NBA_Country-NBA_DraftNumber-Tm,NBA_new,method ="forward", intercept = FALSE)
summary (regfit.fwd.int)

detach(NBA_new)
########################

NBA1<-dplyr::select(NBA_new,"Player","Salary","Age","G","MP","USG","WS","Par")

summary(NBA1)

which(is.na(NBA1$Par))

NBA1[30,]
NBA1[38,]


NBA1par<-NBA1[-30,]
NBA1par<-NBA1par[-37,]

NBA1par[37,]


###############################

summary(NBA1par)

regres01par1 <- lm(Salary~Age+G+MP+USG+WS+Par,data = NBA1par) 
summary(regres01par1)

regres01par2 <- lm(Salary~Age+G+MP+USG+WS,data = NBA1par) 
summary(regres01par2)


regres02 <- lm(Salary~Age+G+MP+USG+WS,data = NBA1) 
summary(regres02)

regres03 <- lm(Salary~Age+G+MP+WS,data=NBA1) 
summary(regres03)

regres04<-lm(Salary~Age+MP+WS,data=NBA1) 
summary(regres04)

regres05<-lm(Salary~Age+G+WS,data=NBA1) 
summary(regres05)


####################################### 


qqPlot(regres01par1, labels=row.names(mData), id.method="identify",
       simulate=TRUE, main="Q-Q Plot")


qqPlot(regres01par2, labels=row.names(mData), id.method="identify",
       simulate=TRUE, main="Q-Q Plot")


qqPlot(regres02, labels=row.names(mData), id.method="identify",
       simulate=TRUE, main="Q-Q Plot")

qqPlot(regres03, labels=row.names(mData), id.method="identify",
       simulate=TRUE, main="Q-Q Plot")

qqPlot(regres04, labels=row.names(mData), id.method="identify",
       simulate=TRUE, main="Q-Q Plot")

qqPlot(regres05, labels=row.names(mData), id.method="identify",
       simulate=TRUE, main="Q-Q Plot")



#########################

residplot <- function(fit, nbreaks=10) {
  z <- rstudent(fit)
  hist(z, breaks=nbreaks, freq=FALSE,
       xlab="Studentized Residual",
       main="Distribution of Errors")
  rug(jitter(z), col="brown")
  curve(dnorm(x, mean=mean(z), sd=sd(z)),
        add=TRUE, col="blue", lwd=2)
  lines(density(z)$x, density(z)$y,
        col="red", lwd=2, lty=2)
  legend("topright",
         legend = c( "Normal Curve", "Kernel Density Curve"),
         lty=1:2, col=c("blue","red"), cex=.7)
}

residplot(regres01par1)

residplot(regres01par2)

residplot(regres02)

residplot(regres03)

residplot(regres04)

residplot(regres05)

#######################

vResid01par1=resid(regres01par1)
vResid01par2=resid(regres01par2)
vResid02=resid(regres02)
vResid03=resid(regres03)
vResid04=resid(regres04)
vResid05=resid(regres05)


shapiro.test(vResid01par1) # 1.484e-08 no es normal 
shapiro.test(vResid01par2) # 1.225e-08  no es normal 
shapiro.test(vResid02)     # 1.027e-08  no es normal 
shapiro.test(vResid03)     # 5.12e-09  no es normal
shapiro.test(vResid04)     # 1.31e-11 no es normal 
shapiro.test(vResid05)     # 1.439e-11 no es normal 

##############################


crPlots(regres01par1)

crPlots(regres02)
crPlots(regres03)
crPlots(regres04)
crPlots(regres05)

#######################

ncvTest(regres01par1)
spreadLevelPlot(regres01par1)


ncvTest(regres01par2)
spreadLevelPlot(regres01par2)

ncvTest(regres02)
spreadLevelPlot(regres02)

ncvTest(regres03)
spreadLevelPlot(regres03)

ncvTest(regres04)
spreadLevelPlot(regres04)

# todas tienen evidencia de tener varianzas heteroskedasticas

#######################


gvmodel <- gvlma(regres01par1) 
summary(gvmodel)


gvmodel <- gvlma(regres04) 
summary(gvmodel)

###########################

vif(regres01par1) 
sqrt(vif(regres01par1)) > 2  ### suguire Multicolinealidad en G y MP - logico porque a mayores juegos, mas tiempo

vif(regres01par2) 
sqrt(vif(regres01par2))>2


vif(regres02) 
sqrt(vif(regres02))>2


vif(regres03) 
sqrt(vif(regres03))>2


vif(regres04) 
sqrt(vif(regres04))>2 # eliminando G no tenemos ninguna Multicolinealidad, sin ambargo hay cercania entre G y WS

vif(regres05) 
sqrt(vif(regres05))>2 # Cambiando la eliminacion a MP, la Multicolinealidad residual es menor 




###########################

outlierTest(regres01par1)




###########################


AIC(regres05,regres04,regres03,regres02)   #aqui regres02 es menor
BIC(regres05,regres04,regres03,regres02)  # aqui regres03 es menor

anova(regres03,regres02) # con p 0.04545 podemos rechazar el modelo regres03 y utilizamos el regres02

summary(regres02)

### Creo un nuevo modelo con regres02 pero eliminando MP, para la Multicolinealidad menor 

regres06 <- lm(Salary~Age+G+USG+WS,data = NBA1) 
summary(regres06)

vif(regres06) 
sqrt(vif(regres06))>2

##########################################################

outlierTest(regres06)  #327 outlier

hat.plot <- function(fit) {
  p <- length(coefficients(fit))
  n <- length(fitted(fit))
  plot(hatvalues(fit), main="Index Plot of Hat Values")
  abline(h=c(2,3)*p/n, col="red", lty=2)
  identify(1:n, hatvalues(fit), names(hatvalues(fit)))
}
#hat.plot(regres02)



cutoff <- 4/(nrow(mData)-length(regres06$coefficients) - 2)
plot(regres06, which=4, cook.levels=cutoff)
abline(h=cutoff, lty=2, col="red")

#70 , 227, 327, 344

#####

avPlots(regres06, ask=FALSE, id.method="identify")

#### influencia 


influencePlot(regres06, id.method="identify", main="Influence Plot", 
              sub="Circle size is proportial to Cook's Distance" )

NBA[228,]

##################




library(ISLR)
set.seed(250)
numData <- nrow(NBA1)
train <- sample(numData ,numData/2)

regres.train <- lm(Salary~Age+G+USG+WS,data = NBA1, subset =train)
attach(NBA1)
mean((Salary - predict(regres.train , Auto))[-train ]^2)



