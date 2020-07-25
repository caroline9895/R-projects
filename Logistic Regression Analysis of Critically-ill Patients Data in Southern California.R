# Once hospital received a critically ill patient, doctors would always give an emergency treatment to alleviate the patient’s suffering and save his/her life after getting a series of measurement results, like: the gender, age, shock type, systolic Pressure, diastolic pressure...etc. It is believed that those initial measurements help doctors give the optimal medical care. The question proposed here is: is there any relationships between the patient’s surviving rate and those initial measurement results?

# Physiological data of 112 critically ill patients were collected in Southern California. Each patient has 2 records. Record 1 contains SURVIVE and initial measurements. Record 2 contains SURVIVE and final measurements, which is just before death or discharge. Each record contains 21 variables. The variables are listed in Table 1A in appendices.

# The main goal of this study is to find out the relationships between the patient’s surviving rate and those initial measurements data. Consequently, this study will focus on the Record 1, which is the initial measures dataset of the critically ill patients. Commercial oftware R 3.6.2 is utilized to handle those data.


# Load libraries
library("readxl")
library("corrplot")
library("MASS")
library("binom")

############################
## Get the target dataset ##
############################

# Load in data and take initial looks
DATAsp2020=read_excel("DATA-FILEsp2020.xlsx",col_names=as.character(1:21))
dim(DATAsp2020)        #[1] 224  21
head(DATAsp2020)       
sum(is.na(DATAsp2020)) #[1] 0: no missing data

# Adjust the data
ShockData=DATAsp2020[seq(1,224,2),1:20]
ShockData=as.data.frame(ShockData)
dim(ShockData)  #[1] 112  20
names(ShockData)=c("ID", "Age", "HT", "Sex", "Survive", "Shock_TYP", "SBP", "MAP", "HR","DBP", "MCVP", "BSI", "CI", "AT", "MCT","UO", "PVI", "RCI", "HG", "HCT")
ShockData$Sex[ShockData$Sex==2]=0 # 1=Male; 0=Female
ShockData$Survive[ShockData$Survive==3]=0  # 1=Survive; 0=Died

###############################
## Exploratory Data Analysis ##
###############################

# Inital look
summary(ShockData) # only Sex/Survive/Shock_TYP are categorical data

table(ShockData$Survive) # Died=43; Survived=69

# Summary statistics
Sn=c()
Smin=c()
Smax=c()
Smean=c()
Sstd=c()
for (i in 2:ncol(ShockData))
{
  Sn=c(Sn, length(ShockData[,i]))
  Smin=c(Smin, min(ShockData[,i]))
  Smax=c(Smax,max(ShockData[,i]))
  Smean=c(Smean, mean(ShockData[,i]))
  Sstd=c(Sstd, sd(ShockData[,i]))
}
Ssummary=cbind(names(ShockData)[2:ncol(ShockData)],Sn,Smin,Smax, round(Smean,3),round(Sstd,3))

###############################
##   Statistical Analyses    ##
###############################

############## Correlation plot
SDcor = cor(ShockData)
corrplot(SDcor,method="number") #Survive~Shock_TYP/SBP/MAP/DBP

# Survive with categorical variable
ShockData$Sex=as.factor(ShockData$Sex)
table(ShockData$Sex, ShockData$Survive)

ShockData$Shock_TYP=as.factor(ShockData$Shock_TYP)
table(ShockData$Shock_TYP, ShockData$Survive)

boxplot(HT~Sex, data=ShockData, ylim=c(120,200), xlab ="Sex", ylab="Height", names.arg=c("Female","Male"))

###################### Build a Model
# Detect the main effects
fit.all=glm(Survive ~., family=binomial(link=logit), data=ShockData[,2:20])
summary(fit.all)    #significant:Shock_TYP/MCVP/BSI/UO/PVI
stepAIC(fit.all)

# Detect the interaction
fit_1=glm(formula = Survive ~ Age+HT+Sex+Shock_TYP+SBP+MAP+HR+DBP+MCVP+BSI+CI+AT+MCT+UO+PVI+RCI+HG+HCT+Shock_TYP:SBP+BSI:HT+BSI:Sex+UO:Age+PVI:HG+PVI:HCT,family = binomial(link = logit), data = ShockData[, 2:20])
summary(fit_1)  
stepAIC(fit_1)

fit_2=glm(formula = Survive ~ HT + Sex + Shock_TYP + SBP + MAP + MCVP + BSI + UO + PVI + Shock_TYP:SBP + HT:BSI + Sex:BSI, family = binomial(link = logit), data = ShockData[, 2:20])
summary(fit_2) #significant: HT,Sex,MAP,MCVP,PVI,HT:BSI, Sex:BSI

fit_3=glm(formula = Survive ~ HT + Sex + MAP+ MCVP + BSI+ HT:BSI + Sex:BSI, family = binomial(link = logit), data = ShockData[, 2:20])
summary(fit_3) #significant: HT,MAP,MCVP,PVI,HT:BSI

fit_4=glm(formula = Survive ~ HT + MAP+ MCVP + BSI+ HT:BSI, family = binomial(link = logit), data = ShockData[, 2:20])
summary(fit_4)       #significant: HT,MAP,MCVP,PVI,HT:BSI

#survive is highly depended on Shouck_TYP
fit_5=glm(formula = Survive ~ HT+Shock_TYP+ MAP + MCVP + BSI + HT:BSI, family = binomial(link = logit),data = ShockData[,2:20])
summary(fit_5)  #significant: HT+Shock_TYP+ MAP + MCVP + BSI + HT:BS

# Final model
#Function to compute AIC, AICc, and BIC
AICc=function(object){
  n=length(object$y)
  r=length(object$coef)
  AICc=AIC(object)+2*r*(r+1)/(n-r-1)
  list(AIC=AIC(object), AICc=AICc, BIC=BIC(object))
}               

M1=AICc(fit.all)
M2=AICc(fit_2)
M3=AICc(fit_3)
M4=AICc(fit_4) 
M5=AICc(fit_5) #best
M=rbind(M1,M2,M3,M4,M5)

final.fit=fit_5

############################ Diagnostic plots
one.fourth.root=function(x){
  x^0.25
}


# Create EVPs by binning continuous covariates
#g = 4 # number of categories
#HT_interval = cut(ShockData$HT, quantile(ShockData$HT, 0:g/g), include.lowest = TRUE) 
#MAP_interval = cut(ShockData$MAP, quantile(ShockData$MAP, 0:g/g), include.lowest = TRUE)
#MCVP_interval = cut(ShockData$MCVP, quantile(ShockData$MCVP, 0:g/g), include.lowest = TRUE)
#BSI_interval=cut(ShockData$BSI, quantile(ShockData$BSI, 0:g/g), include.lowest = TRUE)


source(file="Examine.logistic.reg.R")
w = aggregate(formula = Survive ~ HT+Shock_TYP+ MAP + MCVP + BSI, data = ShockData[, 2:20], FUN = sum)
n = aggregate(formula = Survive ~ HT+Shock_TYP+ MAP + MCVP + BSI, data = ShockData[, 2:20], FUN = length)
w.n = data.frame(w, trials = n$Survive, prop = round(w$Survive/n$Survive,2))
mod.prelim1 = glm(formula = Survive/trials ~ HT + Shock_TYP+ MAP + MCVP + BSI + HT:BSI, family = binomial(link = logit), data = w.n, weights = trials)
save1=examine.logistic.reg(mod.prelim1, identify.points=TRUE, scale.n=one.fourth.root, scale.cookd=sqrt)

#Evaluation of EVPs for potential outlying sets of points
w.n.diag1=data.frame(w.n, pi.hat=round(save1$pi.hat, 2), std.res=round(save1$stand.resid, 2), cookd=round(save1$cookd, 2), h=round(save1$h, 2))
p=length(mod.prelim1$coef) # number of parameters in model (# coefficients)
ck.out=abs(w.n.diag1$std.res)>2 | w.n.diag1$cookd>4/nrow(w.n) | w.n.diag1$h > 3*p/nrow(w.n)
extract.EVPs=w.n.diag1[ck.out, ]
extract.EVPs

#51
b=ShockData[ShockData$HT==161&ShockData$Shock_TYP==6&ShockData$MCVP==129,] ##outlier:ID=653

########################### fit of the model
#Hosmer and Lemeshow goodness-of-fit test
source("HLtest.R")
HL = HLTest(mod.prelim1, 5)  # 10 groups by default
# HL test output: Y0 are Died, Y1 are Survived
cbind(HL$observed, round(HL$expect, digits=1)) # Observed and Expected table as illustration
HL # HL test results

########################## Inference table for logistic regression
betahat = formatC(signif(final.fit$coeff,digits=3), digits=2, format="f", flag="#")
OR = formatC(signif(exp(final.fit$coeff),digits=3), digits=2, format="f", flag="#")
SE = formatC(signif(summary(final.fit)$coeff[,2],digits=3), digits=2, format="f", flag="#") 
cibounds = formatC(signif(exp(confint(final.fit)),digits=3), digits=2, format="f", flag="#") 
pval = formatC(signif(summary(final.fit)$coeff[,4],digits=4), digits=4, format="f", flag="#")
# create the table matrix
x = cbind(betahat, OR, SE, pval, matrix(paste("(", cibounds[,1], ",", cibounds[,2], ")")))
# create informative column names
colnames(x) = rbind("Coefficient", "Odds ratio", "SE", "p-value", "95% CI on OR")
# create informative row names
rownames(x) = cbind("Intercept", "HT","Shock_TYP3", "Shock_TYP4", "Shock_TYP5", "Shock_TYP6",
                    "Shock_TYP7","MAP","MCVP", "BSI","HT:BSI")


