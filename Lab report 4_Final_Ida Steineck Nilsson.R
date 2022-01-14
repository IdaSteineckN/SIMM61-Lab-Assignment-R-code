#######INSTALLIGN AND LOADING PACKAGES#######
install.packages("semPlot")
install.packages("semptools")
install.packages("CompQuadForm")
install.packages("psychTools")

library(lavaan)
library(semPlot)
library(semptools)
library(tidyverse)
library(CompQuadForm)
library(ICS)
library(psychTools)

#########LOADING THE DATA########

Holz_swin <- holzinger.swineford
str(Holz_swin)

#########Task 1: Model A######## 
#Specify a structural equation model matching the theoretical factor structure
#Visual perception ability = latent (unobserveable) factor that casually influences test scores 
#Test scores = t01_visperc, t02_cubes, t03_frmbord, t04-lozenges
#Verbal ability = latent factor that casually influences test scores 
#Test scores: t10_addition t12_countdot, and t13_sccaps.
#Processing speed = latent factors that casually influences test scores
#Test scores measured by t10_addition, t12_countdot, and t13_sccaps
#latent factors allowed to correlate w each other. 

#Assessing normality

mvnorm.kur.test(Holz_swin[,c("t01_visperc", "t02_cubes", "t03_frmbord", "t04_lozenges", 
                             "t10_addition", "t12_countdot", "t13_sccaps",
                             "t06_paracomp", "t07_sentcomp", "t09_wordmean")])

mvnorm.skew.test(Holz_swin[,c("t01_visperc", "t02_cubes", "t03_frmbord", "t04_lozenges", 
                              "t10_addition", "t12_countdot", "t13_sccaps",
                              "t06_paracomp", "t07_sentcomp", "t09_wordmean")]) #Data is non-nomral --> normality-adjusted robust standard errors will have to be used + correction to the model fit indices.


#Specifying the model. Measurement component. 


ModelA <- '

VisPer =~ t01_visperc + t02_cubes + t03_frmbord + t04_lozenges

VerbAbil =~ t06_paracomp + t07_sentcomp + t09_wordmean

ProcSpeed =~ t10_addition + t12_countdot + t13_sccaps

'

#Then fit the model to the dataset.


fit_modelA <- sem(ModelA, data = Holz_swin, estimator = "MLM") #fitting the model

plot <- semPaths(fit_modelA, nCharNodes = 8, label.scale = F, fixedStyle = 1,
                 sizeMan2=5, sizeMan=10, asize=3, residuals = T)


summary(fit_modelA, fit.measures = T)  #Checking coefficients and fit statistics


####TASK 2: MODEL B########

ModelB <- '

VisPer =~ t01_visperc + t02_cubes + t03_frmbord + t04_lozenges

VerbAbil =~ t06_paracomp + t07_sentcomp + t09_wordmean

ProcSpeed =~ t10_addition + t12_countdot + t13_sccaps

t10_addition ~~ t12_countdot

'

fit_modelB <- sem(ModelB, data = Holz_swin, estimator = "MLM") #fitting the model

plot <- semPaths(fit_modelB, nCharNodes = 8, label.scale = F, fixedStyle = 1,
                 sizeMan2=5, sizeMan=10, asize=3, residuals = F)

#Unstandardized coefficients and AIC
summary(fit_modelB, fit.measures = T)

anova(fit_modelA, fit_modelB)   #comparing A and B


#Changing the reference parameter

ModelB2 <- '

VisPer =~  t03_frmbord + t02_cubes + t01_visperc + t04_lozenges

VerbAbil =~ t06_paracomp + t07_sentcomp + t09_wordmean

ProcSpeed =~ t10_addition + t12_countdot + t13_sccaps

t10_addition ~~ t12_countdot

'

fit_modelB2 <- sem(ModelB2, data = Holz_swin, estimator = "MLM") #fitting the model

summary(fit_modelB2, fit.measures = T)  #Cubes remains as the smallest one. It has an AIC of 8267.652

#Standardized estimates

standardizedsolution(fit_modelB, type = "std.all")

#The factor loading of Visual perception ability is weakest for t02_cubes. 

semPaths(fit_modelB, whatLabels = "std")

summary(fit_modelB, standardized = T, rsquare = T)

#Getting the R2 values

parameterEstimates(fit_modelB, standardized=TRUE, rsquare = TRUE) %>% 
  filter(op == "r2") %>% 
  select(Item=rhs, R2 = est)
