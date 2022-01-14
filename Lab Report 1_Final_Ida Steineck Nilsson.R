#########INSTALL AND LOAD PACKAGES#########
install.packages("tidyverse") #for data management
install.packages("psych") #for descriptive statistics
install.packages("gridExtra") #for graphing
install.packages("forcats") #changing the Cabin variable (combining levels) 
install.packages("magrittr")  #for piping
install.packages("compareGroups")
install.packages("pscl") #For McFadden R^2
install.packages("mice") #For missing values examination
install.packages("lmtest") #For likelihood ratio
install.packages("dominanceanalysis") #For the dominance analysis

library(compareGroups)
library(magrittr)
library(tidyverse)	
library(psych)	
library(gridExtra)	
library(forcats) 
library(pscl)
library(mice)
library(dplyr)
library(lmtest)
library(dominanceanalysis)



#########LOAD DATASET#########

Titanic_1 <- read.csv("https://raw.githubusercontent.com/kekecsz/SIMM61-Course-materials/main/Home_assignment/Titanic%20-%20training%20set.csv")


#########INSPECTION OF VARIABLE TYPES AND POSSIBLE DATA ERRORS#########

str(Titanic_1)

#Checking that each ticket ID has only occurred once 

check_duplicates <- data.frame(table(Titanic_1$PassengerId)) 

check_duplicates[check_duplicates$Freq > 1,]

table(duplicated(Titanic_1))

#Assigning correct variable type to each variable

Titanic_2 <- Titanic_1%>%
  mutate(Survived = factor(dplyr::recode(Survived, "0" = "Died", "1" = "Lived")),
         Pclass = as_factor(Pclass),
         Sex = as_factor(Sex),
         Embarked = as_factor(Embarked),
         Cabin = as_factor(Cabin))  %>%
  select(Survived, Age, Pclass, Sex, Embarked, SibSp, Parch, Fare, Cabin)
  
  
#Missing values

md.pattern(Titanic_1, rotate.names = TRUE) #Missing values are located in the "cabin" and "age" category. 

prop.table(table(is.na(Titanic_1$Cabin)))


#Crate new Cabin variable and change it into having two categories: "having a cabin" vs "not having a cabin" 

Titanic_2 %<>% mutate(CabinLump = fct_explicit_na(Cabin, na_level = "No Cabin")) 
  
Titanic_2 <- Titanic_2 %>%   #Then collapse all other levels into one "has a cabin" category. 
    mutate(CabinLump = fct_lump(CabinLump, n = 1, other_level = "Cabin"))
  

#Checking that the variables have no "strange" values in them. 

Titanic_2 %>%    #Checking categorical variables
  select(Survived, Pclass, Sex, SibSp, Parch, CabinLump, Embarked) %>%
  lapply(table)

Titanic_2 %>%   #Checking numeric variables
  ggplot() +
  aes(x = Age)+
  geom_histogram() 

Titanic_2 %>%
  ggplot() +
  aes(x = Fare) +
  geom_histogram()  #If using fare, keep the value of ~500 in mind as it might be an outlier. 


#######EXPLORE FEATURES OF SURVIVED VS SURVIVED PASSANGERS VISUALLY ##########

agegraph <- Titanic_2 %>%   #Seems like age had some impact. Being very young, for example a baby, seems to increase survival chances. 
  ggplot() +
  geom_freqpoly(mapping = aes(x = Age, color = Survived), binwidth = 2) +
  guides(fill = FALSE) +
  theme_classic()

Titanic_2 %>%   #Higher fare ---> more survive (but quite few who paid a lot of money)
  ggplot() +
  geom_freqpoly(mapping = aes(x = Fare, color = Survived), binwidth = 5) +
  guides(fill = FALSE) +
  theme_classic()

classgraph <- Titanic_2 %>%    #Seems like being in Class 3 is bad for survival. 
  ggplot() +
  aes(x = Pclass, fill = Survived) +
  geom_bar(position = "fill") +
  xlab("Ticket Class") +
  ylab("Proportion") +
  theme_classic()


sexgraph <- Titanic_2 %>%   #Seems like being male meant less chance of survival. 
  ggplot() + 
  aes(y = Sex, fill = Survived) +
  geom_bar(position = "fill") +
  coord_flip() +
  xlab("Proportion") + 
  theme_classic()

Titanic_2 %>%     #Those who were accompanied by 0 or 3+ siblings/spouses seem to have been more likely to survive. 
  ggplot() +
  aes(y = SibSp, fill = Survived) +
  geom_bar(position="fill") +
  coord_flip() +
  xlab("Proportion") +
  ylab ("Accompanying Siblings/Spouses") +
  theme_classic()

Titanic_2 %>%     #Those accompanied by 0 parents/children had low survival rate, then it increases and finally decreases with 4+ parents/children. 
  ggplot() +
  aes(y = Parch, fill = Survived) +
  geom_bar(position="fill") +
  coord_flip() +
  xlab("Proportion") +
  ylab ("Accompanying Parents/Children") +
  theme_classic() 


Titanic_2 %>%  #Not having a cabin seems to have negatively impacted survival. 
  ggplot() + 
  aes(y = CabinLump, fill = Survived) +
  geom_bar(position = "fill") +
  theme_classic() +
  coord_flip()


Titanic_2 %>%  #Embarking via Cherbourgh might have an influence.  
  ggplot() + 
  aes(y = Embarked, fill = Survived) +
  geom_bar(position = "fill")  +
  theme_classic() +
  coord_flip()


#Graphs with pclass, age, sex. etc. 

grid.arrange(sexgraph, classgraph, ncol=2)


########SELECT VARIABLES FOR REGRESSSION ANALYSIS##########

Titanic_Model1 <- Titanic_2 %>%
  select(Survived, Age, Pclass, Sex, SibSp, Parch)

table(complete.cases(Titanic_Model1)) #There are 177 observations that are incomplete (due to the Age variable

177/(714+177)

Titanic_Model1 <- na.omit(Titanic_Model1)


########LOGISTIC REGRESSION#######


#Running the regression itself

model1 <- glm(Survived ~ Pclass + Sex + Age + Parch + SibSp,
              family = binomial(), data = Titanic_Model1)

summary(model1)

#Model performance

pR2(model1) #McFadden R^2

pR2(model1)["llh"] * -2 



########PREDICTION ACCURACY#########

#Choosing cut-off point (50%) and saving predicted outcome into new variable

Titanic_Model1 <- Titanic_Model1 %>%    
  mutate(pred_model1 = predict(model1)) %>%
  mutate(pred_model1 = case_when(pred_model1 <= 0 ~ "Died", 
                                pred_model1 > 0 ~ "Lived"))

#Creating another variable showing when Model 1 was correct

Titanic_Model1 <- Titanic_Model1 %>%
  mutate(pred_correct = case_when(pred_model1 == Survived ~ "correct",
                                  pred_model1 != Survived ~ "incorrect"))


#Overall categorization accuracy (%)

Titanic_Model1 %>%   #Overall, 80.5% correctly classified. 
  group_by(pred_correct) %>%
  dplyr::summarise(count = n()) %>%
  mutate(percent = (count/sum(count)*100))


#Comparing Model 1 to a null model to contextualize the cateogrization accuracy

null_model <- glm(Survived ~ 1, family = binomial(), data = Titanic_Model1)

summary(null_model)

print(predict(null_model)) #predicts the more common category for all outcomes

prop.table(table(Titanic_Model1$Survived)) #Null correct 60% of the time

lrtest(null_model, model1)

AIC(null_model, model1)

pR2(null_model)["llh"] * -2     # -2LL of the null

#Prediction accuracy for those who lived and died, separately

Titanic_Model1 %>%   #Crosstab for Survived and the predictions. 
  group_by(Survived, pred_model1) %>%
  dplyr::summarise(n = n()) %>%
  spread(Survived, n)

Titanic_Model1 %>%   #Correctly predicted as dying.
  filter(Survived == "Died") %>%
  group_by(pred_correct) %>%
  dplyr::summarise(count = n()) %>%
  mutate(percent = (count/sum(count)*100))

Titanic_Model1 %>%   #Correctly predicted as surviving.
  filter(Survived == "Lived") %>%
  group_by(pred_correct) %>%
  dplyr::summarise(count = n()) %>%
  mutate(percent = (count/sum(count)*100))


#########RELATIVE CONTRIBUTION OF PREDICTORS TO THE MODEL#######

dominance_model1 <- dominanceAnalysis(model1)

contributionByLevel(dominance_model1, fit.functions = "r2.m")

plot(dominance_model1, which.graph = "conditional")

averageContribution(dominance_model1, fit.function = "r2.m")

plot(dominance_model1, fit.function = "r2.m") + 
  theme_classic() +
  ylab("McFadden R^2") +
  xlab("Predictor")



####### ODDS RATIO AND 95% CI ###########

exp(cbind(OR = coef(model1), confint(model1)))


###### KATE AND SUE SURVIVAL PROBABILITY ########


#Kate without Leonardo

(1.71+(1.415*0)+(-2.650*1)+(2.642*1)+(-.045*20)+(-.039*1)+(-.368*0)) 
                                               
(exp(0.763))/(1+(exp(0.763)))

#Kate with Leonardo

(1.71+(1.415*0)+(-2.650*1)+(2.642*1)+(-.045*20)+(-.039*1)+(-.368*1)) 

(exp(0.395))/(1+(exp(0.395)))



#Sue with Leondardo

1.71+(1.415*0)+(-2.650*1)+(2.642*1)+(-.045*4)+(-.039*2)+(-.368*0) 

(exp(1.444))/(1+(exp(1.444)))


#Sue without Leonardo

(1.71+(1.415*0)+(-2.650*1)+(2.642*1)+(-.045*4)+(-.039*1)+(-.368*0))

(exp(1.483))/(1+(exp(1.483)))





