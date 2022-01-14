
########LOAD PACKAGES#######

install.packages("psych") #exploratory data analysis with describe()
install.packages("tidyverse")  #nicer-looking code
install.packages("cAIC4")   #for cAIC
install.packages("r2glmm")  #getting the r2beta
install.packages("lme4")  #for running the mixed models
install.packages("lmerTest") #significance testing
install.packages("MuMIn") #to get r.squared
install.packages("rockchalk") #presentation of regression analysis
install.packages("ggplot2")  #for plotting
install.packages("mice")   #examining missing values
install.packages("plyr")   #for data management

library("psych")
library("tidyverse")
library("cAIC4")
library("r2glmm")
library("lme4")
library("lmerTest")
library("MuMIn")
library("ggplot2")
library("mice")
library("rockchalk")
library("plyr")

#Custom function to extract standardized beta coefficients from linear mixed models. From the lab notes. 
#https://github.com/kekecsz/SIMM61-Course-materials/blob/main/Exercise_03%20-%20Linear%20mixed%20models%20basics/Exercise_03_Basics_of_linear_mixed_models.pdf

stdCoef.merMod <- function(object) {
  sdy <- sd(getME(object, "y"))
  sdx <- apply(getME(object, "X"), 2, sd)
  sc <- fixef(object) * sdx/sdy
  se.fixef <- coef(summary(object))[, "Std. Error"]
  se <- se.fixef * sdx/sdy
  return(data.frame(stdcoef = sc, stdse = se))
}


#######LOAD DATASETS########

surgery_data_A <- read.csv("https://raw.githubusercontent.com/kekecsz/SIMM61-Course-materials/main/Home_assignment/surgery_data_1.csv")
surgery_data_B <- read.csv("https://raw.githubusercontent.com/kekecsz/SIMM61-Course-materials/main/Home_assignment/surgery_data_2.csv")


#######CHECKING VARIABLES IN DATASET A FOR ERRORS#######

#Ensuring they are the correct data type

str(surgery_data_A)

surgery_data_A1 <- surgery_data_A %>%
  mutate(sex = as_factor(sex),
         hospital = as_factor(hospital))

#Changing the names of certain cateogries so it will look nicer in graphs later

surgery_data_A1 <- dplyr::rename(surgery_data_A1, Pain = pain, Hospital = hospital) 
         
levels(surgery_data_A1$Hospital)

surgery_data_A1$Hospital <- dplyr::recode(surgery_data_A1$Hospital, 
                          hospital_1 = "Hospital 1",
                          hospital_2 = "Hospital 2",
                          hospital_3 = "Hospital 3",
                          hospital_4 = "Hospital 4",
                          hospital_5 = "Hospital 5",
                          hospital_6 = "Hospital 6",
                          hospital_7 = "Hospital 7",
                          hospital_8 = "Hospital 8",
                          hospital_9 = "Hospital 9",
                          hospital_10 = "Hospital 10")

#Checking that there are no duplicate entires

check_duplicate <- data.frame(table(surgery_data_A1$ID)) 

check_duplicates[check_duplicates$Freq > 1,]

table(duplicated(surgery_data_A1))

#Are there any missing values?

md.pattern(surgery_data_A1, rotate.names = TRUE)

#Checking that there are no "strange" values

surgery_data_A1 %>%    #Checking categorical variables
  select(sex, Hospital) %>%
  lapply(table)        #There is one female coded as "woman", which needs to be changed into female

levels(surgery_data_A1$sex)

surgery_data_A1$sex <- combineLevels(surgery_data_A1$sex, 
                                      levs = c("female", "woman"), newLabel = "female")

table(surgery_data_A1$sex)

surgery_data_A1 %>%   #Checking age - seems ok
  ggplot() +
  aes(x = age) +
  geom_histogram()

surgery_data_A1 %>%   #Checking pain - seems ok
  ggplot() +
  aes(x = Pain) +
  geom_bar()

surgery_data_A1 %>%   #Checking STAI_trait - seems ok
  ggplot() +
  aes(x = STAI_trait) +
  geom_bar()

surgery_data_A1 %>%   #Checking pain_cat
  ggplot() +
  aes(x = pain_cat) +
  geom_bar()

surgery_data_A1 %>%   #Checking cortisol_serum - seems ok
  ggplot() +
  aes(x = cortisol_serum) +
  geom_histogram()


surgery_data_A1 %>%   #Checking cortisol_saliva - seems ok
  ggplot() +
  aes(x = cortisol_saliva) +
  geom_histogram()


surgery_data_A1 %>%   #Checking mindfulness - seems ok
  ggplot() +
  aes(x = mindfulness) +
  geom_histogram()


surgery_data_A1 %>%   #Checking weight - seems ok
  ggplot() +
  aes(x = weight) +
  geom_histogram()


surgery_data_A1 %>%   #Checking IQ - seems ok
  ggplot() +
  aes(x = IQ) +
  geom_histogram()


surgery_data_A1 %>%   #Checking household income - seems like there is a possible outlier. 
  ggplot() +
  aes(x = household_income) +
  geom_boxplot()


surgery_data_A1 %>%
  select(Pain, pain_cat, age, STAI_trait, cortisol_saliva, cortisol_serum,
         mindfulness, weight, IQ, household_income, sex) %>%
  describe()


#Checking in on the negative value in the household income variable. It is the only negative value = a data error? Run the model with and without the observation. 

surgery_data_A1 %>% filter(household_income < 0)


####### VISUAL INSPECTION OF VARIABLES THAT SEEM IMPORTANT TO PAIN ########


surgery_data_A1 %>%  
  ggplot() + 
  aes(y = Pain, x = sex) +
  geom_boxplot()  +
  theme_classic() +
  coord_flip()

surgery_data_A1 %>%  
  ggplot() + 
  aes(y = Pain, x = Hospital) +
  geom_boxplot()  +
  theme_classic()


####### LINEAR MIXED REGRESSION #######

#Crate new data frame with only those variables we need

surgery_dataA_model1 <- surgery_data_A1 %>%
  select(Pain, age, sex, STAI_trait, pain_cat, mindfulness, cortisol_serum, Hospital)


#Linear mixed model to estimate post-op pain. Age, Sex, STAI, pain cat, mindfulness, serum cortisol as FIXED EFFECT PREDICTORS.
#and RANDOM INTERCEPTS using hospital-ID. 

modelMixed1 <- lmer(Pain ~ age + sex + STAI_trait + pain_cat +    #allowing intercept to vary by hospital
                    mindfulness + cortisol_serum + (1 | Hospital), data = surgery_dataA_model1)

summary(modelMixed1)

confint(modelMixed1)   #confidence intervals for coefficients. 

######## COMPUTING VARIANCE ###########

#Variance explained by fixed effect predictors (marginal R^2)

marginalR <- r2beta(modelMixed1, method = "nsj", data = surgery_data_A1)

#Variance explained by the fixed and random effect terms combined

conditionalR <- r.squaredGLMM(modelMixed1)



####### USING MODEL 1 TO PREDICT PAIN IN DATASET B #######

#Tidying up dataset B

str(surgery_data_B)

surgery_data_B1 <- surgery_data_B %>%    #Changing variable types. 
  mutate(sex = as_factor(sex),
         hospital = as_factor(hospital))

surgery_data_B2 <- surgery_data_B1 %>%   #Selecting the necessary variables. 
  select(pain, pain_cat, age, STAI_trait, cortisol_serum,
         mindfulness, sex, hospital)

#Checking that there are no strange values in dataset B

md.pattern(surgery_data_B1, rotate.names = TRUE)


surgery_data_B2 %>%
  select(pain, pain_cat, age, STAI_trait, cortisol_serum,
         mindfulness, sex, hospital) %>%
  describe()


#Using model coeffieients from data file A to predict pain in data file B 


prediction <- predict(modelMixed1, newdata = surgery_data_B2, re.form = NA)

surgery_data_B2 <- surgery_data_B2 %>%    #Creating variable in dataset B with the predicted pain using Model 1
  mutate(pred_painModel1 = predict(modelMixed1, newdata = surgery_data_B2, re.form = NA))



###### COMPUTING VARIANCE FOR DATA FILE B #######

#Residual error terms (subtract predicted value of the outcome variable from actual observed value)

RSS <- sum((abs(surgery_data_B2$pain - surgery_data_B2$pred_painModel1))^2)


#TSS - the sum of squared residual error terms of the null model (with mean as outcome prediction) 

null_mean <- lm(pain ~ 1, data = surgery_data_B2) #The null model predictions

TSS <- sum((surgery_data_B2$pain - predict(null_mean))^2)

#The variance explained

RsquaredB <- 1-(RSS/TSS)

RsquaredB

#Comparing Rsquared to conditional and marginal

marginalR
conditionalR
RsquaredB


####### MOST INFLUENTAIL PREDICTORS FROM MODEL 1########

#Standardize coefficients from Model 1

stdCoef.merMod(modelMixed1) #Cortisol at 0.34 is the largest impact. 

###### NEW LINEAR MIXED EFFECTS ON DATASET A BUT ONLT MOST INFLUENTIAL PREDICTORS #######

modelMixed2 <- lmer(Pain ~ cortisol_serum + (cortisol_serum | Hospital), 
                    data = surgery_dataA_model1)

summary(modelMixed2)


#######COMPARE MODEL FIT#########

cAIC(modelMixed2)$caic

cAIC(modelMixed1)$caic

######VISUALIZE FITTED REGRESSION LINES#######

#Save prediction of the model into a variable


surgery_dataA_model1 <- surgery_dataA_model1 %>% 
  mutate(predict_reg = predict(modelMixed2))


surgery_dataA_model1 %>% 
  ggplot() + 
  aes(y = Pain, x = cortisol_serum, group = Hospital) +
  geom_point(aes(color = Hospital), size = 4) +
  geom_line(color = "red", aes(y = predict_reg, x = cortisol_serum)) +
  facet_wrap(~Hospital, ncol = 2) +
  xlab("Serum cortisol") + 
  ylab("Pain")
  
