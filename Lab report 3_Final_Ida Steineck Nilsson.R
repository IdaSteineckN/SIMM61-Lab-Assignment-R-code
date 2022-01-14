######LOAD PACKAGES########

library(GGally) #for ggcorr
library(corrr)  #network plots
library(ggcorrplot) #correlation pots
library(factoextra) #visualizing factors
library(paran) #paran
library(psych) #KMO, KGC and other useful factor analysis functions
library(GPArotation)  #complement to the psych package fa function
library(MVN) #for multivariate normality checks
library(ICS)#for multivariate skewness and kurtosis tests
library(tidyverse) #tidy code
library(mice) #missing values plots


########CUSTOM FUNCTION FROM THE LAB########
#https://github.com/kekecsz/SIMM61-Course-materials/blob/main/Exercise_06%20-%20CFA%20and%20EFA/Exercise_06_PCA_and_EFA.pdf

fviz_loadnings_with_cor <- function(mod, axes = 1, loadings_above = 0.4){	
  require(factoextra)	
  require(dplyr)	
  require(ggplot2)	
  
  
  
  if(!is.na(as.character(mod$call$call)[1])){	
    if(as.character(mod$call$call)[1] == "PCA"){	
      contrib_and_cov = as.data.frame(rbind(mod[["var"]][["contrib"]], mod[["var"]][["cor"]]))	
      
      vars = rownames(mod[["var"]][["contrib"]])	
      attribute_type = rep(c("contribution","correlation"), each = length(vars))	
      contrib_and_cov = cbind(contrib_and_cov, attribute_type)	
      contrib_and_cov	
      
      plot_data = cbind(as.data.frame(cbind(contrib_and_cov[contrib_and_cov[,"attribute_type"] == "contribution",axes], contrib_and_cov[contrib_and_cov[,"attribute_type"] == "correlation",axes])), vars)	
      names(plot_data) = c("contribution", "correlation", "vars")	
      
      plot_data = plot_data %>% 	
        mutate(correlation = round(correlation, 2))	
      
      plot = plot_data %>% 	
        ggplot() +	
        aes(x = reorder(vars, contribution), y = contribution, gradient = correlation, label = correlation)+	
        geom_col(aes(fill = correlation)) +	
        geom_hline(yintercept = mean(plot_data$contribution), col = "red", lty = "dashed") + scale_fill_gradient2() +	
        xlab("variable") +	
        coord_flip() +	
        geom_label(color = "black", fontface = "bold", position = position_dodge(0.5))	
      
      
    }	
  } else if(!is.na(as.character(mod$Call)[1])){	
    
    if(as.character(mod$Call)[1] == "fa"){	
      loadings_table = mod$loadings %>% 	
        matrix(ncol = ncol(mod$loadings)) %>% 	
        as_tibble() %>% 	
        mutate(variable = mod$loadings %>% rownames()) %>% 	
        gather(factor, loading, -variable) %>% 	
        mutate(sign = if_else(loading >= 0, "positive", "negative"))	
      
      if(!is.null(loadings_above)){	
        loadings_table[abs(loadings_table[,"loading"]) < loadings_above,"loading"] = NA	
        loadings_table = loadings_table[!is.na(loadings_table[,"loading"]),]	
      }	
      
      if(!is.null(axes)){	
        
        loadings_table = loadings_table %>% 	
          filter(factor == paste0("V",axes))	
      }	
      
      
      plot = loadings_table %>% 	
        ggplot() +	
        aes(y = loading %>% abs(), x = reorder(variable, abs(loading)), fill = loading, label =       round(loading, 2)) +	
        geom_col(position = "dodge") +	
        scale_fill_gradient2() +	
        coord_flip() +	
        geom_label(color = "black", fill = "white", fontface = "bold", position = position_dodge(0.5)) +	
        facet_wrap(~factor) +	
        labs(y = "Loading strength", x = "Variable")	
    }	
  }	
  
  
  
  
  
  
  return(plot)	
  
}	


##########LOAD ANIMAL RIGHTS DATA#######

animalrights_1 <- read.csv("https://raw.githubusercontent.com/kekecsz/SIMM61-Course-materials/main/Exercise_06%20-%20CFA%20and%20EFA/animalrights.csv")


########CHANGING VARIABLES INTO CORRECT TYPE#########

animalrights <- animalrights_1 %>%
  mutate(sex = as_factor(sex),
         party = as_factor(party))



#Giving labels to the categorical variables

animalrights$sex <- recode(animalrights$sex, "1" = "Female", "2" = "Male")

animalrights$party <- recode(animalrights$party, "1" = "Democrat", "2" = "Republican",
                             "3" = "Other", "4" = "None")
levels(animalrights$sex)
levels(animalrights$party)


########EXPLORING DESCRIPTIVE STATS#########

animalrights %>%    #Checking categorical variables. More females than males. 
  select(sex, party) %>%
  lapply(table)   

animalrights %>%    #Checking descriptive statistics and that each question in the survey has at most 5 answer categories. 
  select(-c(sex, party)) %>%   #The data is quite skewed... Seems like people care a lot about animal rights. 
  lapply(describe)   

summary(animalrights)


#Check duplicate entries

table(duplicated(animalrights))


########MISSING VALUES########

md.pattern(animalrights, rotate.names = TRUE)

colSums(is.na(animalrights))


########CREATING NEW DATAFRAMES#######

#Create new dataset with only complete cases

animalrightsComplete <- animalrights[complete.cases(animalrights), ]

#Create a new dataset with only the questions

animalrightsQuestions <- animalrightsComplete %>%
  select(ar1:ar28)

#########CREATING CORRELATION MATRIX AND CORREALATION PLOTS#######

cor_matrix <- animalrightsQuestions %>%    #Create create correlation matrix
  cor()

cor_matrix

ggcorr(cor_matrix)  #Correlation plot

corrplot(cor_matrix, type = "upper",  #Positive correaltions = blue, negative correlations = red
         order = "hclust", tl.col = "black", 
         tl.srt = 45)



######FACTORABILITY######

#For Barlett's test: checking ratio between number of observations and number of observed variables

149/28    #It is 5.3 --> over 5, so better to use KMO test than Bartlett's test


#Kaiser-Meyer-Olkin (KMO) test

KMO(cor_matrix)    #KMO is higher that 0.6 --> the data is facotable. 


########CHECKING MULTIVARIATE NORMALITY######

multiv_norm <- mvn(animalrightsQuestions, mvnTest = "hz")   #Henze-Zirkler test. P-value under 0.05. 
multiv_norm$multivariateNormality

mvnorm.skew.test(animalrightsQuestions)  #P-value is under 0.05 for both these tests. 
mvnorm.kur.test(animalrightsQuestions)   # We need to use the PAF extraction method. 


#######CHOOSING IDEAL NUMBER OF FACTORS#######

#Parallel test - suggests 5, I interpret Skree plot as 2 or 3. 
fa.parallel(cor_matrix, n.obs = 149, fa = "fa", fm = "pa") 

#VSS and MAP - both suggest 2 factors. 
nfactors(cor_matrix, n.obs = 149)

#Kaiser-Guttman criterion, eigenvalues - suggests 2

KGC(cor_matrix)


########TRYING 5 FACTORS#########

#Trying 5 factors + different oblique rotations

EFA_model1_oblim5 <- fa(cor_matrix, nfactors = 5, fm = "pa", rotate = "oblimin")  #uses the oblique direct oblim rotation

EFA_model1_promax5 <- fa(cor_matrix, nfactors = 5, fm = "pa", rotate = "promax")  #uses the oblique direct oblim rotation

EFA_model1_oblim5$loadings
EFA_model1_promax5$loadings

fa.diagram(EFA_model1_oblim5)
fa.diagram(EFA_model1_promax5) #Looks good in terms of distribution of items across factors, but referring back to the survey items 5-factor solution seems to be lacking. 



#########TRYING 3 FACTORS##########

EFA_model1_promax3 <- fa(cor_matrix, nfactors = 3, fm = "pa", rotate = "varimax")  #uses the oblique direct oblim rotation
fa.diagram(EFA_model1_promax3)
fa.sort(EFA_model1_promax3, polar=FALSE)  

animalrightsQuestions3 <- animalrightsQuestions %>%  #Removing items with low loading 
  select(-c(ar3, ar14, ar16, ar25))

cor_matrix3 <- animalrightsQuestions3 %>%    #Create create new correlation matrix with the retained items.
  cor()

EFA_model2_promax3 <- fa(cor_matrix3, nfactors = 3, fm = "pa", rotate = "varimax")  #uses the oblique direct oblim rotation
fa.diagram(EFA_model2_promax3)
fa.sort(EFA_model2_promax3, polar=FALSE)  #Looks good in terms of distribution of items across factors, but referring back to the survey items 5-factor solution seems to be lacking. 


#######TRYING AND SETTLING ON 2 FACTORS#######


#Trying 2 factors with oblim rotation

EFA_model1_oblim2 <- fa(cor_matrix, nfactors = 2, fm = "pa", rotate = "oblimin")  #uses the oblique direct oblim rotation

fa.diagram(EFA_model1_oblim2) #visaluze it

EFA_model1_oblim2_commun <- as.data.frame(sort(EFA_model1_oblim2$communality, decreasing = TRUE))

EFA_model1_oblim2_commun #checking lowest communality. 

mean(EFA_model1_oblim2$communality) 

fa.sort(EFA_model1_oblim2, polar=FALSE)

#Trying two factors with promax rotation

EFA_model1_promax2 <- fa(cor_matrix, nfactors = 2, fm = "pa", rotate = "promax")  #uses the oblique direct oblim rotation

fa.diagram(EFA_model1_promax2)    #items evenly distributed across the factors 



#######REMOVING ITEMS WITH LOW COMMUNALITY OR MAIN LOADING########

animalrightsQuestions2 <- animalrightsQuestions %>% #attempt with 4
  select(-c(ar8, ar28, ar1, ar3, ar16, ar12))


cor_matrix2 <- animalrightsQuestions2 %>%    #Create create correlation matrix
  cor()

EFA_model2_oblim2 <- fa(cor_matrix2, nfactors = 2, fm = "pa", rotate = "oblimin")  #uses the oblique direct oblim rotation

fa.diagram(EFA_model2_oblim2)  #visualize the structure

EFA_model2_oblim2_commun <- as.data.frame(sort(EFA_model2_oblim2$communality, decreasing = TRUE)) #check communalities

EFA_model2_oblim2_commun #checking lowest communality. 

mean(EFA_model2_oblim2$communality)   #average communality is 0.38. 

fa.sort(EFA_model2_oblim2, polar=FALSE)   #get loadings

eigenv <- eigen(cor(animalrightsQuestions2)) #get eigenvalues
eigenv$values


########GRAPS - MOST IMPORTANT QUESTIONS#########

fviz_loadnings_with_cor(EFA_model2_oblim2, axes = 1, loadings_above = 0.4)

fviz_loadnings_with_cor(EFA_model2_oblim2, axes = 2, loadings_above = 0.4)


#######SAVING FACTOR SCORES########

factor_scores <- factor.scores(animalrightsQuestions2[, 1:22], EFA_model2_oblim2)$scores
animalrightsComplete_factorver <- cbind(animalrightsQuestions2, factor_scores)

#Creating the needed dataframe

animalrightsComplete_factorver <- bind_cols(animalrightsComplete_factorver, animalrightsComplete["liberal"])

str(animalrightsComplete_factorver)


#######RUNNING REGRESSION########

regression <- lm(liberal ~ PA1 + PA2, data = animalrightsComplete_factorver)

summary(regression)


########CHECKING OUTLIERS#######

cooksD <- cooks.distance(regression)

install.packages("fdm2id")
library(fdm2id)

cookplot(regression)    #This identifies several outliers: 18, 22, 23, 34, 42, 43, 63, 104, 111, 113, 124.

########CHECKING COOK'S D#########
