#=================================================
# Title: 02_imputation

# Author: Sairah Lai Fa Chen
# Date: 13.07.2021


# Description:
# - create change variables for predictors
# - run imputation

# Notes:
# - conditional specifications; see 6.4.6 van Buuren for use of post()
# - https://www.gerkovink.com/miceVignettes/Passive_Post_processing/Passive_imputation_post_processing.html


#=================================================

# packages

install.packages("missForest")
install.packages("Hmisc")
install.packages("mice")
install.packages("VIM")
install.packages("rms")
install.packages("tidyverse")
install.packages("survival")
library(missForest)
library(Hmisc)
library(mice)
library(VIM)
library(rms)
library(tidyverse)
library(survival)


# generate change variables ----

# physical activity

calculateChangeVariables <- function(dataframe){
  dataframe %>%
    mutate(changePhysicalActivity = dataframe$yaktidag - dataframe$aktidag) %>%
    mutate(changeAlcohol = dataframe$yalkogr - dataframe$alkogr) %>%
    mutate(changeBMI = dataframe$q2.bmi - dataframe$q1.bmi) %>%
    mutate(changeHeight = dataframe$yhoyde - dataframe$hoyde) %>%
    mutate(changeWeight = dataframe$yvektana - dataframe$vektana) %>%
    mutate(changeSmokingScore = dataframe$q2.smokingScore - dataframe$q2.smokingScore) %>%
    mutate(changeWholegrain = dataframe$q2.gramsWholegrain - dataframe$q1.gramsWholegrain) %>%
    mutate(changeFruit = dataframe$q2.gramsFruit - dataframe$q1.gramsFruit) %>%
    mutate(changeVegetable = dataframe$q2.gramsVegetable - dataframe$q1.gramsVegetable) %>%
    mutate(changeDairy = dataframe$q2.gramsDairy - dataframe$q1.gramsDairy) %>%
    mutate(changeRedMeat = dataframe$q2.gramsRedMeat - dataframe$q1.gramsRedMeat) %>%
    mutate(changeProcessedMeat = dataframe$q2.gramsProcessedMeat - dataframe$q1.gramsProcessedMeat) %>%
    mutate(changeEnergyIntake = dataframe$ytotkjoul - dataframe$totkjoul) %>%
    mutate(nelsonAalen = nelsonaalen(dataframe,
                                     ageExit,
                                     lifestyleCancerIncident)) 
    
    
}

df <- calculateChangeVariables(df)


#nelson aalen





# Create dataframe with only variables in analysis / predictors of those variables ----
  # include all variables that appear in the complete data model, including the outcome
  # include variables related to nonresponse
  # include variables that explain a significant amount of variance - helps reduce incertainty of imputations and identified by their correlation with target variable
  # remove variables from above two points that have too many missing within the subgroup of incomplete cases


selectImputationVars <- function(dataframe){
  return(
    dataframe %>%
      select(id, #complete, but taken out from predictor matrix, below
             q2.age, # complete
             skole, #
             aktidag,
             changePhysicalActivity,
             vektana,
             changeWeight,
             hoyde,
             changeHeight,
             q1.bmi,
             changeBMI,
             q1.smokingScore,
             changeSmokingScore,
             alkogr,
             changeAlcohol,
             q1.gramsWholegrain,
             changeWholegrain,
             q1.gramsFruit,
             changeFruit,
             q1.gramsVegetable,
             changeVegetable,
             q1.gramsDairy,
             changeDairy,
             q1.gramsRedMeat,
             changeRedMeat,
             q1.gramsProcessedMeat,
             changeProcessedMeat,
             totkjoul,
             changeEnergyIntake,
             nelsonAalen,
             q1.hrtStatus,
             q1.ocEverUse,
             q1.parityCat,
             breastfeedingCategories,
             mensald,
             q2.menopausalStatus,
             q1.q2.timeDifferenceYears,
             statusLifestyle
             
      ))
}

imputationReady <- selectImputationVars(df)   
p_missing <- unlist(lapply(imputationReady, function(x) sum(is.na(x))))/nrow(imputationReady)
sort(p_missing[p_missing > 0], decreasing = TRUE)
sapply(df, function(x) sum(is.na(x)))


# imputation ----

  
imputeToMids <- function(imputationDataframe){
  initialise <- mice(imputationDataframe, maxit = 0)
  predM <- initialise$predictorMatrix
  meth <- initialise$method
  predM[, c("id")] = 0
  predM[c("nelsonAalen"),] = 0
  meth["q1.bmi"] <- "~I(vektana/(hoyde/100)^2)" 
  pred[c("vektana", "hoyde"), "q1.bmi"] <- 0
  imputedMids <- mice(imputationDataframe, method=meth, predictorMatrix = predM, m=20, print=FALSE)
  #TODO make a plot for imputedMids to check convergence
  return(imputedMids)
}

