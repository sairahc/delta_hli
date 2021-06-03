#----------------------------
# Title: 01_wrangle

# Author: Sairah L F Chen
# Date: 17.03.2021

# Description: 
#       - cleaning data
#       - formatting dates
#       - creating variables needed for scoring
#       - imputation will take place somewhere in the middle, will eventually split file

#TODO

#---------------------------

# load packages

library(plyr)
library(dplyr)
library(lubridate)


# load data
df <- read.csv("C:/Users/sch044/OneDrive - UiT Office 365/R/delta_hli/data/09052021.csv")

# convert all colnames to lowercase
colnames(df) <- tolower(colnames(df))

# remove all obs that do not have a second questionnaire
df <- df[!is.na(df$yserienr),]

# create ID variable
df$id <- 1:nrow(df)


# format dates

df$dateDeath <- as.Date(df$doddt, format= "%d/%m/%Y")
df$dateEmigration <- as.Date(df$emigdt, format = "%d/%m/%Y")
df$dateDiagnosis <- as.Date(df$diagdat, format = "%d/%m/%Y")
df$dateBirth <- as.Date(paste0("010719", df$faar),"%d%m%Y")
df$q1.date <- as.Date(df$startdat, format="%d/%m/%Y")
df$q2.date <- as.Date(df$ystartdat, format="%d/%m/%Y")


# find age at Q1 and Q2
df$q1.age <- as.numeric((df$q1.date - df$dateBirth)/365.25)
df$q2.age <- as.numeric((df$q2.date - df$dateBirth)/365.25)

# follow up time 
find_follow_up_time_days <- function(dataframe){
  endOfFollowUp <-  pmin(as.Date("2018-12-31"), 
                         dataframe$dateDeath,
                         dataframe$dateEmigration,
                         dataframe$dateDiagnosis,
                         na.rm = TRUE)
  
  mutate(dataframe, followUpTimeDays = endOfFollowUp - q2.date)
}


df <- find_follow_up_time_days(df)
df$followUpTimeDays <- as.numeric(df$followUpTimeDays)

# find how many people died and emigrated before follow up

table(df$dateDeath < df$q2.date)
table(df$dateEmigration < df$q2.date)
table(df$dateDiagnosis < df$q2.date)

# exclude prevalent cancers, death, emigration before start of follow-up
df <- df[df$followUpTimeDays>=0, ]

# age exit
find_age_exit <- function(dataframe){
  dataframe %>%
    mutate(ageExit = as.numeric(q2.age + followUpTimeDays/365.25))
  
}

df <- find_age_exit(df)
df$ageExit <- as.numeric(df$ageExit)






# bmi ----
df$q1.bmi <- df$vektana/(df$hoyde/100)^2
df$q2.bmi <- df$yvektana/(df$yhoyde/100)^2





# smoking variable transformations ----

# merge smoking intensity at age group variables from different q1 series

merge_smoking_intensity_age_intervals <- function(dataframe){
  return(
    dataframe %>%
      mutate(
        smokingIntensity1519 = case_when(
          serienr >= 14 & serienr <= 26 ~ roykant1,
          TRUE ~ roykant1519),
        smokingIntensity2029 = case_when(
          serienr >= 14 & serienr <= 26 ~ roykant2,
          TRUE ~ roykant2029),
        smokingIntensity3039 = case_when(
          serienr >= 14 & serienr <= 26 ~ roykant3,
          TRUE ~ roykant3039),
        smokingIntensity4049 = case_when(
          serienr >= 14 & serienr <= 26 ~ roykant4,
          TRUE ~ roykant4049),
        smokingIntensity5059 = case_when(
          serienr >= 14 & serienr <= 26 ~ roykant5,
          TRUE ~ roykant50mm),
        smokingIntensity60plus = case_when(
          serienr >= 14 & serienr <= 26 ~ roykant6,
          TRUE ~ roykant50mm))
  )
}

df <- merge_smoking_intensity_age_intervals(df)
# Find current smoking intensity at q1 by matching age to corresponding interval
      # takes into account series 28,29,32,33 roykar which is daily smoking intensity in the previous 4 years
      # respective roykar vars are used a proxy for the 10 year age intervals in the remaining series


find_q1_smoking_intensity <- function(dataframe){
  return (dataframe %>% 
            mutate(
              q1.smokingIntensity = case_when(
                roykstat == 3 ~ case_when(
                  serienr == 28 | serienr == 29 ~ roykar2,
                  serienr == 32 | serienr == 33 ~ roykar1,
                  q1.age >= 65 ~ smokingIntensity60plus,
                  q1.age >= 55 ~ smokingIntensity5059,
                  q1.age >= 45 ~ smokingIntensity4049,
                  q1.age >= 35 ~ smokingIntensity3039,
                  q1.age >= 25 ~ smokingIntensity2029,
                  q1.age >= 15 ~ smokingIntensity1519), 
                roykstat == 1 | roykstat == 2 ~ as.integer(0),
              )))
}  

df <- find_q1_smoking_intensity(df)

# Find smoking intensity at q2 
    #

find_q2_smoking_intensity <- function(dataframe){
  return(
    dataframe %>%
      mutate(
        q2.smokingIntensity = case_when(
          yroykstat == 3 ~ case_when(
            yserienr == 32 | yserienr == 33 ~ yroykar1,
            yserienr == 38 | yserienr == 39 | yserienr == 42 ~ yroksist5,
            yserienr == 46 ~ yroksist8,
            yserienr == 47 | yserienr == 48 ~ yroykar2),
          yroykstat == 1 | yroykstat == 2 ~ as.integer(0),# set never and former smokers to 0 smoking intensity, meaning that intensity will not be multiple imputed
          TRUE ~ NA_integer_  
          )
        )
      )
}

df <- find_q2_smoking_intensity(df)


# years since quit smoking 

years_since_smoking_cessation_q1 <- function(dataframe){
  return(dataframe %>%
           mutate(
             q1.yearsSinceSmokingCessation = case_when(
             roykstat == 2 ~ q1.age - sistald,
             roykstat == 1 | roykstat == 3 ~ 0)
           ))
}

df <- years_since_smoking_cessation_q1(df)

years_since_smoking_cessation_q2 <- function(dataframe){
  return(dataframe %>%
           mutate(
             q2.yearsSinceSmokingCessation = case_when(
               yroykstat == 2 ~ q2.age - sistald,
               yroykstat == 1 | yroykstat == 3 ~ 0)
           ))
}

df <- years_since_smoking_cessation_q2(df)


# alcohol ----


calculateGrAlcoholQ1 <- function(dataframe){
  return(dataframe %>%
           mutate(q1.gramsAlcohol = case_when(
            avhold == 1 & # avhold=1 is "not sober"
            is.na(dataframe$olglass) &
            is.na(dataframe$vinglass) &
            is.na(dataframe$drinker) ~ NA_real_,#Missing on alcohol if declared not sober and have not filled in frequency questions
            TRUE ~ alkogr
      )))
}  

df <- calculateGrAlcoholQ1(df)


calculateGrAlcoholQ2 <- function(dataframe){
  return(dataframe %>%
           mutate(q2.gramsAlcohol = case_when(
             avhold == 1 & # avhold=1 is "not sober"
               is.na(dataframe$yolglass) &
               is.na(dataframe$yvinglass) &
               is.na(dataframe$ydrinker) ~ NA_real_,#Missing on alcohol if declared not sober and have not filled in frequency questions
             TRUE ~ yalkogr
           )))
}  

df <- calculateGrAlcoholQ2(df)

# physical activity----

    # (dichotomize PA scale for descriptive purposes)

find_active <- function(physical_activity) {
  a <- case_when(physical_activity < 6 ~ 0,
                 physical_activity >=6 ~ 1
  )
  return(a)
  }

df$q1.active <- find_active(df$aktidag)
df$q2.active <- find_active(df$yaktidag)

# diet ---- 

# Whole grain bread----

calculateGrWholeGrainBreadQ1 <- function(dataframe){
  return(
    dataframe %>%
      mutate(q1.gramsWholeGrainBread = case_when(
        is.na(fqgrbrod) &
          is.na(fqknbrod) &
          is.na(brodfin) ~ NA_real_,
        TRUE ~ grgrbrod)
      ))
}

df <- calculateGrWholeGrainBreadQ1(df)


calculateGrWholeGrainBreadQ2 <- function(dataframe){
  return(
    dataframe %>%
      mutate(q2.gramsWholeGrainBread = case_when(
        is.na(yfqgrbrod) &
          is.na(yfqknbrod) &
          is.na(ybrodfin) ~ NA_real_,
        TRUE ~ ygrgrbrod)
      ))
}

df <- calculateGrWholeGrainBreadQ2(df)



# Fruit----

calculateGrFruitQ1 <- function(dataframe){
  return(
    dataframe %>% 
      mutate(q1.gramsFruit = case_when(
        is.na(fqeplepa) &
          is.na(fqappels) &
          is.na(fqbanan) &
          is.na(fqanfruk) ~ NA_real_,
        TRUE ~ grfrukt
      ))
  )
}

df <- calculateGrFruitQ1(df)



calculateGrFruitQ2 <- function(dataframe){
  return(
    dataframe %>% 
      mutate(q2.gramsFruit = case_when(
        is.na(yfqeplepa) &
          is.na(yfqappels) &
          is.na(yfqbanan) &
          is.na(yfqanfruk) ~ NA_real_,
        TRUE ~ ygrfrukt
      ))
  )
}

df <- calculateGrFruitQ2(df)


# Vegetables----

calculateGrVegetableQ1 <- function(dataframe){
  return(
    dataframe %>%
      mutate(q1.gramsVegetable = case_when(
        is.na(fqgulrot) &
          is.na(fqkaal) &
          is.na(fqbrokko)&
          is.na(fqsalat) &
          is.na(fqgrblan) &
          is.na(fqkalrot)&
          is.na(fqgrsak) ~ NA_real_,
        TRUE ~ grgrsak
      )))
}

df <- calculateGrVegetableQ1(df)


calculateGrVegetableQ2 <- function(dataframe){
  return(
    dataframe %>%
      mutate(q2.gramsVegetable = case_when(
        is.na(yfqgulrot) &
          is.na(yfqkaal) &
          is.na(yfqbrokko)&
          is.na(yfqsalat) &
          is.na(yfqgrblan) &
          is.na(yfqkalrot)&
          is.na(yfqgrsak) ~ NA_real_,
        TRUE ~ ygrgrsak
      )))
}

df <- calculateGrVegetableQ2(df)



# Red meat----

calculateGrRedMeatQ1 <- function(dataframe){
  return(
    dataframe %>%
      mutate(q1.gramsRedMeat = case_when(
        is.na(fqsteik) &
          is.na(fqkotele) &
          is.na(fqbiff) &
          is.na(fqkjkake) &#
          is.na(fqpolse) &#
          is.na(fqlapska) &
          is.na(fqpizza) &
          is.na(fqkyllin) &
          is.na(fqkjot) ~ NA_real_,
        TRUE ~ grrenkjo
      ))
  )
}


df <- calculateGrRedMeatQ1(df)

calculateGrRedMeatQ2 <- function(dataframe){
  return(
    dataframe %>%
      mutate(q2.gramsRedMeat = case_when(
        is.na(yfqsteik) &
          is.na(yfqkotele) &
          is.na(yfqbiff) &
          is.na(yfqkjkake) &#
          is.na(yfqpolse) &#
          is.na(yfqlapska) &
          is.na(yfqpizza) &
          is.na(yfqkyllin) &
          is.na(yfqkjot) ~ NA_real_,
        TRUE ~ ygrrenkjo
      ))
  )
}


df <- calculateGrRedMeatQ2(df)


# Processed Meat----




calculateGrProcessedMeatQ1 <- function(dataframe){
  return(
    dataframe %>%
      mutate(q1.gramsProcessedMeat = case_when(
        is.na(fqsteik) &
          is.na(fqkotele) &
          is.na(fqbiff) &
          is.na(fqkjkake) &
          is.na(fqpolse) &
          is.na(fqlapska) &
          is.na(fqpizza) &
          is.na(fqkyllin) &
          is.na(fqkjot) ~ NA_real_,
        TRUE ~ (grkjkake + # as per Parr, 2013
                  ifelse(
                    is.na(grpolse), 
                    0, 
                    grpolse) + 
                  grkjotpa)
      ))
  )
}



df <- calculateGrProcessedMeatQ1(df)



calculateGrProcessedMeatQ2 <- function(dataframe){
  return(
    dataframe %>%
      mutate(q2.gramsProcessedMeat = case_when(
        is.na(yfqsteik) &
          is.na(yfqkotele) &
          is.na(yfqbiff) &
          is.na(yfqkjkake) &
          is.na(yfqpolse) &
          is.na(yfqlapska) &
          is.na(yfqpizza) &
          is.na(yfqkyllin) &
          is.na(yfqkjot) ~ NA_real_,
        TRUE ~ (ygrkjkake + # as per Parr, 2013
                  ifelse(
                    is.na(ygrpolse), 
                    0, 
                    ygrpolse) + 
                  ygrkjotpa)
      ))
  )
}


df <- calculateGrProcessedMeatQ2(df)

# Milk----

calculateGrMilkQ1 <- function(dataframe){
  return(
    dataframe %>% 
      mutate(q1.gramsMilk = case_when(
        is.na(fqhemelk) &
          is.na(fqlemelk) &
          is.na(fqskmelk) &
          is.na(fqdmelk) ~ NA_real_,
        TRUE ~ grmelk
      ))
  )
}

df <- calculateGrMilkQ1(df)



calculateGrMilkQ2 <- function(dataframe){
  return(
    dataframe %>% 
      mutate(q2.gramsMilk = case_when(
        is.na(yfqhemelk) &
          is.na(yfqlemelk) &
          is.na(yfqskmelk) &
          is.na(yfqdmelk) ~ NA_real_,
        TRUE ~ ygrmelk
      ))
  )
}

df <- calculateGrMilkQ2(df)

# Cheese----

calculateGrCheeseQ1 <- function(dataframe){
  return(
    dataframe %>%
      mutate(q1.gramsCheese = case_when(
        is.na(fqsyltet) & 
          is.na(fqbrostf) &
          is.na(fqbrostm) &
          is.na(fqkvostf) &
          is.na(fqkvostm) &
          is.na(fqkjotpa) ~ NA_real_,
        TRUE ~ grbrostf + grbrostm + grkvostf + grkvostm
      ))
  )
}

df <- calculateGrCheeseQ1(df)

calculateGrCheeseQ2 <- function(dataframe){
  return(
    dataframe %>%
      mutate(q2.gramsCheese = case_when(
        is.na(yfqsyltet) & 
          is.na(yfqbrostf) &
          is.na(yfqbrostm) &
          is.na(yfqkvostf) &
          is.na(yfqkvostm) &
          is.na(yfqkjotpa) ~ NA_real_,
        TRUE ~ ygrbrostf + ygrbrostm + ygrkvostf + ygrkvostm
      ))
  )
}

df <- calculateGrCheeseQ2(df)



######################################################################################
# imputation 
######################################################################################

# physical activity score 

score_physical_activity <- function(physicalActivity){
  a <- cut(physicalActivity, breaks = c(1,4,5,6,7,11), 
    labels = c("0", "1", "2", "3", "4"), include.lowest = TRUE, right = FALSE)
  a <- as.numeric(as.character(a))
  return(a)
}

df$q1.physicalActivityScore <- score_physical_activity(df$aktidag)
df$q2.physicalActivityScore <- score_physical_activity(df$yaktidag)

# BMI score

score_bmi <- function(bmi){
  a <- case_when(bmi > 30 ~ 0,
                 bmi > 27 ~ 1,
                 bmi > 25 ~ 2,
                 bmi > 23 ~ 3,
                 bmi > 0 ~ 4,
                 TRUE ~ NA_real_
  )
}

df$q1.bmiScore <- score_bmi(df$q1.bmi)
df$q2.bmiScore <- score_bmi(df$q2.bmi)


# smoking score

score_smoking <- function(smoking_status, 
                         years_since_smoking_cessation,
                         current_smoking_intensity){
  a <- case_when(smoking_status == 1 ~ 4,
                 smoking_status == 2 & years_since_smoking_cessation > 10 ~ 3,
                 smoking_status == 2 & years_since_smoking_cessation <= 10 ~ 2,
                 smoking_status == 3 & current_smoking_intensity <= 3 ~ 1,
                 smoking_status == 3 & current_smoking_intensity > 3 ~ 0,
                 TRUE ~ NA_real_)
  return(a)
}


df$q1.smokingScore <- score_smoking(df$roykstat, 
                                    df$q1.yearsSinceSmokingCessation, 
                                    df$q1.smokingIntensity)


df$q2.smokingScore <- score_smoking(df$yroykstat, 
                                    df$q2.yearsSinceSmokingCessation, 
                                    df$q2.smokingIntensity)

# alcohol score


score_alcohol <- function(daily_grams_alcohol_intake){
  a <- case_when(daily_grams_alcohol_intake > 20 ~ 0,
                 daily_grams_alcohol_intake > 10 ~ 1,
                 daily_grams_alcohol_intake > 5 ~ 2,
                 daily_grams_alcohol_intake > 0 ~ 3,
                 daily_grams_alcohol_intake == 0 ~ 4,
                 TRUE ~ NA_real_)
  return(a)
}

df$q1.alcoholScore <- score_alcohol(df$alkogr)
df$q2.alcoholScore <- score_alcohol(df$yalkogr)


# diet score ----
  # TODO Missing fq vars, some grams vars, and energy intake for energy adjustment
  # TODO Find absolute cutoffs for energy adjusted dietary variables to 
df$q1.gramsWholegrain <- df$grgrbrod + df$grfrubla
df$q2.gramsWholegrain <- df$ygrgrbrod + df$ygrfrubla

  # dairy

df$q1.gramsDairy <- df$q1.gramsMilk + df$q1.gramsCheese + df$gryoghur
df$q2.gramsDairy <- df$q2.gramsMilk + df$q2.gramsCheese + df$ygryoghur


# nutrient density

find_nutrient_densities_Q1 <- function(dataframe){
  a <- dataframe$totkjoul/1000
  dataframe %>% 
    mutate(
      q1.ndWholeGrain = q1.gramsWholegrain/a,
      q1.ndVegetable = q1.gramsVegetable/a,
      q1.ndFruit = q1.gramsFruit/a,
      q1.ndDairy = q1.gramsDairy/a,
      q1.ndRedMeat = q1.gramsRedMeat/a,
      q1.ndProcessedMeat = q1.gramsProcessedMeat/a)
  
}

df <- find_nutrient_densities_Q1(df)

find_nutrient_densities_Q2 <- function(dataframe){
  a <- dataframe$ytotkjoul/1000
  dataframe %>% 
    mutate(
      q2.ndWholeGrain = q2.gramsWholegrain/a,
      q2.ndVegetable = q2.gramsVegetable/a,
      q2.ndFruit = q2.gramsFruit/a,
      q2.ndDairy = q2.gramsDairy/a,
      q2.ndRedMeat = q2.gramsRedMeat/a,
      q2.ndProcessedMeat = q2.gramsProcessedMeat/a)
  
}

df <- find_nutrient_densities_Q2(df)


score_diet_full_Q1 <- function(wholegrain,
                            vegetable,
                            fruit,
                            dairy,
                            red_meat,
                            processed_meat){ 
  a <- cut(wholegrain, breaks = c(
    quantile(wholegrain, probs =seq(0,1, 0.25), na.rm=TRUE)),
    labels = c("3","2", "1", "0"), include.lowest = TRUE, right = FALSE)
  a <- as.numeric(as.character(a))
  b <- cut(vegetable, breaks = c(
    quantile(vegetable, probs = seq(0,1, 0.25), na.rm=TRUE)),
    labels = c("3","2", "1", "0"), include.lowest = TRUE, right = FALSE)
  b <- as.numeric(as.character(b))
  c <- cut(fruit, breaks = c(
    quantile(fruit, probs = seq(0,1, 0.25), na.rm=TRUE)),
    labels = c("3","2", "1", "0"), include.lowest = TRUE, right = FALSE)
  c <- as.numeric(as.character(c))
  d <- cut(dairy, breaks = c(
    quantile(dairy, probs = seq(0,1, 0.25), na.rm=TRUE)),
    labels = c("3","2", "1", "0"), include.lowest = TRUE, right = FALSE)
  d <- as.numeric(as.character(d))
  e <- cut(red_meat, breaks = c(
    quantile(red_meat, probs = seq(0,1, 0.25), na.rm=TRUE)),
    labels = c("3","2", "1", "0"), include.lowest = TRUE, right = FALSE)
  e <- as.numeric(as.character(e))
  f <- cut(processed_meat, breaks = c(
    quantile(processed_meat, probs = seq(0,1, 0.25), na.rm=TRUE)),
    labels = c("3","2", "1", "0"), include.lowest = TRUE, right = FALSE)
  f <- as.numeric(as.character(f))
  g <- a+b+c+d+e+f
  return(g)}

df$q1.dietScoreFull <- score_diet_full_Q1(df$q1.ndWholeGrain,
                                          df$q1.ndVegetable,
                                          df$q1.ndFruit,
                                          df$q1.ndDairy,
                                          df$q1.ndRedMeat,
                                          df$q1.ndProcessedMeat)

quantile(df$q1.ndWholeGrain, probs =seq(0,1, 0.25), na.rm=TRUE)
quantile(df$q1.ndVegetable, probs =seq(0,1, 0.25), na.rm=TRUE)
quantile(df$q1.ndFruit, probs =seq(0,1, 0.25), na.rm=TRUE)
quantile(df$q1.ndDairy, probs =seq(0,1, 0.25), na.rm=TRUE)
quantile(df$q1.ndRedMeat, probs =seq(0,1, 0.25), na.rm=TRUE)
quantile(df$q1.ndProcessedMeat, probs =seq(0,1, 0.25), na.rm=TRUE)



score_diet_full_Q2 <- function(wholegrain, # base cutpoint on Q1 quantiles for nutrient density variables
                               vegetable,
                               fruit,
                               dairy,
                               red_meat,
                               processed_meat){ 
  a <- cut(wholegrain, breaks = c(
    0, 13.72589, 18.66699, 23.85540, 90.09577),
    labels = c("3","2", "1", "0"), include.lowest = TRUE, right = FALSE)
  a <- as.numeric(as.character(a))
  b <- cut(vegetable, breaks = c(
    0, 11.39690, 17.54922, 25.98288, 250.24961),
    labels = c("3","2", "1", "0"), include.lowest = TRUE, right = FALSE)
  b <- as.numeric(as.character(b))
  c <- cut(fruit, breaks = c(
    0,13.80500, 24.21816, 38.40811, 363.21080),
    labels = c("3","2", "1", "0"), include.lowest = TRUE, right = FALSE)
  c <- as.numeric(as.character(c))
  d <- cut(dairy, breaks = c(
    0, 16.30475,31.32657, 54.47469, 299.77021),
    labels = c("3","2", "1", "0"), include.lowest = TRUE, right = FALSE)
  d <- as.numeric(as.character(d))
  e <- cut(red_meat, breaks = c(
    0,1.054850,1.905374,3.007543,33.185089),
    labels = c("3","2", "1", "0"), include.lowest = TRUE, right = FALSE)
  e <- as.numeric(as.character(e))
  f <- cut(processed_meat, breaks = c(
    0,2.663930,4.314017,3.362097,61.962624),
    labels = c("3","2", "1", "0"), include.lowest = TRUE, right = FALSE)
  f <- as.numeric(as.character(f))
  g <- a+b+c+d+e+f
  return(g)}

df$q2.dietScoreFull <- score_diet_full_Q2(df$q2.ndWholeGrain,
                                          df$q2.ndVegetable,
                                          df$q2.ndFruit,
                                          df$q2.ndDairy,
                                          df$q2.ndRedMeat,
                                          df$q2.ndProcessedMeat)


# HLI diet

score_diet_Q1 <- function(wholegrain,
                               vegetable,
                               fruit,
                               dairy,
                               red_meat,
                               processed_meat){ 
  a <- cut(wholegrain, breaks = c(
    quantile(wholegrain, probs =seq(0,1, 0.25), na.rm=TRUE)),
    labels = c("3","2", "1", "0"), include.lowest = TRUE, right = FALSE)
  a <- as.numeric(as.character(a))
  b <- cut(vegetable, breaks = c(
    quantile(vegetable, probs = seq(0,1, 0.25), na.rm=TRUE)),
    labels = c("3","2", "1", "0"), include.lowest = TRUE, right = FALSE)
  b <- as.numeric(as.character(b))
  c <- cut(fruit, breaks = c(
    quantile(fruit, probs = seq(0,1, 0.25), na.rm=TRUE)),
    labels = c("3","2", "1", "0"), include.lowest = TRUE, right = FALSE)
  c <- as.numeric(as.character(c))
  d <- cut(dairy, breaks = c(
    quantile(dairy, probs = seq(0,1, 0.25), na.rm=TRUE)),
    labels = c("3","2", "1", "0"), include.lowest = TRUE, right = FALSE)
  d <- as.numeric(as.character(d))
  e <- cut(red_meat, breaks = c(
    quantile(red_meat, probs = seq(0,1, 0.25), na.rm=TRUE)),
    labels = c("3","2", "1", "0"), include.lowest = TRUE, right = FALSE)
  e <- as.numeric(as.character(e))
  f <- cut(processed_meat, breaks = c(
    quantile(processed_meat, probs = seq(0,1, 0.25), na.rm=TRUE)),
    labels = c("3","2", "1", "0"), include.lowest = TRUE, right = FALSE)
  f <- as.numeric(as.character(f))
  g <- a+b+c+d+e+f
  g <- cut(g, breaks = c(
    quantile(g, probs = seq(0, 1, 0.2), na.rm=TRUE)), 
    labels = c("0", "1", "2", "3", "4"), include.lowest = TRUE, right = FALSE)
  g <- as.numeric(as.character(g))
  return(g)}


df$q1.dietScore <- score_diet_Q1(df$q1.ndWholeGrain,
                                 df$q1.ndVegetable,
                                 df$q1.ndFruit,
                                 df$q1.ndDairy,
                                 df$q1.ndRedMeat,
                                 df$q1.ndProcessedMeat)


score_diet_Q2 <- function(wholegrain, # base cutpoint on Q1 quantiles for nutrient density variables
                               vegetable,
                               fruit,
                               dairy,
                               red_meat,
                               processed_meat){ 
  a <- cut(wholegrain, breaks = c(
    0, 13.72589, 18.66699, 23.85540, 90.09577),
    labels = c("3","2", "1", "0"), include.lowest = TRUE, right = FALSE)
  a <- as.numeric(as.character(a))
  b <- cut(vegetable, breaks = c(
    0, 11.39690, 17.54922, 25.98288, 250.24961),
    labels = c("3","2", "1", "0"), include.lowest = TRUE, right = FALSE)
  b <- as.numeric(as.character(b))
  c <- cut(fruit, breaks = c(
    0,13.80500, 24.21816, 38.40811, 363.21080),
    labels = c("3","2", "1", "0"), include.lowest = TRUE, right = FALSE)
  c <- as.numeric(as.character(c))
  d <- cut(dairy, breaks = c(
    0, 16.30475,31.32657, 54.47469, 299.77021),
    labels = c("3","2", "1", "0"), include.lowest = TRUE, right = FALSE)
  d <- as.numeric(as.character(d))
  e <- cut(red_meat, breaks = c(
    0,1.054850,1.905374,3.007543,33.185089),
    labels = c("3","2", "1", "0"), include.lowest = TRUE, right = FALSE)
  e <- as.numeric(as.character(e))
  f <- cut(processed_meat, breaks = c(
    0,2.663930,4.314017,3.362097,61.962624),
    labels = c("3","2", "1", "0"), include.lowest = TRUE, right = FALSE)
  f <- as.numeric(as.character(f))
  g <- a+b+c+d+e+f
  g <- cut(g, breaks = c(
    quantile(g, probs = seq(0, 1, 0.2), na.rm=TRUE)), 
    labels = c("0", "1", "2", "3", "4"), include.lowest = TRUE, right = FALSE)
  g <- as.numeric(as.character(g))
  return(g)
  
  }

df$q2.dietScore <- score_diet_Q2(df$q2.ndWholeGrain,
                                 df$q2.ndVegetable,
                                 df$q2.ndFruit,
                                 df$q2.ndDairy,
                                 df$q2.ndRedMeat,
                                 df$q2.ndProcessedMeat)


# HLI score 



df$q1.hli <- df$q1.physicalActivityScore +
  df$q1.bmiScore +
  df$q1.smokingScore +
  df$q1.alcoholScore +
  df$q1.dietScore

df$q2.hli <- df$q2.physicalActivityScore +
  df$q2.bmiScore +
  df$q2.smokingScore +
  df$q2.alcoholScore +
  df$q2.dietScore


# HLI groups

df$q1.hliGroup <- cut(df$q1.hli, c(0,5,10,15,20), include.lowest = TRUE)
df$q2.hliGroup <- cut(df$q2.hli, c(0,5,10,15,20), include.lowest = TRUE)

# time difference between questionnaires in years
df$q1.q2.timeDifferenceYears <- as.numeric((df$q2.date - df$q1.date)/365.25)

# absolute change in HLI
df$hliChange <- df$q2.hli - df$q1.hli

# change in HLI per year

df$hliChangeYearly <- df$hliChange/df$q1.q2.timeDifferenceYears

# categories for hli change

# +/-1 point considered stable

df$hliChange_plusminus1Stable <- cut(df$hliChange, c(-15, -1.1, 1, 15))
df$hliChange_plusminus2Stable <- cut(df$hliChange, c(-15,-2.1, 2, 15))

# change in individual HLI factors ----
    # physical activity
df$physicalActivityChange <- df$yaktidag-df$aktidag
    # BMI
df$bmiChange <- df$q2.bmi-df$q1.bmi
    # smoking status
df$smokingStatusChange <- case_when(
  (df$roykstat==1 & df$yroykstat==1) |(df$roykstat==2 & df$yroykstat==2)~ "non-smoking maintainer",
  df$roykstat==3 & df$yroykstat==2~ "quitter",
  (df$roykstat==1|df$roykstat==2) & df$yroykstat==3~ "started/re-started smoking",             
  df$roykstat==3 & df$yroykstat==3 ~ "constant smoker",
  (df$roykstat==3|df$roykstat==2) & df$yroykstat==1~ "later denier",                 
  df$roykstat==1 & df$yroykstat== 2 ~ "quick smoker/later admitter",           
  TRUE~NA_character_
)
                                                
    # alcohol
df$alcoholChange <- df$yalkogr-df$alkogr
    # diet
df$dietChange <- df$q2.dietScoreFull-df$q1.dietScoreFull


# how many within each Q1 HLI group are >=HLI 13 at Q2?



df$improved13above <- case_when(df$q2.hli >= 13 & df$q1.hli < 13 ~ "improved 13 above",
                                df$q2.hli >= 13 & df$q1.hli >= 13 ~ "maintain 13 above",
                                df$q2.hli <13 & df$q1.hli <13 ~ " maintain below 13")





# endpoint formatting ----

# overall cancer status
df$statusCancer <- ifelse(df$icd10_gr =="",
                          0,
                          1)

# colorectal cancer status

findColonCancerStatus <- function(dataframe){
  dataframe%>%
    mutate(statusColon = ifelse(dataframe$icd10_gr =="C18"|
                                  dataframe$icd10_gr == "C181" |
                                  dataframe$icd10_gr == "C183" |
                                  dataframe$icd10_gr == "C184" |
                                  dataframe$icd10_gr == "C185" |
                                  dataframe$icd10_gr == "C186" |
                                  dataframe$icd10_gr == "C187" |
                                  dataframe$icd10_gr == "C188" |
                                  dataframe$icd10_gr == "C189",
                                TRUE,
                                FALSE)
    )
}

df <- findColonCancerStatus(df)

findRectalCancerStatus <- function(dataframe){
  dataframe%>%
    mutate(statusRectal = ifelse(dataframe$icd10_gr =="C19"|
                                   dataframe$icd10_gr == "C199" |
                                   dataframe$icd10_gr == "C20" |
                                   dataframe$icd10_gr == "C209",
                                 TRUE,
                                 FALSE)
    )
}


df <- findRectalCancerStatus(df)

findColorectalCancerStatus <- function(dataframe){
  dataframe%>%
    mutate(statusColorectal = ifelse(dataframe$statusColon == TRUE |
                                       dataframe$statusRectal == TRUE,
                                     TRUE,
                                     FALSE
    ))
}


df <- findColorectalCancerStatus(df)

# Lung cancer status 


findLungCancerStatus <- function(dataframe){
  dataframe%>%
    mutate(statusLung = ifelse(dataframe$icd10_gr=="C340" |
                                 dataframe$icd10_gr=="C341" |
                                 dataframe$icd10_gr=="C342" |
                                 dataframe$icd10_gr=="C343" |
                                 dataframe$icd10_gr=="C348" |
                                 dataframe$icd10_gr=="C349" ,
                               TRUE, #true if event occurs
                               FALSE #false if censored
    ))
}


df <- findLungCancerStatus(df)



# alcohol related cancer status

findAlcoholStatus <- function(dataframe){
  dataframe %>%
    mutate(statusAlcohol = ifelse(
      icd10_gr=="C180"| # colorectal
        icd10_gr=="C181"|
        icd10_gr=="C182"|
        icd10_gr=="C183"|
        icd10_gr=="C184"|
        icd10_gr=="C185"|
        icd10_gr=="C186"|
        icd10_gr=="C187"|
        icd10_gr=="C188"|
        icd10_gr=="C189"|
        icd10_gr=="C199"|
        icd10_gr=="C50"| # breast
        icd10_gr=="C01"| # upper aerodigestive
        icd10_gr=="C02"|
        icd10_gr=="C03"|
        icd10_gr=="C04"|
        icd10_gr=="C05"|
        icd10_gr=="C06"|
        icd10_gr=="C07"|
        icd10_gr=="C09"|
        icd10_gr=="C10"|
        icd10_gr=="C32"| # larynx
        icd10_gr=="C11"| # pharynx
        icd10_gr=="C12"|
        icd10_gr=="C13"|
        icd10_gr=="C14"|
        icd10_gr=="C15"| # esophagus
        icd10_gr=="C22"| # liver
        icd10_gr=="C23"|
        icd10_gr=="C24",
      TRUE,
      FALSE
    ))
}

df <- findAlcoholStatus(df)

# tobacco related cancer status

findTobaccoStatus <- function(dataframe){
  dataframe %>%
    mutate(statusTobacco = ifelse(icd10_gr=="C01"| # upper aerodigestive
                                    icd10_gr=="C02"|# upper aerodigestive
                                    icd10_gr=="C03"|# upper aerodigestive
                                    icd10_gr=="C04"|# upper aerodigestive
                                    icd10_gr=="C05"|# upper aerodigestive
                                    icd10_gr=="C06"|# upper aerodigestive
                                    icd10_gr=="C07"|# upper aerodigestive
                                    icd10_gr=="C09"|# upper aerodigestive
                                    icd10_gr=="C32"|# larynx
                                    icd10_gr=="C11"|# pharynx
                                    icd10_gr=="C12"|# pharynx
                                    icd10_gr=="C13"|# pharynx
                                    icd10_gr=="C14"|# pharynx
                                    icd10_gr=="C15"|# esophagus
                                    icd10_gr=="C22"|# liver
                                    icd10_gr=="C23"|# liver
                                    icd10_gr=="C24"|# liver
                                    icd10_gr=="C25"|# pancreas
                                    icd10_gr=="C18"| # colorectal
                                    icd10_gr=="C180"|
                                    icd10_gr=="C181"|
                                    icd10_gr=="C182"|
                                    icd10_gr=="C183"|
                                    icd10_gr=="C184"|
                                    icd10_gr=="C185"|
                                    icd10_gr=="C186"|
                                    icd10_gr=="C187"|
                                    icd10_gr=="C188"|
                                    icd10_gr=="C189"|
                                    icd10_gr=="C199"|
                                    icd10_gr=="C209"|
                                    icd10_gr=="C67"|# bladder
                                    icd10_gr=="C64"|# kidney
                                    icd10_gr=="C65"|# kidney
                                    icd10_gr=="C53"|# cervix
                                    icd10_gr=="C16"|# stomach
                                    icd10_gr=="C33"|# trachea
                                    icd10_gr=="C340"|# lung
                                    icd10_gr=="C341"|
                                    icd10_gr=="C342"|
                                    icd10_gr=="C343"|
                                    icd10_gr=="C348"|
                                    icd10_gr=="C349"|
                                    icd10_gr=="C92",# acute myeloid leukemia
                                  TRUE,
                                  FALSE
                                  )
    )
                                    
                                    
                                  
}

df <- findTobaccoStatus(df)


# obesity related cancer status

findObesityStatus <- function(dataframe){
  dataframe %>%
    mutate(statusObesity = ifelse(
      icd10_gr=="C15"| # esophagus
        icd10_gr=="C25"| # pancreas
        icd10_gr=="C180"| # crc
        icd10_gr=="C181"| 
        icd10_gr=="C182"|
        icd10_gr=="C183"|
        icd10_gr=="C184"|
        icd10_gr=="C185"|
        icd10_gr=="C186"|
        icd10_gr=="C187"|
        icd10_gr=="C188"|
        icd10_gr=="C189"|
        icd10_gr=="C199"|
        icd10_gr=="C209"| # crc
        icd10_gr=="C50"| # breast
        icd10_gr=="C54"| # endometrial
        icd10_gr=="C64"| # kidney
        icd10_gr=="C65"| # kidney
        icd10_gr=="C73"| # thyroid
        icd10_gr=="C23", # gallbladder
      TRUE,
      FALSE
    )
    )
        
}

df <- findObesityStatus(df)

# female breast and reproductive related cancers

findReproductiveStatus <- function(dataframe) {
  dataframe %>% 
    mutate(statusReproductive = ifelse(
      icd10_gr=="C50"| # breast
        icd10_gr=="C51"| # vulva 
        icd10_gr=="C52"| # vagina
        icd10_gr=="C53"| # cervix
        icd10_gr=="C54"| # uterine
        icd10_gr=="C55"| # uterine
        icd10_gr=="C56"| # ovarian
        icd10_gr=="C57"| # other female genital organs
        icd10_gr=="C58", # other female genital organs
      TRUE,
      FALSE
    )
    )
  
}

df <- findReproductiveStatus(df)

# lifestyle cancer status

findLifestyleCancerStatus <- function(dataframe){
  dataframe %>%
    mutate(statusLifestyle = ifelse(
      statusAlcohol == T|
        statusTobacco == T|
        statusObesity == T |
        statusReproductive == T,
      TRUE,
      FALSE
    )
    )
  
}

df <- findLifestyleCancerStatus(df)







#########################

# create dataframe with only complete cases

df <- df[!is.na(df$q1.hli) & !is.na(df$q2.hli),]
