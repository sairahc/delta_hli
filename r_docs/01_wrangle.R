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
df <- read.csv("C:/Users/sch044/OneDrive - UiT Office 365/R/delta_hli/data/23042021.csv")

# convert all colnames to lowercase
colnames(df) <- tolower(colnames(df))

# remove all obs that do not have a second questionnaire
df <- df[!is.na(df$yserienr),]

# create ID variable
df$id <- 1:nrow(df)

# Create function to parse 1900 and 2000 at year "19"

parseYear19 <- function(dateAsInteger){
  sixDigitInteger <- sprintf("%06d", dateAsInteger) #left-pad with zeros if less than 6 digits
  dateAsDate <- as.Date(as.character(sixDigitInteger), "%d%m%y")
  a <- year(dateAsDate) %% 100
  b <- ifelse(
    a > 19, 
    1900 + a, 
    2000 + a)
  monthFromDate <- sprintf("%02d", month(dateAsDate))
  dayFromDate <- day(dateAsDate)
  return(as.Date(
    paste(b, 
          monthFromDate, 
          dayFromDate,
          sep = ""),
    "%Y%m%d")
  )
}

df$deathDate <- parseYear19(df$doddt)
df$emigrationDate <- parseYear19(df$emigdt)
df$diagnosisDate <- parseYear19(df$diagdat)

# follow up time 
find_follow_up_time_days <- function(dataframe){
  endOfFollowUp <-  pmin(as.Date("2018-12-31"), 
                         dataframe$deathDate,
                         dataframe$emigrationDate,
                         dataframe$diagnosisDate,
                         na.rm = TRUE)
  
  mutate(dataframe, followUpTimeDays = endOfFollowUp - q2.date)
}


df <- find_follow_up_time_days(df)

# age exit
find_age_exit <- function(dataframe){
  dataframe %>%
    mutate(ageExit = as.numeric(q2.age + followUpTimeDays/365.25))
  
}

df <- find_age_exit(df)


# merge year of birth into one column, assume 1st July birthday, date format
df$dateBirth <- as.Date(paste0("010719",
                              ifelse(!is.na(df$faar),
                                    df$faar,
                                    df$q1.faar)),
                        "%d%m%Y")

# find date of q1
df <- df %>%
  mutate(q1.date = case_when(serienr >= 11 & serienr<=16 ~ "01071996",
                             serienr >= 19 & serienr<=24 ~ "01071996",
                             serienr == 26 | serienr == 28| serienr == 29 ~ "01071998",
                             serienr == 32 | serienr == 33 ~ "01072002",
                             serienr == 35 ~ "07012003",
                             serienr == 36 ~ "07012004",
                             TRUE ~ NA_character_
  )
)

df$q1.date <- as.Date(df$q1.date, "%d%m%Y")

# find date of q2
df <- df %>%
  mutate(q2.date = case_when(yserienr == 32 | yserienr==33 ~ "01072002",
                             yserienr == 38 | yserienr == 39 ~ "01072004",
                             yserienr == 42 ~ "01072005",
                             yserienr == 46 ~ "01072010",
                             yserienr == 47 ~ "01072011",
                             yserienr == 48 ~ "01072014",
                             TRUE ~ NA_character_
  )
  )

df$q2.date <- as.Date(df$q2.date, "%d%m%Y")



# find age at Q1 and Q2
df$q1.age <- (df$q1.date - df$dateBirth)/365.25
df$q2.age <- (df$q2.date - df$dateBirth)/365.25


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

      # do not have variables for this yet
calculateGrAlcohol <- function(dataframe, name_of_resulting_variable){
  return(
    dataframe %>%
      mutate(name_of_resulting_variable = case_when(
        avhold == 1 & #Missing on alcohol if declared not sober and have not filled in frequency questions
          is.na(casesIncluded$OLGLASS) &
          is.na(casesIncluded$VINGLASS) &
          is.na(casesIncluded$DRINKER) ~ NA_real_,
        TRUE ~ ALKOGR
      )))
}  


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

# diet ---- waiting on fq vars

# whole grain bread 
    # need to get wholegrain bread variables to incorporate frequency

# fruit
    # need to get fruit variables to incorporate frequency

# cheese

df$q1.gramsCheese <- df$grbrostf + df$grbrostm + df$grkvostf + df$grkvostm

df$q2.gramsCheese <- df$ygrbrostf + df$ygrbrostm + df$ygrkvostf + df$ygrkvostm



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
                 TRUE ~ 4
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
                 TRUE ~ 4)
  return(a)
}

df$q1.alcoholScore <- score_alcohol(df$alkogr)
df$q2.alcoholScore <- score_alcohol(df$yalkogr)


# diet score ----
  # TODO Missing fq vars, some grams vars, and energy intake for energy adjustment
  # TODO Find absolute cutoffs for energy adjusted dietary variables to 
df$q1.gramsWholegrain <- df$grgrbrod # still need to add grfrubla
df$q2.gramsWholegrain <- df$ygrgrbrod

  # dairy

df$q1.gramsDairy <- df$grmelk + df$q1.gramsCheese # still need to add yoghurt
  


score_diet_full <- function(wholegrain,
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



# HLI score 

  # TODO add diet score

df$q1.hli <- df$q1.physicalActivityScore +
  df$q1.bmiScore +
  df$q1.smokingScore +
  df$q1.alcoholScore 

df$q2.hli <- df$q2.physicalActivityScore +
  df$q2.bmiScore +
  df$q2.smokingScore +
  df$q2.alcoholScore 

