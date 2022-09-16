# Load packages 
library(tidyverse)
library(stringr)
library(gtsummary)
library(flextable)


# Load data into R 
data <- readxl::read_excel("data/AMR_KAP_Coded.xlsx")


# Data pre-processing for knowledge section 
df_knowledge <- data %>% 
  select(12:23) %>% 
  mutate(Total_Knowledge_Score = rowSums(across(where(is.numeric)))) %>% 
  mutate(Knowledge_Score_Percent = round((Total_Knowledge_Score/12) * 100,2)) %>% 
  mutate(Knowledge_Level = case_when(
    Knowledge_Score_Percent >= 0 & Knowledge_Score_Percent <= 49 ~ "Poor", 
    Knowledge_Score_Percent > 49 & Knowledge_Score_Percent <= 79 ~ "Moderate", 
    Knowledge_Score_Percent > 79 & Knowledge_Score_Percent <= 100 ~ "Good", 
    
  ))


# Data pre-processing for attitude section 
df_attitude <- data %>% 
  select(24:33) %>% 
  mutate(Total_Attitude_Score = rowSums(across(where(is.numeric)))) %>% 
  mutate(Attitude_Score_Percent = round((Total_Attitude_Score/10) * 100,2)) %>% 
  mutate(Attitude_Level = case_when(
    Attitude_Score_Percent >= 0 & Attitude_Score_Percent <= 49 ~ "Negative", 
    Attitude_Score_Percent > 49 & Attitude_Score_Percent <= 79 ~ "Uncertain", 
    Attitude_Score_Percent > 79 & Attitude_Score_Percent <= 100 ~ "Positive", 
    
  ))


# Data pre-processing for practices section 
df_practices <- data %>% 
  select(34:39) %>% 
  mutate(Total_Practice_Score = rowSums(across(where(is.numeric)))) %>% 
  mutate(Practice_Score_Percent = round((Total_Practice_Score/6) * 100,2)) %>% 
  mutate(Practice_Level = case_when(
    Practice_Score_Percent >= 0 & Practice_Score_Percent <= 79 ~ "Poor", 
    Practice_Score_Percent > 79 & Practice_Score_Percent <= 100 ~ "Good", 

  ))


