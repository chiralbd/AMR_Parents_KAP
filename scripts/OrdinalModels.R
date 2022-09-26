# Load packages 
library(tidyverse)
library(stringr)
library(gtsummary)
library(flextable)
library(MASS)
library(janitor)

# Import data into 
data <- read.csv("data/AMR_Processed_Data_V2.csv", stringsAsFactors = T)


# Univariate and multivariate analysis of factors associated with parents’ good knowledge
model1 <- polr(formula = Knowledge_Level ~ Parents_age+Parents_sex+Parents_education_level+Employment_status+Family_type+Income+Number_of_children, data = data, Hess = T)

broom::tidy(model1, p.values = TRUE)
tbl_regression(model1, 
               exponentiate = TRUE, 
               tidy_fun = function(x, ...) broom::tidy(x, ..., p.values = TRUE))





tbl_uv_knowledge <- tbl_uvregression(
  processed_df[c("Parent’s age (years)", 
                 "Parent’s sex", 
                 "Parent’s education level", 
                 "Employment status", 
                 "Family type", 
                 "Your average household income per month (BDT)",
                 "Number of children", 
                 "Knowledge_Level")], 
  method = glm, 
  y = Knowledge_Level, 
  method.args = list(family = binomial),
  exponentiate = TRUE)




# Univariate and multivariate analysis of factors associated with parents’ positive attitude


# Factors associated with appropriate use of antibiotics in children (good practice)
model3 <- glm(Practice_Level ~ Parents_age+Parents_sex+Parents_education_level+Employment_status+Family_type+Income+Number_of_children, 
              data = data)


model <- glm(Practice_Level ~ `Parent’s age (years)`, data = processed_df)
tbl_regression(model, exponentiate = TRUE)



processed_df %>% 
  select("Parent’s age (years)", 
         "Parent’s sex", 
         "Parent’s education level", 
         "Employment status", 
         "Family type", 
         "Your average household income per month (BDT)",
         "Number of children", 
         "Practice_Level") %>% 
  tbl_uvregression(
    method = glm,
    y = Practice_Level,
    method.args = list(family = binomial(link = "logit")),
    exponentiate = TRUE,
    pvalue_fun = ~style_pvalue(.x, digits = 2)) %>%
  bold_p() %>%      
  bold_labels()  


summary(data$Practice_Level)
class(data$Practice_Level)
