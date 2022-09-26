library(gtsummary)
library(dplyr)
library(MASS)
library(gt)

# https://github.com/ddsjoberg/gtsummary/issues/958

# example data for ordinal logistic regression
df <- 
  foreign::read.dta("https://stats.idre.ucla.edu/stat/data/ologit.dta") %>% 
  mutate(public = ifelse(public == 1, "Public", "Private"))

# run model
mod1 <- polr(data = df, formula = apply ~ public, Hess = TRUE)

broom::tidy(mod1, p.values = TRUE)
#> # A tibble: 3 x 6
#>   term                        estimate std.error statistic p.value coef.type  
#>   <chr>                          <dbl>     <dbl>     <dbl>   <dbl> <chr>      
#> 1 publicPublic                   0.183     0.284     0.646   0.520 coefficient
#> 2 unlikely|somewhat likely       0.225     0.108     2.09   NA     scale      
#> 3 somewhat likely|very likely    2.22      0.172    12.9    NA     scale

# shows OR and 95% CI but no p value
tbl_regression(
  mod1, 
  exponentiate = TRUE,
  tidy_fun = function(x, ...) broom::tidy(x, ..., p.values = TRUE)
) 
