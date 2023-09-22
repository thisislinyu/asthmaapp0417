#---------libs
library(readxl)
library(tidyverse)

t1_f <- function(dat = workdat1,
                 vars = vars,
                 num_vars = num_vars,
                 factor_vars= factor_vars,
                 showlevel_p=T,
                 strata=strata){
  
  library(jstable)
  library(dplyr)
  library(DT)
  
  ### data type character, factor and numeric, you need to specify the data types before run the t1_f
  
  
  
  #dat_new <- dat1[ ,colSums(is.na(dat1)) <nrow(dat1)]  ## remove the columns with only NAs
  
  #vars <-  dput(c(dput(colnames(dat_new[sapply(dat_new,is.factor)])),dput(colnames(dat_new[sapply(dat_new, is.numeric)]))))
  
  #factor_vars <- dput(colnames(dat_new[sapply(dat_new, is.factor)]))
  
  
  #num_vars <- dput(colnames(dat_new[sapply(dat_new, is.numeric)]))## not null num vars
  
  
  ## nonnorm variables
  # nonnorm_vars <- sapply(dat_new[num_vars],shapiro.test) %>% t() %>% data.frame() %>%
  # mutate(var_names = num_vars) %>% select(-data.name) %>% filter(p.value<0.05) 
  # nonnorm_vars <- dput(nonnorm_vars$var_names)
  
  
  
  t1_temp <- CreateTableOne2(data = dat,
                             strata = strata,
                             vars = vars,
                             factorVars = factor_vars,
                             includeNA = F,
                             showAllLevels = showlevel_p,
                             Labels = T,
                             # exact = factor_vars,
                             nonnormal = num_vars,
                             catDigits = 1,
                             contDigits = 2,
                             pDigits = 3,
  )
  t1_temp 
}