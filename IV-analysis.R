### IV analysis using due date for effect of SSA on ADHD and academic skills ######

# load packages
library(tidyverse)
library(janitor)
library(ivDiag)
library(lmtest)
library(sandwich)

# load sample

## sample includes all individuals born in Norway between 1999 and 2007 to two Norwegian-born parents and who were alive and living in Norway
## between the ages of 7 and 16 or until 2021. Those missing information on 5th grade national tests and missing due date from ultrasound were excluded


### ADHD #####


# select cohorts - diagnoses between 7 and 15 - select those with due date between 1999 and 2004
adhd <- data %>% 
  filter(term_year >= 1999 & term_year <= 2004)


# make dummy variable for if obs received a diagnosis for ADHD in either the primary care (KUHR) or national patient register
adhd <- adhd %>% 
  mutate(adhd_diag = case_when(adhd_npr==1 | adhd_kuhr==1~1, TRUE~0))


# using ivDiag for IV analysis

model_adhd <- ivDiag(data=adhd, Y="adhd_diag", D="SSA", Z="cutoff", FE = c("recenter_cohort_1998","recenter_cohort_1999","recenter_cohort_2000","recenter_cohort_2001",
                                                                      "recenter_cohort_2002","recenter_cohort_2003","recenter_cohort_2004"))


# percentage risk reduction
risk_adhd <- adhd  %>% summarise(mean = mean(adhd_diag))

((model_adhd$est_2sls[1,1])/risk_adhd$mean)*100

# f stat
model_adhd$F_stat

# first stage
format(model_adhd$est_fs, digits = 7)

# tsls
model_adhd$est_2sls

#ols
format(model_adhd$est_ols, digits = 7)

# plot coefficients
plot_coef(model_adhd)


# ols
model_ols_adhd <- lm(adhd_diag~SSA+recenter_cohort_1998+recenter_cohort_1999+
                  recenter_cohort_2000+recenter_cohort_2001+recenter_cohort_2002+
                  recenter_cohort_2003+recenter_cohort_2004, data = adhd)

summary(model_ols_adhd)

coeftest(model_ols_adhd, vcov. = vcovHC(model_ols_adhd, type = "HC1"))

# risk diff
risk_adhd <- adhd  %>% summarise(mean = mean(adhd_diag))

((model_ols_adhd[["coefficients"]]["SSA"])/risk_adhd$mean)*100




#### National Test Scores ####

# load data on national tests


# gather Sami tests into reading tests
tests <- tests %>% 
  mutate(test  = case_when((PROVE == "NPENG05")~"ENG_05",
                           (PROVE == "NPENG08")~"ENG_08",
                           (PROVE == "NPLES05" | PROVE=="NPLSA05" | PROVE == "NPNSA05" | PROVE == "NPSSA05")~"READ_05",
                           (PROVE == "NPLES08" | PROVE=="NPLSA08" | PROVE == "NPNSA08")~"READ_08",
                           (PROVE == "NPLES09" | PROVE == "NPNSA09" | PROVE == "NPSSA09")~"READ_09",
                           (PROVE == "NPREG05")~"MATH_05",
                           (PROVE == "NPREG08")~"MATH_08",
                           (PROVE == "NPREG09")~"MATH_09"))

# create temp var to use for removing duplicates
tests <- tests %>% 
  mutate(point_dupe = case_when(year>=2014~SKALAPOENG, year<2014~POENG))

# check for duplicates by id, test, and year
tests %>% get_dupes(id, test, year) %>% print(width=Inf)

# drop duplicates and keep highest point score
tests <- tests %>% 
  group_by(id, year, test) %>% 
  slice_max(order_by = point_dupe, with_ties = F) %>% 
  ungroup()

tests$point_dupe <- NULL

# standardize score for each year and test and create z-score
tests <- tests %>% 
  group_by(year, test) %>% 
  mutate(pointmean= case_when((year>=2014~mean(SKALAPOENG, na.rm=T)), (year<2014~mean(POENG, na.rm=T)))) %>% 
  mutate(pointsd= case_when((year>=2014~sd(SKALAPOENG, na.rm=T)), (year<2014~sd(POENG, na.rm=T)))) %>% 
  mutate(pointstd= case_when((year>=2014~((SKALAPOENG-pointmean)/pointsd)), (year<2014~((POENG-pointmean)/pointsd)))) %>% 
  ungroup()

# select variables
tests <- tests %>% 
  select(test, pointstd, id)

# create df for each test

read_05 <- tests %>% 
  filter(test == "READ_05")

read_09 <- tests %>% 
  filter(test == "READ_09")

math_05 <- tests %>% 
  filter(test == "MATH_05")


math_09 <- tests %>% 
  filter(test == "MATH_09")


# use entire analytical sample


## Math 5th grade 

# merge sample with data on 5th grade math national test scores
math_05 <- left_join(data, math_05, by=id)

# check duplicates
math_05 %>% get_dupes(id)

# drop duplicates
math_05 <- math_05 %>% distinct(id, .keep_all = T)


# IV analysis
model_math05 <- ivDiag(data=math_05, Y="pointstd", D="SSA", Z="cutoff", FE = c("recenter_cohort_1998","recenter_cohort_1999","recenter_cohort_2000","recenter_cohort_2001",
                                                                                    "recenter_cohort_2002","recenter_cohort_2003","recenter_cohort_2004",
                                                                                    "recenter_cohort_2005","recenter_cohort_2006","recenter_cohort_2007"),
                       bootstrap = F)
# f stat
model_math05$F_stat

# first stage
model_math05$est_fs

# tsls
model_math05$est_2sls

#ols
model_math05$est_ols

# plot coefficients
plot_coef(model_math05)




## Reading 5th grade 

# merge sample with data on 5th grade reading national test scores
read_05 <- left_join(data, read_05, by=id)

# check duplicates
read_05 %>% get_dupes(id)

# drop duplicates
read_05 <- read_05 %>% distinct(id, .keep_all = T)


# IV analysis
model_read05 <- ivDiag(data=read_05, Y="pointstd", D="SSA", Z="cutoff", FE = c("recenter_cohort_1998","recenter_cohort_1999","recenter_cohort_2000","recenter_cohort_2001",
                                                                               "recenter_cohort_2002","recenter_cohort_2003","recenter_cohort_2004",
                                                                               "recenter_cohort_2005","recenter_cohort_2006","recenter_cohort_2007"),
                       bootstrap = F)
# f stat
model_read05$F_stat

# first stage
model_read05$est_fs

# tsls
model_read05$est_2sls

#ols
model_read05$est_ols

# plot coefficients
plot_coef(model_read05)



## Math 9th grade 

# merge sample with data on 9th grade math national test scores
math_09 <- left_join(data, math_09, by=id)

# check duplicates
math_09 %>% get_dupes(id)

# drop duplicates
math_09 <- math_09 %>% distinct(id, .keep_all = T)


# IV analysis
model_math09 <- ivDiag(data=math_09, Y="pointstd", D="SSA", Z="cutoff", FE = c("recenter_cohort_1998","recenter_cohort_1999","recenter_cohort_2000","recenter_cohort_2001",
                                                                               "recenter_cohort_2002","recenter_cohort_2003","recenter_cohort_2004",
                                                                               "recenter_cohort_2005","recenter_cohort_2006","recenter_cohort_2007"),
                       bootstrap = F)
# f stat
model_math09$F_stat

# first stage
model_math09$est_fs

# tsls
model_math09$est_2sls

#ols
model_math09$est_ols

# plot coefficients
plot_coef(model_math09)




## Reading 9th grade 

# merge sample with data on 9th grade reading national test scores
read_09 <- left_join(data, read_09, by=id)

# check duplicates
read_09 %>% get_dupes(id)

# drop duplicates
read_09 <- read_09 %>% distinct(id, .keep_all = T)


# IV analysis
model_read09 <- ivDiag(data=read_09, Y="pointstd", D="SSA", Z="cutoff", FE = c("recenter_cohort_1998","recenter_cohort_1999","recenter_cohort_2000","recenter_cohort_2001",
                                                                               "recenter_cohort_2002","recenter_cohort_2003","recenter_cohort_2004",
                                                                               "recenter_cohort_2005","recenter_cohort_2006","recenter_cohort_2007"),
                       bootstrap = F)
# f stat
model_read09$F_stat

# first stage
model_read09$est_fs

# tsls
model_read09$est_2sls

#ols
model_read09$est_ols

# plot coefficients
plot_coef(model_read09)

