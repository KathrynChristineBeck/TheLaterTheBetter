library(tidyverse)
library(devtools)
library(lubridate)
library(zoo)
library(janitor)
library(fastDummies)


### shifting births from 1995-2018 ########

# load holiday data


# restrict to Norway and until 2018
holidays <- holidays %>% 
  filter(country=="NO") %>% 
  filter(year <= 2018)


# import population data


# select cohorts
population <- population %>% 
  filter(birth_year >=1995 & birth_year <=2018)

# keep those born in Norway to Norwegian parents
population <- population %>% 
  filter(invkat =="A")

# remove few with missing ID 
population <- population %>% 
  filter(!is.na(id))

# check for duplicates
population %>% get_dupes(id)

# rename data
data <- population

# convert birthdate to date object
data$birthdate<-as.Date(as.character(data$birthdate), "%Y%m%d")


# date objects
data <- data %>% 
  mutate(yob = lubridate::year(birthdate),
         mob = lubridate::month(birthdate),
         dob = lubridate::day(birthdate))

# set universal year of leapyear for those born on a leapday
data$birth_monthday <- make_datetime(year = 2016, month = data$mob, day = data$dob)


# recenter year around January 1
data<- data %>% 
  mutate(newyear = year(birth_monthday) - as.integer(month(birth_monthday)>=7))

cutoff <- as.Date("2016-01-01")

data$cutoffdate <- make_datetime(year = data$newyear, month = data$mob, day = data$dob)

# calculate distance to cutoff
data$diff<-difftime(data$cutoffdate,cutoff,units = c("days"))
data$cutoff<-as.numeric(difftime(data$cutoffdate,cutoff,units = c("days")))

#create a dummy for being born after Dec 31st
data <- data %>%
  mutate(january = case_when(cutoff < 0 ~ 0, cutoff >= 0 ~ 1))

# recenter birth cohorts to July-June for birth year dummies
data <- data %>% 
  mutate(recenter_cohort = case_when(mob<=6~yob-1, mob>=7~yob))

data %>% tabyl(mob, recenter_cohort, yob)

# create dummies
data <- dummy_cols(data, select_columns = "recenter_cohort")


# turn holiday variable into date
holidays$date <- as.Date(holidays$ds, format="%Y-%m-%d")

# create variable for which holiday
data$holiday <- ifelse(data$birthdate %in% holidays$date, holidays$holiday[match(data$birthdate, holidays$date)], 0)

# create dummy for each holiday
data <- dummy_cols(.data = data, select_columns =  c("holiday"))

# remove dummmy for NA
data$holiday_0 <- NULL

# rename holidays
data <- data %>% 
  rename("newyearday" = `holiday_New Year's Day`) %>% 
  rename("skjærtors" = `holiday_Maundy Thursday`) %>% 
  rename("langfre" = `holiday_Good Friday`) %>% 
  rename("firsteaster" = `holiday_Easter Sunday`) %>% 
  rename("secondeaster" = `holiday_Easter Monday`) %>% 
  rename("arbeiddag" = `holiday_Labor Day`) %>% 
  rename("grunndag" = `holiday_Constitution Day`) %>% 
  rename("ascen" = `holiday_Ascension Day`) %>% 
  rename("whitsun" = `holiday_Whit Sunday`) %>% 
  rename("whitmon" = `holiday_Whit Monday`) %>% 
  rename("christmas" = `holiday_Christmas Day`) %>% 
  rename("andrechristmas" = `holiday_Second Day of Christmas`)

# create variable for day of week of birthday
data <- data %>% 
  mutate(weekday = wday(birthdate,label = T, abbr = F))

# create dummy for day of week
data <- dummy_cols(.data = data, select_columns =  c("weekday"))

# make leap day dummy
data <- data %>% 
  mutate(leapday = case_when((mob==2 & dob==29)~1, TRUE~0))







## birth shifting analysis ######

# count births by day and year
data <- data %>% 
  group_by(cutoff, recenter_cohort) %>% 
  mutate(count = n()) %>% 
  ungroup()


data %>% tabyl(recenter_cohort)

## regression ####


### 7 day window ####
# select 7 days around cutoff, and count number of births each day by year (January first is 0 days from cutoff)
window_7 <-
  data %>% 
  filter(cutoff<= 6 & cutoff>=(-7))  %>%
  group_by(cutoff, recenter_cohort, weekday, holiday) %>%
  summarise(count = n())

# create January dummy variable for days after Dec 31
window_7 <-
  window_7 %>%
  mutate(january = case_when(cutoff >= 0 ~ 1, TRUE ~ 0))

# make dummy columns for recenter_cohort
window_7 <-
  dummy_cols(.data = window_7,
             select_columns =  c("recenter_cohort"))

# make dummy columns for weekday
window_7 <-
  dummy_cols(.data = window_7,
             select_columns =  c("weekday"))

# make dummy columns for holiday
window_7 <-
  dummy_cols(.data = window_7,
             select_columns =  c("holiday"))

# remove holiday = 0 dummy
window_7$holiday_0<- NULL

# rename holidays within window
window_7 <- window_7 %>% 
  rename("newyearday" = `holiday_New Year's Day`) %>% 
  rename("christmas" = `holiday_Christmas Day`) %>% 
  rename("andrechristmas" = `holiday_Second Day of Christmas`)

# run regression controlling for year, day of week, and holiday fixed effects
model <- lm(count~january+recenter_cohort_1994+recenter_cohort_1995+recenter_cohort_1996+
              recenter_cohort_1997+recenter_cohort_1998+recenter_cohort_1999+recenter_cohort_2000+recenter_cohort_2001+recenter_cohort_2002+
              recenter_cohort_2003+recenter_cohort_2004+recenter_cohort_2005+
              recenter_cohort_2006+recenter_cohort_2007+recenter_cohort_2008+
              recenter_cohort_2009+recenter_cohort_2010+recenter_cohort_2011+recenter_cohort_2012+recenter_cohort_2013+
              recenter_cohort_2014+recenter_cohort_2015+recenter_cohort_2016+recenter_cohort_2017+recenter_cohort_2018 + weekday_søndag + weekday_mandag + 
              weekday_tirsdag + weekday_onsdag + weekday_torsdag + weekday_fredag + weekday_lørdag + newyearday + christmas + andrechristmas , data=window_7)

summary(model)

coeftest(model, vcov. = vcovHC(model, type = "HC1"))


# number of births moved
7 * (model[["coefficients"]]["january"]/2)

## share of births
# run regression controlling for year fixed effects, day of week, holiday
model <- lm(log(count)~january+recenter_cohort_1994+recenter_cohort_1995+recenter_cohort_1996+
              recenter_cohort_1997+recenter_cohort_1998+recenter_cohort_1999+recenter_cohort_2000+recenter_cohort_2001+recenter_cohort_2002+
              recenter_cohort_2003+recenter_cohort_2004+recenter_cohort_2005+
              recenter_cohort_2006+recenter_cohort_2007+recenter_cohort_2008+
              recenter_cohort_2009+recenter_cohort_2010+recenter_cohort_2011+recenter_cohort_2012+recenter_cohort_2013+
              recenter_cohort_2014+recenter_cohort_2015+recenter_cohort_2016+recenter_cohort_2017+recenter_cohort_2018+ weekday_søndag + weekday_mandag + 
              weekday_tirsdag + weekday_onsdag + weekday_torsdag + weekday_fredag + weekday_lørdag + newyearday + christmas + andrechristmas, data=window_7)

summary(model)


coeftest(model, vcov. = vcovHC(model, type = "HC1"))

# share of births moved
(exp(model[["coefficients"]]["january"]/2)-1)*100



### 15 day window #####
# select 15 days around cutoff, and count number of births each day by year
window_15 <-
  data %>% 
  filter(cutoff<= 14 & cutoff>=(-15))  %>%
  group_by(cutoff, recenter_cohort, weekday, holiday) %>%
  summarise(count = n())

# create january dummy variable for days after Dec 31
window_15 <-
  window_15 %>%
  mutate(january = case_when(cutoff >= 0 ~ 1, TRUE ~ 0))

# make dummy columns for recenter_cohort
window_15 <-
  dummy_cols(.data = window_15,
             select_columns =  c("recenter_cohort"))

# make dummy columns for weekday
window_15 <-
  dummy_cols(.data = window_15,
             select_columns =  c("weekday"))

# make dummy columns for holiday
window_15 <-
  dummy_cols(.data = window_15,
             select_columns =  c("holiday"))

# remove holiday = 0 dummy
window_15$holiday_0<- NULL

# rename holidays within window
window_15 <- window_15 %>% 
  rename("newyearday" = `holiday_New Year's Day`) %>% 
  rename("christmas" = `holiday_Christmas Day`) %>% 
  rename("andrechristmas" = `holiday_Second Day of Christmas`)


# run regression controlling for year fixed effects, day of week, holiday
model <- lm(count~january+recenter_cohort_1994+recenter_cohort_1995+recenter_cohort_1996+
              recenter_cohort_1997+recenter_cohort_1998+recenter_cohort_1999+recenter_cohort_2000+recenter_cohort_2001+recenter_cohort_2002+
              recenter_cohort_2003+recenter_cohort_2004+recenter_cohort_2005+
              recenter_cohort_2006+recenter_cohort_2007+recenter_cohort_2008+
              recenter_cohort_2009+recenter_cohort_2010+recenter_cohort_2011+recenter_cohort_2012+recenter_cohort_2013+
              recenter_cohort_2014+recenter_cohort_2015+recenter_cohort_2016+recenter_cohort_2017+recenter_cohort_2018 + weekday_søndag + weekday_mandag + 
              weekday_tirsdag + weekday_onsdag + weekday_torsdag + weekday_fredag + weekday_lørdag + newyearday + christmas + andrechristmas , data=window_15)

summary(model)

coeftest(model, vcov. = vcovHC(model, type = "HC1"))


# number of births moved
15 * (model[["coefficients"]]["january"]/2)



## share of births moved

# run regression controlling for year fixed effects, day of week, holiday
model <- lm(log(count)~january+recenter_cohort_1994+recenter_cohort_1995+recenter_cohort_1996+
              recenter_cohort_1997+recenter_cohort_1998+recenter_cohort_1999+recenter_cohort_2000+recenter_cohort_2001+recenter_cohort_2002+
              recenter_cohort_2003+recenter_cohort_2004+recenter_cohort_2005+
              recenter_cohort_2006+recenter_cohort_2007+recenter_cohort_2008+
              recenter_cohort_2009+recenter_cohort_2010+recenter_cohort_2011+recenter_cohort_2012+recenter_cohort_2013+
              recenter_cohort_2014+recenter_cohort_2015+recenter_cohort_2016+recenter_cohort_2017+recenter_cohort_2018+ weekday_søndag + weekday_mandag + 
              weekday_tirsdag + weekday_onsdag + weekday_torsdag + weekday_fredag + weekday_lørdag + newyearday + christmas + andrechristmas, data=window_15)

summary(model)


coeftest(model, vcov. = vcovHC(model, type = "HC1"))



# share of births moved
(exp(model[["coefficients"]]["january"]/2)-1)*100



#### 30 day window ######
# select 30 days around cutoff, and count number of births each day by year
window_30 <-
  data %>% 
  filter(cutoff<= 29 & cutoff>=(-30))  %>%
  group_by(cutoff, recenter_cohort, weekday, holiday) %>%
  summarise(count = n())

# create january dummy variable for days after Dec 31
window_30 <-
  window_30 %>%
  mutate(january = case_when(cutoff >= 0 ~ 1, TRUE ~ 0))

# make dummy columns for recenter_cohort
window_30 <-
  dummy_cols(.data = window_30,
             select_columns =  c("recenter_cohort"))


# make dummy columns for weekday
window_30 <-
  dummy_cols(.data = window_30,
             select_columns =  c("weekday"))

# make dummy columns for holiday
window_30 <-
  dummy_cols(.data = window_30,
             select_columns =  c("holiday"))

# remove holiday = 0 dummy
window_30$holiday_0<- NULL

# rename holidays
window_30 <- window_30 %>% 
  rename("newyearday" = `holiday_New Year's Day`) %>% 
  rename("christmas" = `holiday_Christmas Day`) %>% 
  rename("andrechristmas" = `holiday_Second Day of Christmas`)


# run regression controlling for year fixed effects, day of week, holiday
model <- lm(count~january+recenter_cohort_1994+recenter_cohort_1995+recenter_cohort_1996+
              recenter_cohort_1997+recenter_cohort_1998+recenter_cohort_1999+recenter_cohort_2000+recenter_cohort_2001+recenter_cohort_2002+
              recenter_cohort_2003+recenter_cohort_2004+recenter_cohort_2005+
              recenter_cohort_2006+recenter_cohort_2007+recenter_cohort_2008+
              recenter_cohort_2009+recenter_cohort_2010+recenter_cohort_2011+recenter_cohort_2012+recenter_cohort_2013+
              recenter_cohort_2014+recenter_cohort_2015+recenter_cohort_2016+recenter_cohort_2017+recenter_cohort_2018 + weekday_søndag + weekday_mandag + 
              weekday_tirsdag + weekday_onsdag + weekday_torsdag + weekday_fredag + weekday_lørdag + newyearday + christmas + andrechristmas , data=window_30)

summary(model)


coeftest(model, vcov. = vcovHC(model, type = "HC1"))



# number of births moved
30 * (model[["coefficients"]]["january"]/2)




## share of births

# run regression controlling for year fixed effects, day of week, holiday
model <- lm(log(count)~january+recenter_cohort_1994+recenter_cohort_1995+recenter_cohort_1996+
              recenter_cohort_1997+recenter_cohort_1998+recenter_cohort_1999+recenter_cohort_2000+recenter_cohort_2001+recenter_cohort_2002+
              recenter_cohort_2003+recenter_cohort_2004+recenter_cohort_2005+
              recenter_cohort_2006+recenter_cohort_2007+recenter_cohort_2008+
              recenter_cohort_2009+recenter_cohort_2010+recenter_cohort_2011+recenter_cohort_2012+recenter_cohort_2013+
              recenter_cohort_2014+recenter_cohort_2015+recenter_cohort_2016+recenter_cohort_2017+recenter_cohort_2018+ weekday_søndag + weekday_mandag + 
              weekday_tirsdag + weekday_onsdag + weekday_torsdag + weekday_fredag + weekday_lørdag + newyearday + christmas + andrechristmas, data=window_30)

summary(model)


coeftest(model, vcov. = vcovHC(model, type = "HC1"))



# share of births moved
(exp(model[["coefficients"]]["january"]/2)-1)*100



#### 60 day window #####

# select 60 days around cutoff, and count number of births each day by year
window_60 <-
  data %>% 
  filter(cutoff<= 59 & cutoff>=(-60))  %>%
  group_by(cutoff, recenter_cohort, weekday, holiday) %>%
  summarise(count = n())

# create january variable for days after Dec 31
window_60 <-
  window_60 %>%
  mutate(january = case_when(cutoff >= 0 ~ 1, TRUE ~ 0))

# make dummy columns for recenter_cohort
window_60 <-
  dummy_cols(.data = window_60,
             select_columns =  c("recenter_cohort"))


# make dummy columns for weekday
window_60 <-
  dummy_cols(.data = window_60,
             select_columns =  c("weekday"))

# make dummy columns for holiday
window_60 <-
  dummy_cols(.data = window_60,
             select_columns =  c("holiday"))

# remove holiday = 0 dummy
window_60$holiday_0<- NULL

# rename holidays
window_60 <- window_60 %>% 
  rename("newyearday" = `holiday_New Year's Day`) %>% 
  rename("christmas" = `holiday_Christmas Day`) %>% 
  rename("andrechristmas" = `holiday_Second Day of Christmas`)


# run regression controlling for year fixed effects, day of week, holiday
model <- lm(count~january+recenter_cohort_1994+recenter_cohort_1995+recenter_cohort_1996+
              recenter_cohort_1997+recenter_cohort_1998+recenter_cohort_1999+recenter_cohort_2000+recenter_cohort_2001+recenter_cohort_2002+
              recenter_cohort_2003+recenter_cohort_2004+recenter_cohort_2005+
              recenter_cohort_2006+recenter_cohort_2007+recenter_cohort_2008+
              recenter_cohort_2009+recenter_cohort_2010+recenter_cohort_2011+recenter_cohort_2012+recenter_cohort_2013+
              recenter_cohort_2014+recenter_cohort_2015+recenter_cohort_2016+recenter_cohort_2017+recenter_cohort_2018 + weekday_søndag + weekday_mandag + 
              weekday_tirsdag + weekday_onsdag + weekday_torsdag + weekday_fredag + weekday_lørdag + newyearday + christmas + andrechristmas , data=window_60)

summary(model)


coeftest(model, vcov. = vcovHC(model, type = "HC1"))



# number of births moved
60 * (model[["coefficients"]]["january"]/2)




## share of births moved

# run regression controlling for year fixed effects, day of week, holiday
model <- lm(log(count)~january+recenter_cohort_1994+recenter_cohort_1995+recenter_cohort_1996+
              recenter_cohort_1997+recenter_cohort_1998+recenter_cohort_1999+recenter_cohort_2000+recenter_cohort_2001+recenter_cohort_2002+
              recenter_cohort_2003+recenter_cohort_2004+recenter_cohort_2005+
              recenter_cohort_2006+recenter_cohort_2007+recenter_cohort_2008+
              recenter_cohort_2009+recenter_cohort_2010+recenter_cohort_2011+recenter_cohort_2012+recenter_cohort_2013+
              recenter_cohort_2014+recenter_cohort_2015+recenter_cohort_2016+recenter_cohort_2017+recenter_cohort_2018+ weekday_søndag + weekday_mandag + 
              weekday_tirsdag + weekday_onsdag + weekday_torsdag + weekday_fredag + weekday_lørdag + newyearday + christmas + andrechristmas, data=window_60)

summary(model)


coeftest(model, vcov. = vcovHC(model, type = "HC1"))



# share of births moved
(exp(model[["coefficients"]]["january"]/2)-1)*100



