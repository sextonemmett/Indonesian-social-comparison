rm(list = ls())
library(dplyr)
library(tidyr)
library(ggplot2)
library(haven)
library(readr)
library(readxl)

setwd('/Users/emmettsexton/Dropbox/Mac (2)/Desktop/BSE/Trimester 3/04 Social Econ/Term paper')

## INDIVIDUAL TRACKING ##
# p_trk_07 <- read_dta('./01 Data Processing/01 Data/IFLS 2007/hh07_trk_dta/ptrack.dta')
# p_trk_07 <- p_trk_07 %>% 
#  select(pidlink, weight = pw9307lr)
p_trk_14 <- read_dta('./01 Data Processing/01 Data/IFLS 2014/hh14_trk_dta/ptrack.dta')
p_trk_14 <- p_trk_14 %>% 
  select(pidlink, weight = pwt_5_waves_lr)

## HH PROVINCE and KABUPATEN ##
hh_trk_07 <- read_dta('./01 Data Processing/01 Data/IFLS 2007/hh07_trk_dta/htrack.dta')
hh_trk_07 <- hh_trk_07 %>% 
  select(hhid07, province = sc010700, kabu = sc020700) %>% 
  filter(hhid07 != "")

hh_trk_14 <- read_dta('./01 Data Processing/01 Data/IFLS 2014/hh14_trk_dta/htrack.dta')
hh_trk_14 <- hh_trk_14 %>% 
  select(hhid14, province = sc01_14_00, kabu = sc02_14_00) %>% 
  filter(hhid14 != "")

## HH CONSUMPTION ##
# Food consumption by household for last week
hh_ks1_07 <- read_dta('./01 Data Processing/01 Data/IFLS 2007/hh07_b1_dta/b1_ks1.dta')
hh_ks1_07 <- hh_ks1_07 %>%
  mutate(food_consump = ks02 + ks03) %>% 
  select(hhid07, food_consump) %>% 
  group_by(hhid07) %>% 
    # Multiplying by 4 to match the monthly non-food consumption estimates
    summarize(food_consump = 4*sum(food_consump, na.rm = TRUE)) %>% 
  ungroup()

hh_ks1_14 <- read_dta('./01 Data Processing/01 Data/IFLS 2014/hh14_b1_dta/b1_ks1.dta')
hh_ks1_14 <- hh_ks1_14 %>%
  mutate(food_consump = ks02 + ks03) %>% 
  select(hhid14, food_consump) %>% 
  group_by(hhid14) %>% 
    # Multiplying by 4 to match the monthly non-food consumption estimates
    summarize(food_consump = 4*sum(food_consump, na.rm = TRUE)) %>% 
  ungroup()

# Non-food by household for last month
hh_ks2_07 <- read_dta('./01 Data Processing/01 Data/IFLS 2007/hh07_b1_dta/b1_ks2.dta')
hh_ks2_07 <- hh_ks2_07 %>%
  select(hhid07, ks06) %>% 
  group_by(hhid07) %>% 
    summarize(non_food_consump = sum(ks06, na.rm = TRUE)) %>% 
  ungroup()

hh_ks2_14 <- read_dta('./01 Data Processing/01 Data/IFLS 2014/hh14_b1_dta/b1_ks2.dta')
hh_ks2_14 <- hh_ks2_14 %>%
  select(hhid14, ks06) %>% 
  group_by(hhid14) %>% 
    summarize(non_food_consump = sum(ks06, na.rm = TRUE)) %>% 
  ungroup()

## AGE and SEX ##
cov_07 <- read_dta('./01 Data Processing/01 Data/IFLS 2007/hh07_b3a_dta/b3a_cov.dta')
cov_07 <- cov_07 %>% 
  select(pidlink, hhid07, age, sex) %>% 
  mutate(age_grp = case_when(age < 18 ~ "<18",
                             age < 35 ~ "18-34",
                             age < 55 ~ "35-54",
                             age >= 55 ~ "55+"),
         sex = case_when(sex == 1 ~ 'Male',
                         sex == 3 ~ 'Female'))

cov_14 <- read_dta('./01 Data Processing/01 Data/IFLS 2014/hh14_b3a_dta/b3a_cov.dta')
cov_14 <- cov_14 %>% 
  select(pidlink, hhid14, age, sex) %>% 
  mutate(age_grp = case_when(age < 18 ~ "<18",
                             age < 35 ~ "18-34",
                             age < 55 ~ "35-54",
                             age >= 55 ~ "55+"),
         sex = case_when(sex == 1 ~ 'Male',
                         sex == 3 ~ 'Female'))

## EDUCATION and ETHNICITY ##
edu_07 <- read_dta('./01 Data Processing/01 Data/IFLS 2007/hh07_b3a_dta/b3a_dl1.dta')
edu_07 <- edu_07 %>% 
  select(pidlink, highest_edu = dl06, ethnicity = dl01e) %>% 
  mutate(edu_grp = case_when(highest_edu == '2' ~ 'Elementary',
                             highest_edu %in% c('3', '4') ~ 'Junior High',
                             highest_edu %in% c('5', '6') ~ 'Senior High',
                             highest_edu %in% c('60', '61', '62', '63') ~ 'College/University',
                             TRUE ~ 'Other')) %>% 
  select(pidlink, edu_grp, ethnicity)

edu_14 <- read_dta('./01 Data Processing/01 Data/IFLS 2014/hh14_b3a_dta/b3a_dl1.dta')
edu_14 <- edu_14 %>% 
  select(pidlink, highest_edu = dl06, ethnicity = dl01e) %>% 
  mutate(highest_edu = as.character(highest_edu)) %>% 
  mutate(edu_grp = case_when(highest_edu == '2' ~ 'Elementary',
                             highest_edu %in% c('3', '4') ~ 'Junior High',
                             highest_edu %in% c('5', '6') ~ 'Senior High',
                             highest_edu %in% c('60', '61', '62', '63') ~ 'College/University',
                             TRUE ~ 'Other'))%>% 
  select(pidlink, edu_grp, ethnicity)

## WELL-BEING OUTCOMES ##
sw_07 <- read_dta('./01 Data Processing/01 Data/IFLS 2007/hh07_b3a_dta/b3a_sw.dta')
sw_07 <- sw_07 %>% 
  select(pidlink, sw_econ_ladder = sw01, sw_curr_std_liv = sw04, sw_food_consump = sw05, sw_happy = sw12)

sw_14 <- read_dta('./01 Data Processing/01 Data/IFLS 2014/hh14_b3a_dta/b3a_sw.dta')
sw_14 <- sw_14 %>% 
  select(pidlink, sw_econ_ladder = sw01, sw_curr_std_liv = sw04, sw_food_consump = sw05, sw_happy = sw12)

## INCOME ##
tk_07 <- read_dta('./01 Data Processing/01 Data/IFLS 2007/hh07_b3a_dta/b3a_tk2.dta')
tk_07 <- tk_07 %>% 
  rename(wage_salary = tk25a2, 
         business_profit = tk26a3) %>% 
  select(pidlink, wage_salary, business_profit)
# If an individual has a positive income, it is either from wages/salary OR self-employment
tk_07 %>% filter(!is.na(business_profit)) %>% nrow()
tk_07 %>% filter(!is.na(wage_salary)) %>% nrow()
tk_07 %>% filter(!is.na(wage_salary)) %>% nrow()
tk_07 %>% filter(!is.na(business_profit) & !is.na(wage_salary)) %>% nrow()

tk_14 <- read_dta('./01 Data Processing/01 Data/IFLS 2014/hh14_b3a_dta/b3a_tk2.dta')
tk_14 <- tk_14 %>% 
  rename(wage_salary = tk25a2, 
         business_profit = tk26a3) %>% 
  select(pidlink, wage_salary, business_profit)
# If an individual has a positive income, it is either from wages/salary OR self-employment
tk_14 %>% filter(!is.na(business_profit)) %>% nrow()
tk_14 %>% filter(!is.na(wage_salary)) %>% nrow()
tk_14 %>% filter(!is.na(business_profit) & !is.na(wage_salary)) %>% nrow()

## MERGE ##
hh_07 <- full_join(hh_trk_07, hh_ks1_07, by = 'hhid07')
hh_07 <- full_join(hh_07, hh_ks2_07, by = 'hhid07')
hh_07 <- hh_07 %>% filter(!is.na(province))
hh_07 <- right_join(hh_07, cov_07, by = 'hhid07')
hh_07 <- left_join(hh_07, edu_07, by = 'pidlink')
hh_07 <- left_join(hh_07, sw_07, by = 'pidlink')
hh_07 <- left_join(hh_07, tk_07, by = 'pidlink')
hh_07 <- left_join(hh_07, p_trk_14, by = 'pidlink')

hh_07 <- hh_07 %>% 
  mutate(indiv_income = case_when(!is.na(wage_salary) ~ wage_salary, 
                            TRUE ~ business_profit)) %>% 
  group_by(hhid07) %>% 
    mutate(hh_income = sum(indiv_income, na.rm = TRUE),
           hh_members = n()) %>% 
  ungroup() %>% 
  mutate(pc_consump = (food_consump + non_food_consump)/hh_members,
         pc_income = hh_income/hh_members,
         year = '2007') %>% 
  rename(hhid = hhid07)

hh_14 <- full_join(hh_trk_14, hh_ks1_14, by = 'hhid14')
hh_14 <- full_join(hh_14, hh_ks2_14, by = 'hhid14')
hh_14 <- hh_14 %>% filter(!is.na(province))
hh_14 <- right_join(hh_14, cov_14, by = 'hhid14')
hh_14 <- left_join(hh_14, edu_14, by = 'pidlink')
hh_14 <- left_join(hh_14, sw_14, by = 'pidlink')
hh_14 <- left_join(hh_14, tk_14, by = 'pidlink')
hh_14 <- left_join(hh_14, p_trk_14, by = 'pidlink')

hh_14 <- hh_14 %>% 
  mutate(indiv_income = case_when(!is.na(wage_salary) ~ wage_salary, 
                            TRUE ~ business_profit)) %>% 
  group_by(hhid14) %>% 
    mutate(hh_income = sum(indiv_income, na.rm = TRUE),
           hh_members = n()) %>% 
  ungroup() %>% 
  mutate(pc_consump = (food_consump + non_food_consump)/hh_members,
         pc_income = hh_income/hh_members,
         year = '2014') %>% 
  rename(hhid = hhid14)

df <- bind_rows(hh_07, hh_14)
df <- df %>% 
  filter(!is.na(sw_happy)) %>% 
  filter(age != 999) %>% 
  filter(!is.na(weight))

ggplot(data = df) + geom_point(aes(x= log(pc_income), y = log(pc_consump)))
ggplot(data = df %>% filter(pc_income == 0)) + geom_histogram(aes(x= age))
ggplot(data = df %>% filter(pc_income != 0)) + geom_histogram(aes(x= age))

write_csv(df, file = './01 Data Processing/03 Temp/Main data for 2007 and 2014.csv')




