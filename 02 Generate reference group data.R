rm(list = ls())
library(dplyr)
library(tidyr)
library(ggplot2)
library(haven)
library(readr)
library(readxl)

setwd('/Users/emmettsexton/Dropbox/Mac (2)/Desktop/BSE/Trimester 3/04 Social Econ/Term paper')

# Read in processed IFLS 2007 and 2014 data
df <- read_csv('./01 Data Processing/03 Temp/Main data for 2007 and 2014.csv')

# Generate reference group incomes and consumption using the province level
df <- df %>% 
  group_by(year, province, age_grp, edu_grp, sex) %>% 
    mutate(ref_tot_income = sum(pc_income, na.rm = TRUE),
           ref_tot_consump = sum(pc_consump, na.rm = TRUE),
           prov_ref_members = n() - 1) %>% 
  ungroup() %>% 
  mutate(prov_ref_grp_income_pc = (ref_tot_income - pc_income)/prov_ref_members,
         prov_ref_grp_consump_pc = (ref_tot_consump - pc_consump)/prov_ref_members) %>% 
  # do the same using province-kabu level 
  group_by(year, province, kabu, age_grp, edu_grp, sex) %>% 
    mutate(ref_tot_income = sum(pc_income, na.rm = TRUE),
           ref_tot_consump = sum(pc_consump, na.rm = TRUE),
           kabu_ref_members = n() - 1) %>% 
  ungroup() %>% 
  mutate(kabu_ref_grp_income_pc = (ref_tot_income - pc_income)/kabu_ref_members,
         kabu_ref_grp_consump_pc = (ref_tot_consump - pc_consump)/kabu_ref_members) %>% 
  select(-ref_tot_consump, -ref_tot_income) %>% 
  # do the same using province-kabu and ethnicity
  group_by(year, province, kabu, ethnicity) %>% 
    mutate(ref_tot_income = sum(pc_income, na.rm = TRUE),
           ref_tot_consump = sum(pc_consump, na.rm = TRUE),
           ethnicity_ref_members = n() - 1) %>% 
  ungroup() %>% 
  mutate(eth_ref_grp_income_pc = (ref_tot_income - pc_income)/ethnicity_ref_members,
         eth_ref_grp_consump_pc = (ref_tot_consump - pc_consump)/ethnicity_ref_members) %>% 
  select(-ref_tot_consump, -ref_tot_income)

ggplot(df) + geom_histogram(aes(x = prov_ref_members))
ggplot(df) + geom_histogram(aes(x = kabu_ref_members))
ggplot(df) + geom_histogram(aes(x = ethnicity_ref_members))
  
df_out <- df %>% 
  mutate(temp = case_when(age > 17 & year == '2007' ~ 1,
                          TRUE ~ 0)) %>% 
  group_by(pidlink) %>% 
  mutate(temp_max = max(temp),
         cnt = n()) %>% 
  ungroup() %>% 
  # Only keep people that were 18+ in 2007 and were also in the 2014 survey
  filter(temp_max == 1 & cnt == 2)

# Inflate 2007 prices to 2014 prices: https://fred.stlouisfed.org/series/IDNCPIALLMINMEI
df_2014 <- df_out %>% filter(year == '2014')
df_2007 <- df_out %>% 
  filter(year == '2007') %>% 
  mutate(across(c(pc_income, prov_ref_grp_income_pc, kabu_ref_grp_income_pc, eth_ref_grp_income_pc,
                  pc_consump, prov_ref_grp_consump_pc, kabu_ref_grp_consump_pc, eth_ref_grp_consump_pc), ~.*(94.02/62.48)))

df_out <- bind_rows(df_2014, df_2007)

df_out <- df_out %>% 
  mutate(female = case_when(sex == 'Female' ~ 1,
                            TRUE ~ 0),
         log_pc_income = case_when(pc_income == 0 ~ 0,
                                   TRUE ~ log(pc_income)),
         log_pc_consump = case_when(pc_consump == 0 ~ 0,
                                    TRUE ~ log(pc_consump)),
         log_k_grp_income = case_when(kabu_ref_grp_income_pc == 0 ~ 0,
                                      TRUE ~ log(kabu_ref_grp_income_pc)),
         log_e_grp_income = case_when(eth_ref_grp_income_pc == 0 ~ 0,
                                        TRUE ~ log(eth_ref_grp_income_pc)),
         log_k_grp_consump = case_when(kabu_ref_grp_consump_pc == 0 ~ 0,
                                       TRUE ~ log(kabu_ref_grp_consump_pc)),
         log_e_grp_consump = case_when(eth_ref_grp_consump_pc == 0 ~ 0,
                                         TRUE ~ log(eth_ref_grp_consump_pc)),
         prov_kabu = paste0(province, kabu),
         province = as.character(province),
         ethnicity = as.character(ethnicity)) %>% 
  mutate(diff_log_k_grp_c = log_pc_consump - log_k_grp_consump,
         diff_log_e_grp_c = log_pc_consump - log_e_grp_consump,
         diff_log_k_grp_i = log_pc_income - log_k_grp_income,
         diff_log_e_grp_i = log_pc_income - log_e_grp_income) %>% 
  select(year, weight,hhid, pidlink, prov_kabu, ethnicity, female, age, age_grp, edu_grp, 
              pc_income, log_pc_income, log_k_grp_income, log_e_grp_income,
              pc_consump, log_pc_consump, log_k_grp_consump, log_e_grp_consump,
              diff_log_k_grp_c, diff_log_e_grp_c, diff_log_k_grp_i, diff_log_e_grp_i,
              sw_econ_ladder, sw_curr_std_liv, sw_food_consump, sw_happy) %>% 
  filter(edu_grp != 'Other')

saveRDS(df_out, './01 Data Processing/04 Output/Processed 2007 and 2014 IFLS data.rds')
write_csv(df_out, './01 Data Processing/04 Output/Processed 2007 and 2014 IFLS data.csv')
