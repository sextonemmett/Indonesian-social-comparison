rm(list = ls())
library(dplyr)
library(tidyr)
library(ggplot2)
library(haven)
library(readr)
library(readxl)

setwd('/Users/emmettsexton/Dropbox/Mac (2)/Desktop/BSE/Trimester 3/04 Social Econ/Term paper')

# Read in processed IFLS 2007 and 2014 data
df <- read_rds('./01 Data Processing/04 Output/Processed 2007 and 2014 IFLS data.rds')

columns_of_interest <- df %>% select(consump = pc_consump, income = pc_income, age, female)

summary_stats <- columns_of_interest %>%
  summarise(across(everything(), list(
    Q25 = ~quantile(., 0.25, na.rm = TRUE),
    Q50 = ~quantile(., 0.5, na.rm = TRUE),
    Mean = ~mean(., na.rm = TRUE),
    Q75 = ~quantile(., 0.75, na.rm = TRUE)
  )))

summary_stats_long <- summary_stats %>%
  pivot_longer(cols = everything(), names_to = c("variable", "stat"), names_sep = "_") %>%
  pivot_wider(names_from = stat, values_from = value)

write.csv(summary_stats_long, "./01 Data processing/04 Output/Other summary stats.csv")

# Education
edu_share <- df %>%
  group_by(edu_grp) %>%
  summarise(count = n()) %>%
  mutate(share = count / sum(count))

write.csv(edu_share, "./01 Data processing/04 Output/Education shares.csv")

# Ethnicity
ethnicity_share <- df %>%
  group_by(ethnicity) %>%
  summarise(count = n()) %>%
  mutate(share = count / sum(count))

# Create the ethnicity translation table as a named vector
ethnicity_translation <- c(
  "Jawa" = 1,
  "Sunda" = 2,
  "Bali" = 3,
  "Batak" = 4,
  "Bugis" = 5,
  "Tionghoa" = 6,
  "Madura" = 7,
  "Sasak" = 8,
  "Minang" = 9,
  "Banjar" = 10,
  "Bima-Dompu" = 11,
  "Makassar" = 12,
  "Nias" = 13,
  "Palembang" = 14,
  "Sumbawa" = 15,
  "Toraja" = 16,
  "Betawi" = 17,
  "Dayak" = 18,
  "Melayu" = 19,
  "Komering" = 20,
  "Ambon" = 21,
  "Manado" = 22,
  "Aceh" = 23,
  "Other South Sumatera" = 24,
  "Banten" = 25,
  "Cirebon" = 26,
  "Gorontalo" = 27,
  "Kutai" = 28,
  "Other" = 95
)

ethnicity_translation_df <- data.frame(
  ethnicity_name = names(ethnicity_translation),
  ethnicity_code = as.character(ethnicity_translation)
)
print(ethnicity_translation_df)

ethnicity_share_named <- ethnicity_share %>%
  left_join(ethnicity_translation_df, by = c("ethnicity" = "ethnicity_code"))

write.csv(ethnicity_share_named, "./01 Data processing/04 Output/Ethnicity shares.csv")
  