## Normalize full, cleaned dataset

setwd("/Users/Thomas/Documents/Hawaiian_Phonetics/KLHData/R_scripts/")

source("2_calculating_exclusions.R")

## Get it into format that Joey's function can use

data$f1 <- as.numeric(data$f1)
data$f2 <- as.numeric(data$f2)
data <- data %>% ungroup()


## This uses Joe Fruehwald's Nearey 2 (log-mean) method to normalize without rescaling back to Hz

normed <- data %>%
  pivot_longer(cols = f1:f3,
               names_to = "formant",
               values_to = "hz") %>%
  group_by(Speaker) %>%
  mutate(nearey = log(hz)-mean(log(hz))) %>%
  select(-hz) %>%
  pivot_wider(names_from = formant,
              values_from = nearey)

normed <- normed %>%
  rename(f1_normed = f1,
         f2_normed = f2,
         f3_normed = f3)

orig_formants <- data %>%
  select(filename, f1:f3, time)

normed <- left_join(normed, orig_formants, by = c("filename","time")) %>%
  ungroup()

data <- normed

#### Print data to be in one big CSV #####

write.csv(data, file='all_data_31Jan2025.csv')


## This uses joey's norm_anae() function to create a normalized dataset
# 
# normed <- data %>%
#   group_by(Speaker) %>%
#   norm_anae(hz_cols = c(f1,f2), token_id = filename, speaker_id = Speaker)
# 
# normed <- normed %>% ungroup()
