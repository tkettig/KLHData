### Phonetic correlates of stress in Hawaiian
library(tidyverse)
library(phonR)
library(lmerTest)
library(yarrr)
library(emmeans)
library(scales)
library(cowplot)
library(caret)

setwd("/Users/Thomas/Documents/Hawaiian_Phonetics/KLHData/R_scripts/")

## Gonna give myself two types of negated %in% here
`%!in%` = Negate(`%in%`)
`%notin%` <- Negate(`%in%`)

## This takes a few minutes to run all the data extraction
# source("3_normalize.R")

## This just gets the .csv with all the data, pre-extracted
data <- read.csv('all_data_10Dec2024.csv')

#### Data Preparation #####

## Make lists of vowels/mono/diph

list_of_monophthongs <- c("ā","ē","ī","ō","ū","a","e","i","o","u")
list_of_diphthongs <- c("ai","au","ao","ae","ei", "ou","eu","oi","iu","āi","āe","āu","āo","ēi","ōu")
list_of_vowels <- c("ā","ē","ī","ō","ū","a","e","i","o","u","ai","au","ao","ae","ei", "ou","eu","oi","iu","āi","āe","āu","āo","ēi","ōu")
list_of_long_mono <- c("ā","ē","ī","ō","ū")
list_of_short_mono <- c("a","e","i","o","u")

## Getting which point is the midpoint for each vowel, since some of the midpoints have now been excluded due to being outliers

midpoints <- data %>%
  group_by(filename) %>%
  mutate(midpoint = time) %>%
  slice(which.min(abs(time - 5.1))) %>%
  ungroup %>%
  dplyr::select(filename,midpoint)

data_midpoints <- left_join(data,midpoints, by="filename") %>% 
  filter(time==midpoint) %>%
  rename(f1_mid = f1_normed, f2_mid = f2_normed, f3_mid = f3_normed, mahal_dist_mid = mahal_dist, time_mid = time) %>%
  ungroup()

## Now get max/min F1 and F2 points

setwd("/Users/Thomas/Documents/Hawaiian_Phonetics/KLHData/R_scripts/")
source("max_min_points.R")

## This merges these readings into a single dataframe such that a/ā are measured at Max F1, etc.

max_f2 <- max_f2 %>% filter(vowel=="e"|vowel=="ē"|vowel=="i"|vowel=="ī")
min_f2 <- min_f2 %>% filter(vowel=="o"|vowel=="ō"|vowel=="u"|vowel=="ū"|vowel=="oa") # putting oa with /o/ rather than /a/ because when checking the midpoint plots, it looks like this is a more accurate representation of the proper measurement. still picks up on the lowering but does not force it.
max_f1 <- max_f1 %>% filter(!(vowel=="e"|vowel=="ē"|vowel=="i"|vowel=="ī"|vowel=="o"|vowel=="ō"|vowel=="u"|vowel=="ū"|vowel=="oa")) # making maxf1 for the rest of them; will eventually only want this for a and ā, but need placeholders for diphthongs

data4 <- rbind(max_f1,max_f2,min_f2) %>% 
  ungroup() %>%
  dplyr::select(filename, time:f3)


data4 <- data4 %>% rename(f1_inf = f1,
                          f2_inf = f2,
                          f3_inf = f3,
                          f1_normed_inf = f1_normed,
                          f2_normed_inf = f2_normed,
                          f3_normed_inf = f3_normed,
                          time_inf = time,
                          mahal_dist_inf = mahal_dist)


## Add back in the midpoint columns

data5 <- left_join(data_midpoints, data4, by="filename") %>%
  dplyr::select(-midpoint)


### Make durations into ms and log-transform
data5$duration <- 1000*(data5$duration)
data5$log_duration <- log(data5$duration)

## Rename so itʻs clear that f1/f2/f3 are of the inflection point

data5 <- data5 %>% rename(f1_normed_mid = f1_mid,
                          f2_normed_mid = f2_mid,
                          f3_normed_mid = f3_mid,
                          f0_reaper = meanf0,
                          f0_reaper_median = medianf0,
                          f0_fasttrack = f0)


### Make column for position/length

data5$length <- ifelse(data5$Syllabification == "Diph", "diph",
                       ifelse(data5$Moras == 2, "long", "short"))
data5$vowel_collapsed <- ifelse(data5$vowel == "ā", "a",
                                ifelse(data5$vowel == "ō", "o",
                                       ifelse(data5$vowel == "ū", "u",
                                              ifelse(data5$vowel == "ī", "i",
                                                     ifelse(data5$vowel == "ē", "e",
                                                            data5$vowel)))))

data5$position_length <- paste(data5$syllable_number, data5$length, sep=".")

### Explore f0 measurements a bit

NA_for_Praat <- data5 %>%
  filter(is.na(f0_praat))
NA_for_Reaper <- data5 %>%
  filter(is.na(f0_reaper))
NA_for_Reaper_median <- data5 %>%
  filter(is.na(f0_reaper_median))

hist(data5$f0_reaper, breaks = 300)
hist(data5$f0_praat, breaks = 300)
hist(data5$f0_reaper_median, breaks = 300)


hist(NA_for_Praat$f0_reaper, breaks = 300)
hist(NA_for_Praat$f0_reaper_median, breaks = 300)
hist(NA_for_Reaper$f0_praat, breaks = 300)
hist(NA_for_Reaper_median$f0_praat, breaks = 300)




### Getting a datset of words which only contain short vowels

only_shortV <- data5 %>%
  filter(Syllabification == "Mono") %>%        # Just monophthongs
  filter(stress != "0") %>%                    # 0s are either long words or issues with end of word interval != end of last letter
  filter(next_sound %notin% list_of_vowels, previous_sound %notin% list_of_vowels) %>%  # exclude next to another vowel
  #  filter(next_sound %notin% problematic_consonants, previous_sound %notin% problematic_consonants) %>% # exclude problematic consonants
  filter(next_sound != "sil", previous_sound !="sil") %>%  # nothing with silences next to it
  filter(next_word != "-") %>%               # no utterance-final words
  filter(word_syllables > 1) %>%            # Just words over 1 syll
  filter( !grepl(paste(list_of_diphthongs, collapse = "|"),word)) %>%     # Just words without any diphthongs
  filter( !grepl(paste(list_of_long_mono, collapse = "|"),word)) %>%         # Just words without any long vowels
  filter( aole == 0)                                          # No aole-type words

### Filtering so we get only the words where we have measurements for all syllables
short1 <- only_shortV %>%
  dplyr::select(word_unique, word_syllables) %>%
  group_by(word_unique, word_syllables) %>%
  summarise(count = length(word_unique)) %>%
  filter(count == word_syllables)
short2 <- only_shortV %>%
  filter(word_unique %in% short1$word_unique) %>%
  arrange(word_unique, interval)


## Adding syllable names and positions
short2$syllable_stress <- ifelse(short2$syllable_number == -1, "Final Unstressed",
                                 ifelse(short2$syllable_number == -2, "Primary Stressed",
                                        ifelse(short2$syllable_number == -3, "Antepen. Unstressed",
                                               "Secondary Stressed")))
short2$syllable_stress <- factor(short2$syllable_stress, levels=c("Secondary Stressed","Antepen. Unstressed","Primary Stressed", "Final Unstressed"))


## Get 2 syllable dataset

dat_2syl <- short2 %>% 
  filter(word_syllables == 2, str_length(vowel) == 1, Moras == 1)

dat_2syl <- dat_2syl %>% 
  dplyr::select(Speaker,
                word,
                word_unique,
                vowel,
                syllable_number,
                word_syllables,
                stress,
                syllable_stress,
                Moras,
                length,
                f0_reaper, 
                f0_praat,
                intensity, 
                duration, 
                log_duration, 
                f1_normed_inf, 
                f2_normed_inf
  ) %>% 
  # mutate(syllable_number = ifelse(syllable_number == -1,0,1),                                                                                                                         across(c(f0_reaper,f0_praat, intensity,log_duration,f1_normed_inf,f2_normed_inf),scale)) %>% 
  #  mutate(across(c(f0_reaper,f0_praat, intensity,log_duration,f1_normed_inf,f2_normed_inf),scale)) %>%
  rename(f1 = f1_normed_inf, f2 = f2_normed_inf)

dat_2syl$syllable_number <- as.factor(dat_2syl$syllable_number)



## Get 3 syllable dataset

dat_3syl <- short2 %>% filter(word_syllables == 3, str_length(vowel) == 1, Moras == 1)

dat_3syl %>% pull(syllable_number) %>% table

dat_3syl <- dat_3syl %>% 
  dplyr::select(Speaker,
                word,
                word_unique,
                vowel,
                syllable_number,
                word_syllables,
                stress,
                syllable_stress,
                Moras,
                length,
                f0_reaper, 
                f0_praat,
                intensity, 
                duration, 
                log_duration, 
                f1_normed_inf, 
                f2_normed_inf
  ) %>% 
  #  mutate(across(c(f0_reaper,f0_praat, intensity,log_duration,f1_normed_inf,f2_normed_inf),scale)) %>%
  rename(f1 = f1_normed_inf, f2 = f2_normed_inf)


dat_3syl$syllable_number <- as.factor(dat_3syl$syllable_number)


## Get 4 syllable dataset

dat_4syl <- short2 %>% 
  filter(word_syllables == 4, str_length(vowel) == 1, Moras == 1)

dat_4syl %>% pull(syllable_number) %>% table

dat_4syl <- dat_4syl %>% 
  dplyr::select(Speaker,
                word,
                word_unique,
                vowel,
                syllable_number,
                word_syllables,
                stress,
                syllable_stress,
                Moras,
                length,
                f0_reaper, 
                f0_praat,
                intensity, 
                duration, 
                log_duration, 
                f1_normed_inf, 
                f2_normed_inf
  ) %>% 
  #  mutate(across(c(f0_reaper,f0_praat, intensity,log_duration,f1_normed_inf,f2_normed_inf),scale)) %>%
  rename(f1 = f1_normed_inf, f2 = f2_normed_inf)


dat_4syl$syllable_number <- as.factor(dat_4syl$syllable_number)


### Now getting a dataset of words which only contain long vowels

only_longV <- data5 %>%
  filter(Syllabification == "Mono") %>%        # Just monophthongs
  filter(stress != "0") %>%                    # 0s are either long words or issues with end of word interval != end of last letter
  filter(next_sound %notin% list_of_vowels, previous_sound %notin% list_of_vowels) %>%  # exclude next to another vowel
  #  filter(next_sound %notin% problematic_consonants, previous_sound %notin% problematic_consonants) %>% # exclude problematic consonants
  filter(next_sound != "sil", previous_sound !="sil") %>%  # nothing with silences next to it
  filter(next_word != "-") %>%               # no utterance-final words
  filter(word_syllables > 1) %>%            # Just words over 1 syll
  filter( !grepl(paste(list_of_diphthongs, collapse = "|"),word)) %>%     # Just words without any diphthongs
  filter( !grepl(paste(list_of_short_mono, collapse = "|"),word))          # Just words without any short vowels

### Filtering so we only have words where we have info for all syllables
long1 <- only_longV %>%
  dplyr::select(word_unique, word_syllables) %>%
  group_by(word_unique, word_syllables) %>%
  summarise(count = length(word_unique)) %>%
  filter(count == word_syllables)
long2 <- only_longV %>%
  filter(word_unique %in% long1$word_unique) %>%
  arrange(word_unique, interval)


## Adding syllable names and positions
long2$syllable_stress <- ifelse(long2$syllable_number == -1, "Final Stressed",
                                ifelse(long2$syllable_number == -2, "Penultimate Secondary",
                                       ifelse(short2$syllable_number == -3, "Antepen. Secondary",
                                              "Secondary")))
long2$syllable_stress <- factor(long2$syllable_stress, levels=c("Final Stressed","Penultimate Secondary","Antepen. Secondary", "Secondary"))

## Get 2 syllable long dataset

dat_2syl_long <- long2 %>% 
  filter(word_syllables == 2, str_length(vowel) == 1, Moras == 2) %>% 
  dplyr::select(Speaker,
                word,
                word_unique,
                vowel,
                syllable_number,
                word_syllables,
                stress,
                syllable_stress,
                Moras,
                length,
                f0_reaper, 
                f0_praat,
                intensity,
                duration,
                log_duration,
                f1_normed_inf, 
                f2_normed_inf
  ) %>% 
  #  mutate(across(c(f0_reaper,f0_praat, intensity,log_duration,f1_normed_inf,f2_normed_inf),scale)) %>%
  rename(f1 = f1_normed_inf, 
         f2 = f2_normed_inf)


dat_2syl_long$syllable_number <- as.factor(dat_2syl_long$syllable_number)


## 2 short & 2 long together for comparison
## Also, vowel is recoded so long/short pairs are only distinguished by length (not by vowel label)

dat_2syll_shortlong <- rbind(dat_2syl, dat_2syl_long)

dat_2syll_shortlong$syll_length <- paste(dat_2syll_shortlong$syllable_number, dat_2syll_shortlong$length, sep = ".")

dat_2syll_shortlong <- dat_2syll_shortlong %>% 
  mutate(vowel = recode(vowel,
                        "ā" = "a",
                        "ō" = "o",
                        "ē" = "e",
                        "ī" = "i",
                        "ū" = "u"))


## 4 short + 2 long together

dat_4syll_shortlong <- rbind(dat_4syl, dat_2syl_long)

dat_4syll_shortlong$syll_length <- paste(dat_4syll_shortlong$syllable_number, dat_4syll_shortlong$length, sep = ".")

## 4 short + 2 long together for comparison of primary and secondary (leaving out all unstressed)


dat_primarysecondary <- rbind(dat_4syl, dat_2syl_long)

dat_primarysecondary$syll_length <- paste(dat_primarysecondary$syllable_number, dat_primarysecondary$length, sep = ".")

dat_primarysecondary <- dat_primarysecondary %>%
  filter(stress == "primary" | stress ==  "secondary")

dat_primarysecondary <- dat_primarysecondary %>% 
  mutate(vowel = recode(vowel,
                        "ā" = "a",
                        "ō" = "o",
                        "ē" = "e",
                        "ī" = "i",
                        "ū" = "u"))

## 4 short + 3 short together for comparison

dat_34syll <- rbind(dat_4syl, dat_3syl)

dat_34syll$syll_comp <- paste(dat_34syll$syllable_number, dat_34syll$word_syllables, sep = "_")



## Get z-scored version of models

scale_columns <- function(df, cols) {
  df %>% 
    group_by(across(Speaker)) %>%
    mutate(across(all_of(cols), ~ scale(.) %>% as.vector))
}

# Example list of dataframes
df_list <- list(dat_2syl, dat_2syl_long, dat_2syll_shortlong,
                dat_34syll, dat_3syl,
                dat_4syl, dat_4syll_shortlong, dat_primarysecondary)  # Replace with your actual dataframes

# Columns to scale
columns_to_scale <- c("f0_praat", "intensity")  # Replace with your actual column names

# Apply the scaling function to each dataframe
scaled_df_list <- lapply(df_list, scale_columns, cols = columns_to_scale)

df_names <- c("dat_2syl", "dat_2syl_long", "dat_2syll_shortlong",
              "dat_34syll", "dat_3syl",
              "dat_4syl", "dat_4syll_shortlong", "dat_primarysecondary")

for (i in seq_along(df_names)) {
  new_name <- paste0(df_names[i], "_z")
  assign(new_name, scaled_df_list[[i]])
}

#### Bayesian logistic mixed effects regression models #####

# 2 syll short
priors <- c(set_prior("normal(0,10", class = "b"))
model_2syll_int <- brm(intensity ~ syllable_number*vowel + (1 + syllable_number|Speaker) + (1|word_unique),
                   data = dat_2syl,
                   family = gaussian(),
                   chains = 4,
                   cores = 4,
                   iter = 6000,
                   warmup = 1000,
                   prior = priors
)
saveRDS(model_2syll_int, "model_2syll_int.rds")

priors <- c(set_prior("normal(0,30", class = "b"))
model_2syll_f0 <- brm(f0_praat ~ syllable_number*vowel + (1 + syllable_number|Speaker) + (1|word_unique),
                       data = dat_2syl,
                       family = gaussian(),
                       chains = 4,
                       cores = 4,
                       iter = 6000,
                       warmup = 1000,
                       prior = priors
)
saveRDS(model_2syll_f0, "model_2syll_f0.rds")

priors <- c(set_prior("normal(0,100", class = "b"))
model_2syll_dur <- brm(duration ~ syllable_number*vowel + (1 + syllable_number|Speaker) + (1|word_unique),
                      data = dat_2syl,
                      family = lognormal(),
                      chains = 4,
                      cores = 4,
                      iter = 6000,
                      warmup = 1000,
                      prior = priors
)
saveRDS(model_2syll_dur, "model_2syll_dur.rds")

# 3 syll short
priors <- c(set_prior("normal(0,10", class = "b"))
model_3syll_int <- brm(intensity ~ syllable_number*vowel + (1 + syllable_number|Speaker) + (1|word_unique),
                       data = dat_3syl,
                       family = gaussian(),
                       chains = 4,
                       cores = 4,
                       iter = 6000,
                       warmup = 1000,
                       prior = priors
)
saveRDS(model_3syll_int, "model_3syll_int.rds")

priors <- c(set_prior("normal(0,30", class = "b"))
model_3syll_f0 <- brm(f0_praat ~ syllable_number*vowel + (1 + syllable_number|Speaker) + (1|word_unique),
                      data = dat_3syl,
                      family = gaussian(),
                      chains = 4,
                      cores = 4,
                      iter = 6000,
                      warmup = 1000,
                      prior = priors
)
saveRDS(model_3syll_f0, "model_3syll_f0.rds")

priors <- c(set_prior("normal(0,100", class = "b"))
model_3syll_dur <- brm(duration ~ syllable_number*vowel + (1 + syllable_number|Speaker) + (1|word_unique),
                       data = dat_3syl,
                       family = lognormal(),
                       chains = 4,
                       cores = 4,
                       iter = 6000,
                       warmup = 1000,
                       prior = priors
)
saveRDS(model_3syll_dur, "model_3syll_dur.rds")

# 4 syll short
priors <- c(set_prior("normal(0,10", class = "b"))
model_4syll_int <- brm(intensity ~ syllable_number*vowel + (1 + syllable_number|Speaker) + (1|word_unique),
                       data = dat_4syl,
                       family = gaussian(),
                       chains = 4,
                       cores = 4,
                       iter = 6000,
                       warmup = 1000,
                       prior = priors
)
saveRDS(model_4syll_int, "model_4syll_int.rds")

priors <- c(set_prior("normal(0,30", class = "b"))
model_4syll_f0 <- brm(f0_praat ~ syllable_number*vowel + (1 + syllable_number|Speaker) + (1|word_unique),
                      data = dat_4syl,
                      family = gaussian(),
                      chains = 4,
                      cores = 4,
                      iter = 6000,
                      warmup = 1000,
                      prior = priors
)
saveRDS(model_4syll_f0, "model_4syll_f0.rds")

priors <- c(set_prior("normal(0,100", class = "b"))
model_4syll_dur <- brm(duration ~ syllable_number*vowel + (1 + syllable_number|Speaker) + (1|word_unique),
                       data = dat_4syl,
                       family = lognormal(),
                       chains = 4,
                       cores = 4,
                       iter = 6000,
                       warmup = 1000,
                       prior = priors
)
saveRDS(model_4syll_dur, "model_4syll_dur.rds")


#  2 syll long
priors <- c(set_prior("normal(0,10", class = "b"))
model_2syll_long_int <- brm(intensity ~ syllable_number*vowel + (1 + syllable_number|Speaker) + (1|word_unique),
                       data = dat_2syl_long,
                       family = gaussian(),
                       chains = 4,
                       cores = 4,
                       iter = 6000,
                       warmup = 1000,
                       prior = priors
)
saveRDS(model_2syll_long_int, "model_2syll_long_int.rds")

priors <- c(set_prior("normal(0,30", class = "b"))
model_2syll_long_f0 <- brm(f0_praat ~ syllable_number*vowel + (1 + syllable_number|Speaker) + (1|word_unique),
                      data = dat_2syl_long,
                      family = gaussian(),
                      chains = 4,
                      cores = 4,
                      iter = 6000,
                      warmup = 1000,
                      prior = priors
)
saveRDS(model_2syll_long_f0, "model_2syll_long_f0.rds")

priors <- c(set_prior("normal(0,100", class = "b"))
model_2syll_long_dur <- brm(duration ~ syllable_number*vowel + (1 + syllable_number|Speaker) + (1|word_unique),
                       data = dat_2syl_long,
                       family = lognormal(),
                       chains = 4,
                       cores = 4,
                       iter = 6000,
                       warmup = 1000,
                       prior = priors
)
saveRDS(model_2syll_long_dur, "model_2syll_long_dur.rds")


# 2 syll short and 2 syll long
priors <- c(set_prior("normal(0,10", class = "b"))
model_2syll_shortlong_int <- brm(intensity ~ syllable_number*length*vowel + (1 + syllable_number|Speaker) + (1|word_unique),
                            data = dat_2syll_shortlong,
                            family = gaussian(),
                            chains = 4,
                            cores = 4,
                            iter = 6000,
                            warmup = 1000,
                            prior = priors
)
saveRDS(model_2syll_shortlong_int, "model_2syll_shortlong_int.rds")

priors <- c(set_prior("normal(0,30", class = "b"))
model_2syll_shortlong_f0 <- brm(f0_praat ~ syllable_number*length*vowel + (1 + syllable_number|Speaker) + (1|word_unique),
                           data = dat_2syll_shortlong,
                           family = gaussian(),
                           chains = 4,
                           cores = 4,
                           iter = 6000,
                           warmup = 1000,
                           prior = priors
)
saveRDS(model_2syll_shortlong_f0, "model_2syll_shortlong_f0.rds")

priors <- c(set_prior("normal(0,100", class = "b"))
model_2syll_shortlong_dur <- brm(duration ~ syllable_number*length*vowel + (1 + syllable_number|Speaker) + (1|word_unique),
                            data = dat_2syll_shortlong,
                            family = lognormal(),
                            chains = 4,
                            cores = 4,
                            iter = 6000,
                            warmup = 1000,
                            prior = priors
)
saveRDS(model_2syll_shortlong_dur, "model_2syll_shortlong_dur.rds")


## 4-syll short vs. 2-syll long primary vs secondary
priors <- c(set_prior("normal(0,10", class = "b"))
model_primarysecondary_int <- brm(intensity ~ stress*length*vowel + (1 + stress|Speaker) + (1|word_unique),
                                 data = dat_primarysecondary,
                                 family = gaussian(),
                                 chains = 4,
                                 cores = 4,
                                 iter = 6000,
                                 warmup = 1000,
                                 prior = priors
)
saveRDS(model_primarysecondary_int, "model_primarysecondary_int.rds")

priors <- c(set_prior("normal(0,30", class = "b"))
model_primarysecondary_f0 <- brm(f0_praat ~ stress*length*vowel + (1 + stress|Speaker) + (1|word_unique),
                                data = dat_primarysecondary,
                                family = gaussian(),
                                chains = 4,
                                cores = 4,
                                iter = 6000,
                                warmup = 1000,
                                prior = priors
)
saveRDS(model_primarysecondary_f0, "model_primarysecondary_f0.rds")

priors <- c(set_prior("normal(0,100", class = "b"))
model_primarysecondary_dur <- brm(duration ~ stress*length*vowel + (1 + stress|Speaker) + (1|word_unique),
                                 data = dat_primarysecondary,
                                 family = lognormal(),
                                 chains = 4,
                                 cores = 4,
                                 iter = 6000,
                                 warmup = 1000,
                                 prior = priors
)
saveRDS(model_primarysecondary_dur, "model_primarysecondary_dur.rds")
# 

# 3 syll short vs 4 syll short
priors <- c(set_prior("normal(0,10", class = "b"))
model_34syll_int <- brm(intensity ~ syll_comp*vowel + (1 + syll_comp|Speaker) + (1|word_unique),
                       data = dat_34syll,
                       family = gaussian(),
                       chains = 4,
                       cores = 4,
                       iter = 6000,
                       warmup = 1000,
                       prior = priors
)
saveRDS(model_34syll_int, "model_34syll_int.rds")

priors <- c(set_prior("normal(0,30", class = "b"))
model_34syll_f0 <- brm(f0_praat ~ syll_comp*vowel + (1 + syll_comp|Speaker) + (1|word_unique),
                      data = dat_34syll,
                      family = gaussian(),
                      chains = 4,
                      cores = 4,
                      iter = 6000,
                      warmup = 1000,
                      prior = priors
)
saveRDS(model_34syll_f0, "model_34syll_f0.rds")

priors <- c(set_prior("normal(0,100", class = "b"))
model_34syll_dur <- brm(duration ~ syll_comp*vowel + (1 + syll_comp|Speaker) + (1|word_unique),
                       data = dat_34syll,
                       family = lognormal(),
                       chains = 4,
                       cores = 4,
                       iter = 6000,
                       warmup = 1000,
                       prior = priors
)
saveRDS(model_34syll_dur, "model_34syll_dur.rds")


