### 2 Sept 2024
### Phonetic correlates of stress in Hawaiian
### Phonetic correlates of stress in Hawaiian
library(tidyverse)
library(phonR)
library(lmerTest)
library(yarrr)
library(emmeans)
library(scales)
library(cowplot)
library(caret)
library(ggdensity)

setwd("/Users/Thomas/Documents/Hawaiian_Phonetics/KLHData/R_scripts/")

## Gonna give myself two types of negated %in% here
`%!in%` = Negate(`%in%`)
`%notin%` <- Negate(`%in%`)

## This takes a few minutes to run all the data extraction
# source("3_normalize.R")

## This just gets the .csv with all the data, pre-extracted
data <- read.csv('all_data_31Jan2025.csv')

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


#### Explore how many of the 2-syllable short words actually have longer final vowel? #####

dat_2syl$Difference_duration <- dat_2syl$duration - lead(dat_2syl$duration)
dat_2syl$Difference_intensity <- dat_2syl$intensity - lead(dat_2syl$intensity)
dat_2syl$Difference_f0 <- dat_2syl$f0_praat - lead(dat_2syl$f0_praat)

dat_2syl$Final_longer <-ifelse(dat_2syl$duration < lead(dat_2syl$duration), TRUE, FALSE) 
dat_2syl$Final_intenser <-ifelse(dat_2syl$intensity < lead(dat_2syl$intensity), TRUE, FALSE) 
dat_2syl$Final_higher <-ifelse(dat_2syl$f0_praat < lead(dat_2syl$f0_praat), TRUE, FALSE) 

dat_2syl_Finals <- dat_2syl %>%
  filter(syllable_number == "-2")

summary(dat_2syl_Finals$Final_longer)
summary(dat_2syl_Finals$Final_intenser)
summary(dat_2syl_Finals$Final_higher)

ggplot(dat_2syl_Finals, aes(x = Difference_duration)) +
  geom_density() +  # Scatter plot of points
  geom_vline(xintercept = 0, color = "blue", linetype = "dashed") +
  theme_minimal() 
#  facet_wrap(~ Speaker) 

ggplot(dat_2syl_Finals, aes(x = Difference_intensity)) +
  geom_density() +  # Scatter plot of points
  geom_vline(xintercept = 0, color = "blue", linetype = "dashed") +
  theme_minimal() 

ggplot(dat_2syl_Finals, aes(x = Difference_f0)) +
  geom_density() +  # Scatter plot of points
  geom_vline(xintercept = 0, color = "blue", linetype = "dashed") +
  theme_minimal() 


#### Plots from before Feb 2025 #######

## Making a ggplot theme
apatheme=theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        text=element_text(family='Arial'),
        legend.title=element_blank(),
        legend.position=c(.7,.8),
        axis.line.x = element_line(color='black'),
        axis.line.y = element_line(color='black'))


### Setting up datasets for intensity measures for words with short vowels
### Intensity in dB is a very stable measure. There won't be any NA values or 
### weird outliers, so this should be the full dataset

short_2syll_int <- filter(short2, word_syllables == 2)
short_3syll_int <- filter(short2, word_syllables == 3) 
short_4syll_int <- filter(short2, word_syllables == 4)


short_2syll_int_means = short_2syll_int %>% 
  group_by(syllable_stress, length, syllable_number, word_syllables) %>% 
  summarise(mean = mean(intensity, na.rm=T), 
            sd = sd(intensity, na.rm=T), 
            count = n())
short_2syll_int_means$se <- short_2syll_int_means$sd / sqrt(short_2syll_int_means$count)
short_2syll_int_means$tval <- qt(0.05/2, df = short_2syll_int_means$count - 1)
short_2syll_int_means$moe <- short_2syll_int_means$tval * short_2syll_int_means$se * -1
short_2syll_int_means$upper <- short_2syll_int_means$mean + short_2syll_int_means$moe
short_2syll_int_means$lower <- short_2syll_int_means$mean - short_2syll_int_means$moe

short_3syll_int_means = short_3syll_int %>% 
  group_by(syllable_stress, length, syllable_number, word_syllables) %>% 
  summarise(mean = mean(intensity, na.rm=T), 
            sd = sd(intensity, na.rm=T), 
            count = n())
short_3syll_int_means$se <- short_3syll_int_means$sd / sqrt(short_3syll_int_means$count)
short_3syll_int_means$tval <- qt(0.05/2, df = short_3syll_int_means$count - 1)
short_3syll_int_means$moe <- short_3syll_int_means$tval * short_3syll_int_means$se * -1
short_3syll_int_means$upper <- short_3syll_int_means$mean + short_3syll_int_means$moe
short_3syll_int_means$lower <- short_3syll_int_means$mean - short_3syll_int_means$moe

short_4syll_int_means = short_4syll_int %>% 
  group_by(syllable_stress, length, syllable_number, word_syllables) %>% 
  summarise(mean = mean(intensity, na.rm=T), 
            sd = sd(intensity, na.rm=T), 
            count = n())
short_4syll_int_means$se <- short_4syll_int_means$sd / sqrt(short_4syll_int_means$count)
short_4syll_int_means$tval <- qt(0.05/2, df = short_4syll_int_means$count - 1)
short_4syll_int_means$moe <- short_4syll_int_means$tval * short_4syll_int_means$se * -1
short_4syll_int_means$upper <- short_4syll_int_means$mean + short_4syll_int_means$moe
short_4syll_int_means$lower <- short_4syll_int_means$mean - short_4syll_int_means$moe



#### Setting up duration measures, short

ggplot(short2,aes(x=duration))+geom_histogram(bins=30)

short3 <- short2 %>%
  filter( ! (duration > .25))

# Now get only the words where we have full info for all syllables
short1 <- short3 %>%
  dplyr::select(word_unique, word_syllables) %>%
  group_by(word_unique, word_syllables) %>%
  summarise(count = length(word_unique)) %>%
  filter(count == word_syllables)
short3 <- short3 %>%
  filter(word_unique %in% short1$word_unique) %>%
  arrange(word_unique, interval)

# get into ms
short3$duration <- short3$duration * 1000

short_2syll_dur <- filter(short3, word_syllables == 2)
short_3syll_dur <- filter(short3, word_syllables == 3)
short_4syll_dur <- filter(short3, word_syllables == 4)


short_2syll_dur_means = short_2syll_dur %>% 
  group_by(syllable_stress, length, syllable_number, word_syllables) %>% 
  summarise(mean = mean(duration), 
            sd = sd(duration), 
            count = n())
short_2syll_dur_means$se <- short_2syll_dur_means$sd / sqrt(short_2syll_dur_means$count)
short_2syll_dur_means$tval <- qt(0.05/2, df = short_2syll_dur_means$count - 1)
short_2syll_dur_means$moe <- short_2syll_dur_means$tval * short_2syll_dur_means$se * -1
short_2syll_dur_means$upper <- short_2syll_dur_means$mean + short_2syll_dur_means$moe
short_2syll_dur_means$lower <- short_2syll_dur_means$mean - short_2syll_dur_means$moe

short_3syll_dur_means = short_3syll_dur %>% 
  group_by(syllable_stress, length, syllable_number, word_syllables) %>% 
  summarise(mean = mean(duration), 
            sd = sd(duration), 
            count = n())
short_3syll_dur_means$se <- short_3syll_dur_means$sd / sqrt(short_3syll_dur_means$count)
short_3syll_dur_means$tval <- qt(0.05/2, df = short_3syll_dur_means$count - 1)
short_3syll_dur_means$moe <- short_3syll_dur_means$tval * short_3syll_dur_means$se * -1
short_3syll_dur_means$upper <- short_3syll_dur_means$mean + short_3syll_dur_means$moe
short_3syll_dur_means$lower <- short_3syll_dur_means$mean - short_3syll_dur_means$moe

short_4syll_dur_means = short_4syll_dur %>% 
  group_by(syllable_stress, length, syllable_number, word_syllables) %>% 
  summarise(mean = mean(duration), 
            sd = sd(duration), 
            count = n())
short_4syll_dur_means$se <- short_4syll_dur_means$sd / sqrt(short_4syll_dur_means$count)
short_4syll_dur_means$tval <- qt(0.05/2, df = short_4syll_dur_means$count - 1)
short_4syll_dur_means$moe <- short_4syll_dur_means$tval * short_4syll_dur_means$se * -1
short_4syll_dur_means$upper <- short_4syll_dur_means$mean + short_4syll_dur_means$moe
short_4syll_dur_means$lower <- short_4syll_dur_means$mean - short_4syll_dur_means$moe



#### Setting up F0 from Praat measures, short

hist(short2$f0_praat)

ggplot(short2,aes(x=f0_praat))+geom_histogram(bins=30)+facet_grid(.~Speaker)

## Exclude outliers past 3sd from the mean internal to each person

cutoffs <- short2 %>%
  group_by(Speaker) %>%
  summarise(upperThreshold = mean(f0_praat, na.rm=T) + 3*sd(f0_praat, na.rm=T),
            lowerThreshold = mean(f0_praat, na.rm=T) - 3*sd(f0_praat, na.rm=T))

short4 <- left_join(short2, cutoffs, by = "Speaker")

short4 <- short4 %>%
  filter(f0_praat > lowerThreshold) %>%
  filter(f0_praat < upperThreshold)
#  filter(Speaker != "IN") # IN f0_praat tracking was bad
#  filter( ! (Speaker == "DK" & f0_praat < 90)) # Get some bad ones from DK out, if we want? should do for others too though



# Now get only the words where we have full info for all syllables
short1 <- short4 %>%
  dplyr::select(word_unique, word_syllables) %>%
  group_by(word_unique, word_syllables) %>%
  summarise(count = length(word_unique)) %>%
  filter(count == word_syllables)
short4 <- short4 %>%
  filter(word_unique %in% short1$word_unique) %>%
  arrange(word_unique, interval)
ggplot(short4,aes(x=f0_praat))+geom_histogram(bins=30)+facet_grid(.~Speaker)


short_2syll_f0_praat <- filter(short4, word_syllables == 2)
short_3syll_f0_praat <- filter(short4, word_syllables == 3)
short_4syll_f0_praat <- filter(short4, word_syllables == 4)



short_2syll_f0_praat_means = short_2syll_f0_praat %>% 
  group_by(syllable_stress, length, syllable_number, word_syllables) %>% 
  summarise(mean = mean(f0_praat), 
            sd = sd(f0_praat), 
            count = n())
short_2syll_f0_praat_means$se <- short_2syll_f0_praat_means$sd / sqrt(short_2syll_f0_praat_means$count)
short_2syll_f0_praat_means$tval <- qt(0.05/2, df = short_2syll_f0_praat_means$count - 1)
short_2syll_f0_praat_means$moe <- short_2syll_f0_praat_means$tval * short_2syll_f0_praat_means$se * -1
short_2syll_f0_praat_means$upper <- short_2syll_f0_praat_means$mean + short_2syll_f0_praat_means$moe
short_2syll_f0_praat_means$lower <- short_2syll_f0_praat_means$mean - short_2syll_f0_praat_means$moe

short_3syll_f0_praat_means = short_3syll_f0_praat %>% 
  group_by(syllable_stress, length, syllable_number, word_syllables) %>% 
  summarise(mean = mean(f0_praat), 
            sd = sd(f0_praat), 
            count = n())
short_3syll_f0_praat_means$se <- short_3syll_f0_praat_means$sd / sqrt(short_3syll_f0_praat_means$count)
short_3syll_f0_praat_means$tval <- qt(0.05/2, df = short_3syll_f0_praat_means$count - 1)
short_3syll_f0_praat_means$moe <- short_3syll_f0_praat_means$tval * short_3syll_f0_praat_means$se * -1
short_3syll_f0_praat_means$upper <- short_3syll_f0_praat_means$mean + short_3syll_f0_praat_means$moe
short_3syll_f0_praat_means$lower <- short_3syll_f0_praat_means$mean - short_3syll_f0_praat_means$moe

short_4syll_f0_praat_means = short_4syll_f0_praat %>% 
  group_by(syllable_stress, position_length, length, syllable_number, word_syllables) %>% 
  summarise(mean = mean(f0_praat), 
            sd = sd(f0_praat), 
            count = n())
short_4syll_f0_praat_means$se <- short_4syll_f0_praat_means$sd / sqrt(short_4syll_f0_praat_means$count)
short_4syll_f0_praat_means$tval <- qt(0.05/2, df = short_4syll_f0_praat_means$count - 1)
short_4syll_f0_praat_means$moe <- short_4syll_f0_praat_means$tval * short_4syll_f0_praat_means$se * -1
short_4syll_f0_praat_means$upper <- short_4syll_f0_praat_means$mean + short_4syll_f0_praat_means$moe
short_4syll_f0_praat_means$lower <- short_4syll_f0_praat_means$mean - short_4syll_f0_praat_means$moe


## Renaming syllables
long2$syllable_stress <- ifelse(long2$syllable_number == -1, "Final Primary Stressed",
                                "Secondary Stressed")
long2$syllable_stress <- factor(long2$syllable_stress, levels=c("Final Primary Stressed","Secondary Stressed"))


## Filter out "kuma" words because it's unclear if it should be treated separately
## Also filter so we're only considering 2-syllable words
long2 <- long2 %>%
  filter(word != "kūmā" & word != "kūmāmā") %>%
  filter(word_syllables == 2)

## Make duration into milliseconds instead of seconds
long2$duration <- long2$duration *1000

## Get a dataset with the means of intensity for each syllable position
long_2syll_int_means = long2 %>% 
  group_by(syllable_stress, position_length, length, syllable_number) %>% 
  summarise(mean = mean(intensity, na.rm=T), 
            sd = sd(intensity, na.rm=T), 
            count = sum(!is.na(intensity)))
long_2syll_int_means$se <- long_2syll_int_means$sd / sqrt(long_2syll_int_means$count)
long_2syll_int_means$tval <- qt(0.05/2, df = long_2syll_int_means$count - 1)
long_2syll_int_means$moe <- long_2syll_int_means$tval * long_2syll_int_means$se * -1
long_2syll_int_means$upper <- long_2syll_int_means$mean + long_2syll_int_means$moe
long_2syll_int_means$lower <- long_2syll_int_means$mean - long_2syll_int_means$moe

## Get a dataset with the means of f0 for each syllable position
long_2syll_f0_means = long2 %>% 
  group_by(syllable_stress, position_length, length, syllable_number) %>% 
  summarise(mean = mean(f0_reaper, na.rm=T), 
            sd = sd(f0_reaper, na.rm=T), 
            count = sum(!is.na(f0_reaper)))
long_2syll_f0_means$se <- long_2syll_f0_means$sd / sqrt(long_2syll_f0_means$count)
long_2syll_f0_means$tval <- qt(0.05/2, df = long_2syll_f0_means$count - 1)
long_2syll_f0_means$moe <- long_2syll_f0_means$tval * long_2syll_f0_means$se * -1
long_2syll_f0_means$upper <- long_2syll_f0_means$mean + long_2syll_f0_means$moe
long_2syll_f0_means$lower <- long_2syll_f0_means$mean - long_2syll_f0_means$moe

## Get a dataset with the means of duration for each syllable position
long_2syll_dur_means = long2 %>% 
  group_by(syllable_stress, position_length, length, syllable_number) %>% 
  summarise(mean = mean(duration, na.rm=T), 
            sd = sd(duration, na.rm=T), 
            count = n())
long_2syll_dur_means$se <- long_2syll_dur_means$sd / sqrt(long_2syll_dur_means$count)
long_2syll_dur_means$tval <- qt(0.05/2, df = long_2syll_dur_means$count - 1)
long_2syll_dur_means$moe <- long_2syll_dur_means$tval * long_2syll_dur_means$se * -1
long_2syll_dur_means$upper <- long_2syll_dur_means$mean + long_2syll_dur_means$moe
long_2syll_dur_means$lower <- long_2syll_dur_means$mean - long_2syll_dur_means$moe





### Intensity stats and plots


## 2-syllable words with just short vowels, intensity
# Stats
short_2syll_int$syllable_stress <- as.factor(short_2syll_int$syllable_stress)
p <- lmer(intensity ~ syllable_stress + (1|Speaker) + (1|word_unique)  + (1|vowel),
          data = short_2syll_int)
summary(p)
emmeans(p, specs = pairwise ~ syllable_stress)

# Plot
my_colors <- c("#009E73")
ggplot(data = short_2syll_int_means, aes(x = syllable_stress, y = mean, fill = length))+
  apatheme+
  labs(x = "Syllable position", y = "Intensity, dB") +
  geom_violin(data = short_2syll_int, aes(x = syllable_stress, y = intensity))+
  geom_point(data = short_2syll_int_means, aes(y = mean, x=syllable_stress), size = 3) +
  geom_errorbar(ymax= short_2syll_int_means$upper, ymin=short_2syll_int_means$lower, width = 0.5) +
  scale_x_discrete(labels = label_wrap(5)) +
  scale_fill_manual(values=my_colors) +
  theme(legend.position = "none")

## 3-syllable words with just short vowels, intensity
# Stats
short_3syll_int$syllable_stress <- as.factor(short_3syll_int$syllable_stress)
p <- lmer(intensity ~ syllable_stress + (1|Speaker) + (1|word_unique)  + (1|vowel),
          data = short_3syll_int)
summary(p)
emmeans(p, specs = pairwise ~ syllable_stress)
# Plot
my_colors <- c("#009E73")
ggplot(data = short_3syll_int_means, aes(x = syllable_stress, y = mean, fill = length))+
  apatheme+
  labs(x = "Syllable position", y = "Intensity, dB") +
  geom_violin(data = short_3syll_int, aes(x = syllable_stress, y = intensity))+
  geom_point(data = short_3syll_int_means, aes(y = mean, x=syllable_stress), size = 3) +
  geom_errorbar(ymax= short_3syll_int_means$upper, ymin=short_3syll_int_means$lower, width = 0.5) +
  scale_x_discrete(labels = label_wrap(5))+
  scale_fill_manual(values=my_colors) +
  theme(legend.position = "none")


## 4-syllable words with just short vowels, intensity
# Stats
short_4syll_int$syllable_stress <- as.factor(short_4syll_int$syllable_stress)
p <- lmer(intensity ~ syllable_stress + (1|Speaker) + (1|word_unique)  + (1|vowel),
          data = short_4syll_int)
summary(p)
emmeans(p, specs = pairwise ~ syllable_stress)
# Plot
my_colors <- c("#009E73")
ggplot(data = short_4syll_int_means, aes(x = syllable_stress, y = mean, fill = length))+
  apatheme+
  labs(x = "Syllable position", y = "Intensity, dB") +
  geom_violin(data = short_4syll_int, aes(x = syllable_stress, y = intensity))+
  geom_point(data = short_4syll_int_means, aes(y = mean, x=syllable_stress), size = 3) +
  geom_errorbar(ymax= short_4syll_int_means$upper, ymin=short_4syll_int_means$lower, width = 0.8) +
  scale_x_discrete(labels = label_wrap(5)) +
  scale_fill_manual(values=my_colors) +
  theme(legend.position = "none")



## 2-syllable words with just long vowels, intensity

long2$syllable_number <- as.factor(long2$syllable_number)
p <- lmer(intensity ~ syllable_number + (1|Speaker) + (1|word_unique)  + (1|vowel),
          data = long2)
summary(p)
emmeans(p, specs = pairwise ~ syllable_number)

# Plot
my_colors <- c("#56B4E9")
ggplot(data = long_2syll_int_means, aes(x = syllable_stress, y = mean, fill = length))+
  apatheme+
  labs(x = "Syllable position", y = "Intensity, dB") +
  geom_violin(data = long2, aes(x = syllable_stress, y = intensity))+
  geom_point(data = long_2syll_int_means, aes(y = mean, x=syllable_stress), size = 3) +
  geom_errorbar(ymax= long_2syll_int_means$upper, ymin=long_2syll_int_means$lower, width = 0.5) +
  scale_x_discrete(labels = label_wrap(5)) +
  scale_fill_manual(values=my_colors) +
  theme(legend.position = "none") +
  scale_x_discrete(limits = rev) 


## 2-syllable short words vs. 2-syllable long words

short_2syll_int_comp <- short_2syll_int %>% 
  dplyr::select(-rel_f0_reaper, -vowel.stress)

long_short_int <- rbind(short_2syll_int_comp, long2)

short_2syll_int_means$length <- "short"
long_2syll_int_means$length <- "long"

long_short_int_means <- rbind(short_2syll_int_means, long_2syll_int_means)

long_short_int_means$position_length = factor(long_short_int_means$position_length, levels= c("-2.long", "-2.short","-1.long","-1.short"))
long_short_int$position_length = factor(long_short_int$position_length, levels= c("-2.long", "-2.short","-1.long","-1.short"))


long_short_int_means$syllable_stress = factor(long_short_int_means$syllable_stress, levels= c("Secondary Stressed", "Primary Stressed","Final Primary Stressed","Final Unstressed"))
long_short_int$syllable_stress = factor(long_short_int$syllable_stress, levels= c("Secondary Stressed", "Primary Stressed","Final Primary Stressed","Final Unstressed"))

# Plot
my_colors <- c("#56B4E9", "#009E73")
ggplot(data = long_short_int_means, aes(x = position_length, y = mean, fill = length))+
  apatheme+
  labs(x = "Syllable position", y = "Intensity, dB") +
  geom_violin(data = long_short_int, aes(x = position_length, y = intensity, fill = length))+
  geom_point(data = long_short_int_means, aes(y = mean, x=position_length, fill = length), size = 3) +
  geom_errorbar(ymax= long_short_int_means$upper, ymin=long_short_int_means$lower, width = 0.5) +
  scale_x_discrete(labels = label_wrap(5)) +
  scale_fill_manual(values=my_colors) +
  theme(legend.position = "right")

# Stats
p <- lmerTest::lmer(intensity ~ length * syllable_number + (1|Speaker) + (1|word_unique)  + (1|vowel),
                    data = long_short_int)
summary(p)



## 4-syll short vs. 2-syll long

short_4syll_int_comp <- short_4syll_int %>% 
  dplyr::select(-rel_f0_reaper, -vowel.stress)

long2_short4_int <- rbind(short_4syll_int_comp, long2)

short_4syll_int_means$length <- "short"
long_2syll_int_means$length <- "long"

long2_short4_means_int <- rbind(short_4syll_int_means, long_2syll_int_means)

long2_short4_means_int$position_length = factor(long2_short4_means_int$position_length, levels= c("-4.short","-3.short", "-2.long", "-2.short","-1.long","-1.short"))
long2_short4_int$position_length = factor(long2_short4_int$position_length, levels= c("-4.short","-3.short","-2.long", "-2.short","-1.long","-1.short"))

# Plot
my_colors <- c("#56B4E9", "#009E73")
ggplot(data = long2_short4_means_int, aes(x = position_length, y = mean, fill = length))+
  apatheme+
  labs(x = "Syllable position", y = "Intensity, dB") +
  geom_violin(data = long2_short4_int, aes(x = position_length, y = intensity, fill = length))+
  geom_point(data = long2_short4_means_int, aes(y = mean, x=position_length, fill = length), size = 3) +
  geom_errorbar(ymax= long2_short4_means_int$upper, ymin=long2_short4_means_int$lower, width = 0.5) +
  scale_x_discrete(labels = label_wrap(5))+
  scale_fill_manual(values=my_colors) +
  theme(legend.position = "right")

# Stats
p <- lmer(intensity ~ position_length + (1|Speaker) + (1|word_unique)  + (1|vowel),
          data = long2_short4_int)
summary(p)
emmeans(p, specs = pairwise ~ position_length)



## 3-syll short vs. 4-syll short

short_4syll_int_comp <- short_4syll_int %>% 
  dplyr::select(-rel_f0_reaper, -vowel.stress)

short_3syll_int_comp <- short_3syll_int %>% 
  dplyr::select(-rel_f0_reaper, -vowel.stress)

short3_short4_int <- rbind(short_4syll_int_comp, short_3syll_int_comp)
short3_short4_int$syll_comp <- paste(short3_short4_int$syllable_number, short3_short4_int$word_syllables, sep = "_")
short3_short4_int$word_syllables <- as.factor(short3_short4_int$word_syllables)

short3_short4_means_int <- rbind(short_4syll_int_means, short_3syll_int_means)
short3_short4_means_int$syll_comp <- paste(short3_short4_means_int$syllable_number, short3_short4_means_int$word_syllables, sep = "_")
short3_short4_means_int$word_syllables <- as.factor(short3_short4_means_int$word_syllables)

# Plot
my_colors <- c("#a1d99b", "#009E73")
ggplot(data = short3_short4_means_int, aes(x = syll_comp, y = mean, fill = word_syllables))+
  apatheme+
  labs(x = "Syllable position", y = "Intensity, dB") +
  geom_violin(data = short3_short4_int, aes(x = syll_comp, y = intensity, fill = word_syllables))+
  geom_point(data = short3_short4_means_int, aes(y = mean, x=syll_comp, fill = word_syllables), size = 3) +
  geom_errorbar(ymax= short3_short4_means_int$upper, ymin=short3_short4_means_int$lower, width = 0.5) +
  scale_x_discrete(limits = rev) +
  scale_fill_manual(values=my_colors) +
  theme(legend.position = "right")

# Stats
p <- lmer(intensity ~ syll_comp + (1|Speaker) + (1|word_unique)  + (1|vowel),
          data = short3_short4_int)
summary(p)
emmeans(p, specs = pairwise ~ syll_comp)




#### F0 stats and plots


## 2-syllable words with just short vowels, f0_reaper
# Stats
short_2syll_f0_reaper$syllable_stress <- as.factor(short_2syll_f0_reaper$syllable_stress)
p <- lmer(f0_reaper ~ syllable_stress + (1|Speaker) + (1|word_unique) + (1|vowel),
          data = short_2syll_f0_reaper)
summary(p)
emmeans(p, specs = pairwise ~ syllable_stress)
# Plot
my_colors <- c( "#009E73")
ggplot(data = short_2syll_f0_reaper_means, aes(x = syllable_stress, y = mean, fill = length))+
  apatheme+
  labs(x = "Syllable position", y = "f0, Hz") +
  geom_violin(data = short_2syll_f0_reaper, aes(x = syllable_stress, y = f0_reaper))+
  geom_point(data = short_2syll_f0_reaper_means, aes(y = mean, x=syllable_stress), size = 3) +
  geom_errorbar(ymax= short_2syll_f0_reaper_means$upper, ymin=short_2syll_f0_reaper_means$lower, width = 0.5) +
  scale_x_discrete(labels = label_wrap(5))+
  scale_fill_manual(values=my_colors) +
  theme(legend.position = "none")

## 3-syllable words with just short vowels, f0_reaper
# Stats
short_3syll_f0_reaper$syllable_stress <- as.factor(short_3syll_f0_reaper$syllable_stress)
p <- lmer(f0_reaper ~ syllable_stress + (1|Speaker) + (1|word_unique) + (1|vowel),
          data = short_3syll_f0_reaper)
summary(p)
emmeans(p, specs = pairwise ~ syllable_stress)
# Plot
my_colors <- c( "#009E73")
ggplot(data = short_3syll_f0_reaper_means, aes(x = syllable_stress, y = mean, fill = length))+
  apatheme+
  labs(x = "Syllable position", y = "f0, Hz") +
  geom_violin(data = short_3syll_f0_reaper, aes(x = syllable_stress, y = f0_reaper))+
  geom_point(data = short_3syll_f0_reaper_means, aes(y = mean, x=syllable_stress), size = 3) +
  geom_errorbar(ymax= short_3syll_f0_reaper_means$upper, ymin=short_3syll_f0_reaper_means$lower, width = 0.5) +
  scale_x_discrete(labels = label_wrap(5)) +
  scale_fill_manual(values=my_colors) +
  theme(legend.position = "none")


## 4-syllable words with just short vowels, f0_reaper
# Stats
short_4syll_f0_reaper$syllable_stress <- as.factor(short_4syll_f0_reaper$syllable_stress)
p <- lmer(f0_reaper ~ syllable_stress + (1|Speaker) + (1|word_unique) + (1|vowel),
          data = short_4syll_f0_reaper)
summary(p)
emmeans(p, specs = pairwise ~ syllable_stress)
# Plot
my_colors <- c( "#009E73")
ggplot(data = short_4syll_f0_reaper_means, aes(x = syllable_stress, y = mean, fill = length))+
  apatheme+
  labs(x = "Syllable position", y = "f0, Hz") +
  geom_violin(data = short_4syll_f0_reaper, aes(x = syllable_stress, y = f0_reaper)) +
  geom_point(data = short_4syll_f0_reaper_means, aes(y = mean, x=syllable_stress), size = 3) +
  geom_errorbar(ymax= short_4syll_f0_reaper_means$upper, ymin=short_4syll_f0_reaper_means$lower, width = 0.8) +
  scale_x_discrete(labels = label_wrap(5)) +
  scale_fill_manual(values=my_colors) +
  theme(legend.position = "none")




## 2-syllable words with just long vowels, f0 (Reaper)
long2$syllable_stress <- as.factor(long2$syllable_stress)
p <- lmer(f0_reaper ~ syllable_stress + (1|Speaker) + (1|word_unique)  + (1|vowel),
          data = long2)
summary(p)
emmeans(p, specs = pairwise ~ syllable_stress)

# Plot
my_colors <- c("#56B4E9")
ggplot(data = long_2syll_f0_means, aes(x = syllable_stress, y = mean, fill = length))+
  apatheme+
  labs(x = "Syllable position", y = "f0, Hz") +
  geom_violin(data = long2, aes(x = syllable_stress, y = f0_reaper))+
  geom_point(data = long_2syll_f0_means, aes(y = mean, x=syllable_stress), size = 3) +
  geom_errorbar(ymax= long_2syll_f0_means$upper, ymin=long_2syll_f0_means$lower, width = 0.5) +
  scale_x_discrete(labels = label_wrap(5)) +
  scale_fill_manual(values=my_colors) +
  theme(legend.position = "none") +
  scale_x_discrete(limits = rev)



## 2-syll short vs. 2-syll long
short_2syll_f0_comp <- short_2syll_f0_reaper %>% 
  dplyr::select(-rel_f0_reaper, -vowel.stress, -upperThreshold, -lowerThreshold)

long_short_f0 <- rbind(short_2syll_f0_comp, long2)

short_2syll_f0_reaper_means$length <- "short"
long_2syll_f0_means$length <- "long"

long_short_f0_means <- rbind(short_2syll_f0_reaper_means, long_2syll_f0_means)

long_short_f0_means$position_length = factor(long_short_f0_means$position_length, levels= c("-2.long", "-2.short","-1.long","-1.short"))
long_short_f0$position_length = factor(long_short_f0$position_length, levels= c("-2.long", "-2.short","-1.long","-1.short"))

# Plot
my_colors <- c("#56B4E9", "#009E73")
ggplot(data = long_short_f0_means, aes(x = position_length, y = mean, fill = length))+
  apatheme+
  labs(x = "Syllable position", y = "f0, Hz") +
  geom_violin(data = long_short_f0, aes(x = position_length, y = f0_reaper, fill = length))+
  geom_point(data = long_short_f0_means, aes(y = mean, x=position_length, fill = length), size = 3) +
  geom_errorbar(ymax= long_short_f0_means$upper, ymin=long_short_f0_means$lower, width = 0.5) +
  scale_x_discrete(labels = label_wrap(5)) +
  scale_fill_manual(values=my_colors) +
  theme(legend.position = "right")

# Stats
p <- lmerTest::lmer(f0_reaper ~ length * syllable_number + (1|Speaker) + (1|word_unique)  + (1|vowel),
                    data = long_short_f0)
summary(p)
emmeans(p, specs = pairwise ~ length)
emmeans(p, specs = pairwise ~ syllable_number)
emmeans(p, specs = pairwise ~ length * syllable_number)



## 4-syll short vs. 2-syll long
short_4syll_f0_comp <- short_4syll_f0_reaper %>% 
  dplyr::select(-rel_f0_reaper, -vowel.stress, -upperThreshold, -lowerThreshold)

long2_short4_f0 <- rbind(short_4syll_f0_comp, long2)

short_4syll_f0_reaper_means$length <- "short"
long_2syll_f0_means$length <- "long"

long2_short4_means_f0 <- rbind(short_4syll_f0_reaper_means, long_2syll_f0_means)

long2_short4_means_f0$position_length = factor(long2_short4_means_f0$position_length, levels= c("-4.short","-3.short", "-2.long", "-2.short","-1.long","-1.short"))
long2_short4_f0$position_length = factor(long2_short4_f0$position_length, levels= c("-4.short","-3.short","-2.long", "-2.short","-1.long","-1.short"))

# Plot
my_colors <- c("#56B4E9", "#009E73")
ggplot(data = long2_short4_means_f0, aes(x = position_length, y = mean, fill = length))+
  apatheme+
  labs(x = "Syllable position", y = "F0, Hz") +
  geom_violin(data = long2_short4_f0, aes(x = position_length, y = f0_reaper, fill = length))+
  geom_point(data = long2_short4_means_f0, aes(y = mean, x=position_length, fill = length), size = 3) +
  geom_errorbar(ymax= long2_short4_means_f0$upper, ymin=long2_short4_means_f0$lower, width = 0.5) +
  scale_x_discrete(labels = label_wrap(5))+
  scale_fill_manual(values=my_colors) +
  theme(legend.position = "right")

# Stats

p <- lmer(f0_reaper ~ position_length + (1|Speaker) + (1|word_unique)  + (1|vowel),
          data = long2_short4_f0)
summary(p)
emmeans(p, specs = pairwise ~ position_length)



## 3-syll short vs. 4-syll short

short_4syll_f0_comp <- short_4syll_f0_reaper %>% 
  dplyr::select(-rel_f0_reaper, -vowel.stress)

short_3syll_f0_comp <- short_3syll_f0_reaper %>% 
  dplyr::select(-rel_f0_reaper, -vowel.stress)

short3_short4_f0 <- rbind(short_4syll_f0_comp, short_3syll_f0_comp)
short3_short4_f0$syll_comp <- paste(short3_short4_f0$syllable_number, short3_short4_f0$word_syllables, sep = "_")
short3_short4_f0$word_syllables <- as.factor(short3_short4_f0$word_syllables)

short3_short4_means_f0 <- rbind(short_4syll_f0_reaper_means, short_3syll_f0_reaper_means)
short3_short4_means_f0$syll_comp <- paste(short3_short4_means_f0$syllable_number, short3_short4_means_f0$word_syllables, sep = "_")
short3_short4_means_f0$word_syllables <- as.factor(short3_short4_means_f0$word_syllables)

# Plot
my_colors <- c("#a1d99b", "#009E73")
ggplot(data = short3_short4_means_f0, aes(x = syll_comp, y = mean, fill = word_syllables))+
  apatheme+
  labs(x = "Syllable position", y = "F0, Hz") +
  geom_violin(data = short3_short4_f0, aes(x = syll_comp, y = f0_reaper, fill = word_syllables))+
  geom_point(data = short3_short4_means_f0, aes(y = mean, x=syll_comp, fill = word_syllables), size = 3) +
  geom_errorbar(ymax= short3_short4_means_f0$upper, ymin=short3_short4_means_f0$lower, width = 0.5) +
  scale_x_discrete(limits = rev)+
  scale_fill_manual(values=my_colors) +
  theme(legend.position = "right")


# Stats

p <- lmer(f0_reaper ~ syll_comp + (1|Speaker) + (1|word_unique)  + (1|vowel),
          data = short3_short4_f0)
summary(p)
emmeans(p, specs = pairwise ~ syll_comp)





#### Duration stats and plots

## 2-syllable words with just short vowels, duration
# Stats
short_2syll_dur$syllable_stress <- as.factor(short_2syll_dur$syllable_stress)
p <- lmer(duration ~ syllable_stress + (1|Speaker) + (1|word_unique) + (1|vowel),
          data = short_2syll_dur)
summary(p)
emmeans(p, specs = pairwise ~ syllable_stress)
# Plot
my_colors <- c("#009E73")
ggplot(data = short_2syll_dur_means, aes(x = syllable_stress, y = mean, fill = length))+
  apatheme+
  labs(x = "Syllable position", y = "Duration, ms") +
  geom_violin(data = short_2syll_dur, aes(x = syllable_stress, y = duration))+
  geom_point(data = short_2syll_dur_means, aes(y = mean, x=syllable_stress), size = 3) +
  geom_errorbar(ymax= short_2syll_dur_means$upper, ymin=short_2syll_dur_means$lower, width = 0.5) +
  scale_x_discrete(labels = label_wrap(5)) +
  scale_fill_manual(values=my_colors) +
  theme(legend.position = "none")


## 3-syllable words with just short vowels, duration
# Stats
short_3syll_dur$syllable_stress <- as.factor(short_3syll_dur$syllable_stress)
p <- lmer(duration ~ syllable_stress + (1|Speaker) + (1|word_unique) + (1|vowel),
          data = short_3syll_dur)
summary(p)
emmeans(p, specs = pairwise ~ syllable_stress)
# Plot
my_colors <- c("#009E73")
ggplot(data = short_3syll_dur_means, aes(x = syllable_stress, y = mean, fill = length))+
  apatheme+
  labs(x = "Syllable position", y = "Duration, ms") +
  geom_violin(data = short_3syll_dur, aes(x = syllable_stress, y = duration))+
  geom_point(data = short_3syll_dur_means, aes(y = mean, x=syllable_stress), size = 3) +
  geom_errorbar(ymax= short_3syll_dur_means$upper, ymin=short_3syll_dur_means$lower, width = 0.5) +
  scale_x_discrete(labels = label_wrap(5)) +
  scale_fill_manual(values=my_colors) +
  theme(legend.position = "none")


## 4-syllable words with just short vowels, duration
# Stats
short_4syll_dur$syllable_stress <- as.factor(short_4syll_dur$syllable_stress)
p <- lmer(duration ~ syllable_stress + (1|Speaker) + (1|word_unique) + (1|vowel),
          data = short_4syll_dur)
summary(p)
emmeans(p, specs = pairwise ~ syllable_stress)
# Plot
my_colors <- c("#009E73")
ggplot(data = short_4syll_dur_means, aes(x = syllable_stress, y = mean, fill = length))+
  apatheme+
  labs(x = "Syllable position", y = "Duration, ms") +
  geom_violin(data = short_4syll_dur, aes(x = syllable_stress, y = duration))+
  geom_point(data = short_4syll_dur_means, aes(y = mean, x=syllable_stress), size = 3) +
  geom_errorbar(ymax= short_4syll_dur_means$upper, ymin=short_4syll_dur_means$lower, width = 0.9) +
  scale_x_discrete(labels = label_wrap(5)) +
  scale_fill_manual(values=my_colors) +
  theme(legend.position = "none")



## 2-syllable words with just long vowels, duration

long2$syllable_stress <- as.factor(long2$syllable_stress)
p <- lmer(duration ~ syllable_stress + (1|Speaker) + (1|word_unique)  + (1|vowel),
          data = long2)
summary(p)
emmeans(p, specs = pairwise ~ syllable_stress)

# Plot
my_colors <- c("#56B4E9")
ggplot(data = long_2syll_dur_means, aes(x = syllable_stress, y = mean, fill = length))+
  apatheme+
  labs(x = "Syllable position", y = "Duration, ms") +
  geom_violin(data = long2, aes(x = syllable_stress, y = duration))+
  geom_point(data = long_2syll_dur_means, aes(y = mean, x=syllable_stress), size = 3) +
  geom_errorbar(ymax= long_2syll_dur_means$upper, ymin=long_2syll_dur_means$lower, width = 0.5) +
  scale_x_discrete(labels = label_wrap(5)) +
  scale_fill_manual(values=my_colors) +
  theme(legend.position = "none") +
  scale_x_discrete(limits = rev) 



## 2-syll Short vs. 2-syll long 

short_2syll_dur_comp <- short_2syll_dur %>% 
  dplyr::select(-rel_f0_reaper, -vowel.stress)

long_short_dur <- rbind(short_2syll_dur_comp, long2)

short_2syll_dur_means$length <- "short"
long_2syll_dur_means$length <- "long"

long_short_dur_means <- rbind(short_2syll_dur_means, long_2syll_dur_means)


# Plot

long_short_dur_means$position_length = factor(long_short_dur_means$position_length, levels= c("-2.long", "-2.short","-1.long","-1.short"))
long_short_dur$position_length = factor(long_short_dur$position_length, levels= c("-2.long", "-2.short","-1.long","-1.short"))
my_colors <- c("#56B4E9", "#009E73")
ggplot(data = long_short_dur_means, aes(x = position_length, y = mean, fill = length))+
  apatheme +
  labs(x = "Syllable position", y = "Duration, ms") +
  geom_violin(data = long_short_dur, aes(x = position_length, y = duration, fill = length))+
  geom_point(data = long_short_dur_means, aes(y = mean, x=position_length, fill = length), size = 3) +
  geom_errorbar(ymax= long_short_dur_means$upper, ymin=long_short_dur_means$lower, width = 0.5) +
  scale_x_discrete(labels = label_wrap(5)) +
  scale_fill_manual(values=my_colors) +
  theme(legend.position = "right")

# Stats

p <- lmerTest::lmer(duration ~ length * syllable_number + (1|Speaker) + (1|word_unique)  + (1|vowel),
                    data = long_short_dur)
summary(p)
emmeans(p, specs = pairwise ~ length)
emmeans(p, specs = pairwise ~ syllable_number)
emmeans(p, specs = pairwise ~ length * syllable_number)




#### 4-syll Short vs. 2-syll long

short_4syll_dur_comp <- short_4syll_dur %>% 
  dplyr::select(-rel_f0_reaper, -vowel.stress)

long2_short4_dur <- rbind(short_4syll_dur_comp, long2)

short_4syll_dur_means$length <- "short"
long_2syll_dur_means$length <- "long"

long2_short4_means_dur <- rbind(short_4syll_dur_means, long_2syll_dur_means)

long2_short4_means_dur$position_length = factor(long2_short4_means_dur$position_length, levels= c("-4.short","-3.short", "-2.long", "-2.short","-1.long","-1.short"))
long2_short4_dur$position_length = factor(long2_short4_dur$position_length, levels= c("-4.short","-3.short","-2.long", "-2.short","-1.long","-1.short"))

# Plot
my_colors <- c("#56B4E9", "#009E73")
ggplot(data = long2_short4_means_dur, aes(x = position_length, y = mean, fill = length))+
  apatheme+
  labs(x = "Syllable position", y = "Duration, ms") +
  geom_violin(data = long2_short4_dur, aes(x = position_length, y = duration, fill = length))+
  geom_point(data = long2_short4_means_dur, aes(y = mean, x=position_length, fill = length), size = 3) +
  geom_errorbar(ymax= long2_short4_means_dur$upper, ymin=long2_short4_means_dur$lower, width = 0.5) +
  scale_x_discrete(labels = label_wrap(5))+
  scale_fill_manual(values=my_colors) +
  theme(legend.position = "right")

# Stats

p <- lmer(duration ~ position_length + (1|Speaker) + (1|word_unique)  + (1|vowel),
          data = long2_short4_dur)
summary(p)
emmeans(p, specs = pairwise ~ position_length)



## 3-syll short vs 4-syll short

short_4syll_dur_comp <- short_4syll_dur %>% 
  dplyr::select(-rel_f0_reaper, -vowel.stress)

short_3syll_dur_comp <- short_3syll_dur %>% 
  dplyr::select(-rel_f0_reaper, -vowel.stress)

short3_short4_dur <- rbind(short_4syll_dur_comp, short_3syll_dur_comp)
short3_short4_dur$syll_comp <- paste(short3_short4_dur$syllable_number, short3_short4_dur$word_syllables, sep = "_")
short3_short4_dur$word_syllables <- as.factor(short3_short4_dur$word_syllables)

short3_short4_means_dur <- rbind(short_4syll_dur_means, short_3syll_dur_means)
short3_short4_means_dur$syll_comp <- paste(short3_short4_means_dur$syllable_number, short3_short4_means_dur$word_syllables, sep = "_")
short3_short4_means_dur$word_syllables <- as.factor(short3_short4_means_dur$word_syllables)

# Plot
my_colors <- c("#a1d99b", "#009E73")
ggplot(data = short3_short4_means_dur, aes(x = syll_comp, y = mean, fill = word_syllables))+
  apatheme+
  labs(x = "Syllable position", y = "Duration, ms") +
  geom_violin(data = short3_short4_dur, aes(x = syll_comp, y = duration, fill = word_syllables))+
  geom_point(data = short3_short4_means_dur, aes(y = mean, x=syll_comp, fill = word_syllables), size = 3) +
  geom_errorbar(ymax= short3_short4_means_dur$upper, ymin=short3_short4_means_dur$lower, width = 0.5) +
  scale_x_discrete(limits = rev)+
  scale_fill_manual(values=my_colors) +
  theme(legend.position = "right")

# Stats

p <- lmer(duration ~ syll_comp + (1|Speaker) + (1|word_unique)  + (1|vowel),
          data = short3_short4_dur)
summary(p)
emmeans(p, specs = pairwise ~ syll_comp)




#### F1/F2 ####

short_means <- short2 %>%
  group_by(vowel) %>%
  summarise(mean_f1_inf = mean(f1_normed_inf),
            mean_f2_inf = mean(f2_normed_inf),
            mean_f1_mid = mean(f1_normed_mid),
            mean_f2_mid = mean(f2_normed_mid))
short2$vowel.stress <- paste(short2$syllable_stress, short2$vowel, sep = " ")


short2syll_means <- dat_2syl %>%
  group_by(vowel, stress) %>%
  summarise(mean_f1 = mean(f1),
            mean_f2 = mean(f2))
dat_2syl$vowel.stress <- paste(dat_2syl$stress, dat_2syl$vowel, sep = " ")




my_colors <- c("#999999", "#E69F00", "#000000", "#009E73") # 4
my_linetype <- c("longdash", "dotted", "solid", "dashed")

ggplot() +
  geom_hdr_lines(data = short2, aes(x = f2_normed_inf, y = f1_normed_inf, group = vowel.stress, color = syllable_stress), probs = 0.5, alpha = 1) +
  scale_x_reverse() + scale_y_reverse() +
  theme_classic() + 
  scale_colour_manual(values=my_colors) +
  labs(title = "All speakers, normalized: Short monophthongs by stress", x = "F2, normalized (Hz)", y = "F1, normalized (Hz)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_wrap(vars(vowel))


## Vowel by vowel density
my_colors <- c("#999999", "#E69F00") # 2
my_linetype <- c("longdash", "dotted", "solid", "dashed")
ggplot() +
  geom_hdr_lines(data = dat_2syl, aes(x = f2, y = f1, group = vowel.stress, color = stress), probs = 0.5, alpha = 1) +
  scale_x_reverse() + scale_y_reverse() +
  theme_classic() + 
  scale_colour_manual(values=my_colors) +
  labs(title = "All speakers, normalized: 2-syllable short monophthongs by stress", x = "F2, normalized (Hz)", y = "F1, normalized (Hz)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_wrap(vars(vowel))

## Just the means, 2 syll
my_colors <- c("#999999", "#E69F00") # 2
ggplot() +
  scale_x_reverse() + scale_y_reverse() +
  theme_classic() + 
  scale_color_manual(values=my_colors) +
  geom_text(data = short2syll_means, aes(x = mean_f2, y = mean_f1, color = stress, label = vowel, fontface=2), cex=5) +
  labs(title = "Monophthongs, primary and unstressed means", x = "F2", y = "F1") +
  theme(plot.title = element_text(hjust = 0.5))


## Combined
my_colors <- c("#999999", "#E69F00") # 2
ggplot() +
  scale_x_reverse() + scale_y_reverse() +
  theme_classic() + 
  scale_color_manual(values=my_colors) +
  geom_hdr_lines(data = dat_2syl, aes(x = f2, y = f1, group = vowel.stress, color = stress), probs = 0.5, alpha = 1) +
  geom_text(data = short2syll_means, aes(x = mean_f2, y = mean_f1, color = stress, label = vowel, fontface=2), cex=5) +
  labs(title = "Monophthongs, primary and unstressed means and densities", x = "F2", y = "F1") +
  theme(plot.title = element_text(hjust = 0.5))

my_colors <- c("#999999", "#E69F00", "#000000", "#009E73", "skyblue","#000000", "#009E73") # 5
my_linetype <- c("solid", "dashed")
short2syll_means$typeface <- ifelse(short2syll_means$stress == "primary", "bold", "italic")
ggplot() +
  scale_x_reverse() + scale_y_reverse() +
  theme_classic() + 
  scale_color_manual(values=my_colors) +
  geom_hdr_lines(data = dat_2syl, aes(x = f2, y = f1, group = vowel.stress, color = vowel, linetype = stress), probs = 0.5, alpha = 1) +
  geom_text(data = short2syll_means, aes(x = mean_f2, y = mean_f1, label = vowel, fontface= typeface, color = vowel), cex=5) +
  labs(title = "Two-syllable words with short monophthongs, primary and unstressed means and 50% densities", x = "F2 (normalized)", y = "F1 (normalized)") +
  theme(plot.title = element_text(hjust = 0.5))


## Get individual vowel dataframes
short2_a <- short2 %>%
  filter(vowel == "a")
short2_e <- short2 %>%
  filter(vowel == "e")
short2_i <- short2 %>%
  filter(vowel == "i")
short2_o <- short2 %>%
  filter(vowel == "o")
short2_u <- short2 %>%
  filter(vowel == "u")

a <- ggplot() +
  geom_hdr_lines(data = short2_a, aes(x = f2_normed_inf, y = f1_normed_inf, group = vowel.stress, color = syllable_stress), probs = 0.5, alpha = 1) +
  scale_x_reverse() + scale_y_reverse() +
  theme_classic() + 
  scale_colour_manual(values=my_colors) +
  xlim(0.8,-0.4) + ylim(0, -1.2) +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none") 

b <- ggplot() +
  geom_hdr_lines(data = short2_a, aes(x = f2_normed_inf, y = f1_normed_inf, group = vowel.stress, color = syllable_stress), probs = 0.67, alpha = 1) +
  scale_x_reverse() + scale_y_reverse() +
  theme_classic() + 
  scale_colour_manual(values=my_colors) +
  xlim(0.8,-0.4) + ylim(0, -1.2) +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none") 

c <- ggplot() +
  geom_hdr_lines(data = short2_a, aes(x = f2_normed_inf, y = f1_normed_inf, group = vowel.stress, color = syllable_stress), probs = 0.8, alpha = 1) +
  scale_x_reverse() + scale_y_reverse() +
  theme_classic() + 
  scale_colour_manual(values=my_colors) +
  xlim(0.8,-0.4) + ylim(0, -1.2) +
  theme(plot.title = element_text(hjust = 0.5)) 

plot_grid(a, b, c)







#### Primary stressed syllables from 2, 3, and 4 short ####
#### Instead of caring about whether we have all data for all syllables
#### for a particular correlate, we can just look at all primary stressed
#### syllables present.

# short2 has all data
primary_int <- short2 %>%
  filter(word_syllables == 2 | word_syllables == 3 | word_syllables == 4) %>%
  filter(stress == "primary")

primary_int$word_syllables <- as.factor(primary_int$word_syllables)


yarrr::pirateplot(data = primary_int,
                  formula = intensity ~ word_syllables)

p <- lmer(intensity ~ word_syllables + (1|Speaker) + (1|vowel),
          data = primary_int)
summary(p)
emmeans(p, specs = pairwise ~ word_syllables)



# Not working to manually make the plots, errorbar is breaking
#
primary_int_means = primary_int %>%
  group_by(word_syllables) %>%
  summarise(mean = mean(intensity, na.rm=T),
            sd = sd(intensity, na.rm=T),
            count = n())
primary_int_means$se <- primary_int_means$sd / sqrt(primary_int_means$count)
primary_int_means$tval <- qt(0.05/2, df = primary_int_means$count - 1)
primary_int_means$moe <- primary_int_means$tval * primary_int_means$se * -1
primary_int_means$upper <- primary_int_means$mean + primary_int_means$moe
primary_int_means$lower <- primary_int_means$mean - primary_int_means$moe

ggplot(data = primary_int, aes(x = word_syllables, y = intensity))+
  apatheme+
  labs(x = "Primary stressed syllable by number of syllables in word", y = "Intensity, dB") +
  geom_violin(data = primary_int, aes(x = word_syllables, y = intensity)) +
  geom_point(data = primary_int_means, aes(y = mean, x=word_syllables), size = 3) +
  geom_errorbar(ymax= primary_int_means$upper, ymin=primary_int_means$lower, width = 0.5) +
  scale_x_discrete(labels = label_wrap(5))



# short 3 excluded ones with duration > .25s
primary_dur <- short3 %>%
  filter(word_syllables == 2 | word_syllables == 3 | word_syllables == 4) %>%
  filter(stress == "primary") 

primary_dur$duration 

primary_dur$word_syllables <- as.factor(primary_dur$word_syllables)

yarrr::pirateplot(data = primary_dur,
                  formula = duration ~ word_syllables)

p <- lmer(duration ~ word_syllables + (1|Speaker) + (1|vowel),
          data = primary_dur)
summary(p)
emmeans(p, specs = pairwise ~ word_syllables)

## Not working to manually make the plots, errorbar is breaking
##
# primary_dur_means = primary_dur %>% 
#   group_by(word_syllables) %>% 
#   summarise(mean = mean(duration, na.rm=T), 
#             sd = sd(duration, na.rm=T), 
#             count = n())
# primary_dur_means$se <- primary_dur_means$sd / sqrt(primary_dur_means$count)
# primary_dur_means$tval <- qt(0.05/2, df = primary_dur_means$count - 1)
# primary_dur_means$moe <- primary_dur_means$tval * primary_dur_means$se * -1
# primary_dur_means$upper <- primary_dur_means$mean + primary_dur_means$moe
# primary_dur_means$lower <- primary_dur_means$mean - primary_dur_means$moe
# 
# ggplot(data = primary_dur, aes(x = word_syllables, y = duration))+
#   apatheme+
#   labs(x = "Primary stressed syllable by number of syllables in word", y = "Intensity, dB") +
#   geom_violin(data = primary_dur, aes(x = word_syllables, y = duration)) +
#   geom_point(data = primary_dur_means, aes(y = mean, x=word_syllables), size = 3) +
#   geom_errorbar(ymax= primary_dur_means$upper, ymin=primary_dur_means$lower, width = 0.5) +
#   scale_x_discrete(labels = label_wrap(5))


# short4 filtered out outlier f0_reapers

primary_f0 <- short4 %>% 
  filter(word_syllables == 2 | word_syllables == 3 | word_syllables == 4) %>%
  filter(stress == "primary")


primary_f0$word_syllables <- as.factor(primary_f0$word_syllables)

yarrr::pirateplot(data = primary_f0,
                  formula = f0_reaper ~ word_syllables)

p <- lmer(f0_reaper ~ word_syllables + (1|Speaker) + (1|vowel),
          data = primary_f0)
summary(p)
emmeans(p, specs = pairwise ~ word_syllables)











#### Conditional inference tree ####

library(partykit)

ctree_data <- dat_2syl %>%
  dplyr::select(syllable_number, f0_praat, intensity, duration) %>%
  drop_na()

ctree_data <- dat_2syl_z %>%
  dplyr::select(syllable_number, f0_praat, intensity, duration) %>%
  drop_na()

ctree_model <- ctree(syllable_number ~ 
                       f0_praat + 
                       intensity + 
                       duration,
                     data = ctree_data,
                     control=ctree_control(minbucket=500))
plot(ctree_model)



#### Linear discriminant analysis

library(MASS)
library(readxl)
library(devtools)
library(ggord)
library(klaR)

lda_data <- dat_2syl_z %>%
  dplyr::select(syllable_number, f0_praat, intensity, duration) %>%
  drop_na()


# Data Partition
set.seed(60)
training.samples <- lda_data$syllable_number %>%
  createDataPartition(p = 0.8, list = FALSE)
training.prep <- lda_data[training.samples, ]
testing.prep <- lda_data[-training.samples, ]
preproc.param <- training.prep %>% 
  preProcess(method = c("center", "scale"))
preproc.param.test <- testing.prep %>% 
  preProcess(method = c("center", "scale"))
training <- preproc.param %>% predict(training.prep)
testing <- preproc.param.test %>% predict(testing.prep)

# Fit the model
model <- lda(syllable_number ~ 
               duration + 
               f0_praat + 
               intensity, 
             data = lda_data)
model


# Make predictions
predictions <- predict(model, training)
predictions

# Confusion matrix and accuracy
p1 <- predict(model, training)$class
tab <- table(Predicted = p1, Actual = training$syllable_number)
tab
sum(diag(tab))/sum(tab)

p2 <- predict(model, testing)$class
tab <- table(Predicted = p2, Actual = testing$syllable_number)
tab
sum(diag(tab))/sum(tab)


# Histograms
ldahist(data = predictions$x[,1],g=training$syllable_number)
ldahist(data = predictions$x[,2],g=training$syllable_number)

###PLOTTING DATA

#Basic plot
plot(model)

#Scatterplot
ldadata <- cbind(training, predict(model)$x)
ggplot(ldadata, aes(LD1, LD2)) +
  #geom_label(label=ldadata$Transcription)
  geom_point(aes(color = Tone))

# Bi-plot
ggord(model,training$Tone,ellipse_pro = .8, txt=3,arrow=.2)

#Partition Plots 
partimat(as.factor(syllable_number) ~ 
           duration + 
           f0_praat + 
           intensity, 
         data = training, method="lda")






#### Arjun Models #####


### Tables of Ns and co-occurrences
dat_2syl %>% pull(syllable_number) %>% table

dat_2syl %>% .$vowel %>% table

dat_2syl %>% group_by(vowel, syllable_number) %>%
  summarise(count = n())

# map NAs
dat_2syl %>% map(~sum(is.na(.x)))

## Sum coding vowel
dat_2syl$vowel <- as.factor(dat_2syl$vowel)
contrasts(dat_2syl$vowel) = contr.sum(5)


## Models

## Model with full interactions and no random effects

model1_praat <- glm(syllable_number ~ (intensity + duration + f0_praat + f1 + f2)*vowel, data = dat_2syl %>% drop_na(intensity,Moras),family = 'binomial')

model1_reaper <- glm(syllable_number ~ (intensity + duration + f0_reaper + f1 + f2)*vowel, data = dat_2syl %>% drop_na(intensity,Moras),family = 'binomial')

## Model with vowel interacting only with F1/F2 and no random effects

model2_praat <- glm(syllable_number ~ intensity + duration + f0_praat + (f1 + f2)*vowel, data = dat_2syl %>% drop_na(intensity,Moras),family = 'binomial')

model2_reaper <- glm(syllable_number ~ intensity + duration + f0_reaper + (f1 + f2)*vowel, data = dat_2syl %>% drop_na(intensity,Moras),family = 'binomial')


## Model with full interactions and by-speaker and by-utterance random intercept

model3_praat <- lme4::glmer(syllable_number ~ (intensity + duration + f0_praat + f1 + f2)*vowel + (1|Speaker) + (1|word_unique), data = dat_2syl %>% drop_na(syllable_number),family = binomial)

model3_reaper <- lme4::glmer(syllable_number ~ (intensity + duration + f0_reaper + f1 + f2)*vowel + (1|Speaker) + (1|word_unique), data = dat_2syl %>% drop_na(syllable_number),family = binomial)

## Full model summary and effects

model3_praat %>% summary

model3_reaper %>% summary


effects::allEffects(model3_praat, partial.residuals=TRUE) %>% plot(multiline = T,rescale.axis=FALSE, residuals.pch=15)

effects::allEffects(model3_reaper, partial.residuals=TRUE) %>% plot(multiline = T,rescale.axis=FALSE, residuals.pch=15)

lme4::ranef(model3_praat)

## Plot variable importance from model without random effects

vip::vip(model1, out_var = 1)


varImp_model <- varImp(model1_reaper)

varImp_model <- varImp(model1_praat)
varImp_data <- varImp_model %>% as.data.frame() %>% arrange(desc(Overall)) %>% rownames_to_column()
varImp_data %>% ggplot(aes(fct_reorder(rowname,Overall), Overall)) + 
  geom_bar(stat = 'identity') +
  coord_flip()

## Doesn't work yet, but seeing if I can use predict() to see how good this model is as predicting the actual outcome
## Write contingency table

predict(model3_praat)

data = dat_2syl %>% drop_na(syllable_number)

y = data$syllable_number
  
predictions = ifelse (predict(model3) > 0, 0,1)

mean(predictions == y)





model %>% summary

anova(model,model2)

exp(coef(model))

emmeans::emmeans(model,~vowel|f0|f1|f2)






library(nnet)

## Sum coding vowel
dat_3syl$vowel <- as.factor(dat_3syl$vowel)
contrasts(dat_3syl$vowel) = contr.sum(5)


### Tables of Ns and co-occurrences
dat_3syl %>% pull(syllable_number) %>% table

dat_3syl %>% .$vowel %>% table

dat_3syl %>% group_by(vowel, syllable_number) %>%
  summarise(count = n())

# Fit the multinomial logistic regression model
# Add p-values to 3-syllable and 4-syllable models?
model <- multinom(syllable_number ~ (intensity + duration + f0 + f1 + f2)*vowel, data = dat_3syl %>% drop_na(intensity))
model %>% summary

# Add CIs?
model %>% effects::allEffects(partial.residuals=TRUE) %>% map(~.x %>% plot(multiline = T,rescale.axis=FALSE, residuals.pch=15))

library(caret)

# Use the varImp function from caret
varImp_model <- varImp(model)

# Plot the variable importance
plot(varImp_model)

varImp_data <- varImp_model %>% as.data.frame() %>% arrange(desc(Overall)) %>% rownames_to_column()

varImp_data %>% ggplot(aes(fct_reorder(rowname,Overall), Overall)) + 
  geom_bar(stat = 'identity') +
  coord_flip()



library(nnet)

### Tables of Ns and co-occurrences
dat_4syl %>% pull(syllable_number) %>% table

dat_4syl %>% .$vowel %>% table

dat_4syl %>% group_by(vowel, syllable_number) %>%
  summarise(count = n())


# Fit the multinomial logistic regression model
model <- multinom(syllable_number ~ (intensity + duration + f0_praat + f1 + f2)*vowel, data = dat_4syl %>% drop_na(intensity))

model1 <- multinom(syllable_number ~ intensity + duration + f0, data = dat_4syl %>% drop_na(intensity))

model2 <- multinom(syllable_number ~ (f1 + f2)*vowel, data = dat_4syl %>% drop_na(intensity))


model %>% summary

# Add CIs?
model %>% effects::allEffects(partial.residuals=TRUE) %>% map(~.x %>% plot(multiline = T,rescale.axis=FALSE, residuals.pch=15))

library(caret)

# Use the varImp function from caret
varImp_model <- varImp(model)

# Plot the variable importance
plot(varImp_model)

varImp_data <- varImp_model %>% as.data.frame() %>% arrange(desc(Overall)) %>% rownames_to_column()

varImp_data %>% ggplot(aes(fct_reorder(rowname,Overall), Overall)) + 
  geom_bar(stat = 'identity') +
  coord_flip()






#### Relative F0 stuff that's commented out for now ####

# ## Exclude outliers past 3sd from the mean
# 
# upperThreshold <- mean(short2$rel_f0, na.rm=T) + 3*sd(short2$rel_f0, na.rm=T)
# upperThreshold # 122
# lowerThreshold <- mean(short2$rel_f0, na.rm=T) - 3*sd(short2$rel_f0, na.rm=T)
# lowerThreshold # -99
# 
# short3 <- short2 %>%
#   filter( ! (rel_f0 > upperThreshold))
# short4 <- short3 %>%
#   filter( ! (rel_f0 < lowerThreshold))
# 
# short_2syll_f0 <- filter(short4, word_syllables == 2) %>% filter(syllable_stress != -2)
# short_3syll_f0 <- filter(short4, word_syllables == 3) %>% filter(syllable_stress != -2)
# short_4syll_f0 <- filter(short4, word_syllables == 4) %>% filter(syllable_stress != -2)
# 
# 
# ## 2-syllable words with just short vowels, relative f0
# 
# pirateplot(rel_f0 ~ syllable_stress,
#            data = short_2syll_f0)
# 
# p <- lmer(rel_f0 ~ syllable_stress + (1|Speaker),
#           data = short_2syll_f0)
# summary(p)
# 
# 
# ## 3-syllable words with just short vowels, relative f0
# 
# pirateplot(rel_f0 ~ syllable_stress,
#            data = short_3syll_f0)
# 
# p <- lmer(rel_f0 ~ syllable_stress + (1|Speaker),
#           data = short_3syll_f0)
# short_3syll_f0$syllable_stress <- as.factor(short_3syll_f0$syllable_stress)
# contrasts(short_3syll_f0$syllable_stress)
# summary(p)
# 
# short_3syll_f0$syllable_stress <- factor(short_3syll_f0$syllable_stress, levels = c('-1','-3'))
# contrasts(short_3syll_f0$syllable_stress)
# p <- lmer(rel_f0 ~ syllable_stress + (1|Speaker),
#           data = short_3syll_f0)
# summary(p)
# 
# 
# 
# ## 4-syllable words with just short vowels, relative f0
# 
# pirateplot(rel_f0 ~ syllable_stress,
#            data = short_4syll_f0)
# 
# p <- lm(rel_f0 ~ syllable_stress, # Singular fit if we include speaker random effect, oops
#         data = short_4syll_f0)
# short_4syll_f0$syllable_stress <- as.factor(short_4syll_f0$syllable_stress)
# contrasts(short_4syll_f0$syllable_stress)
# summary(p)
# 
# short_4syll_f0$syllable_stress <- factor(short_4syll_f0$syllable_stress, levels = c('-3','-1','-4'))
# contrasts(short_4syll_f0$syllable_stress)
# p <- lm(rel_f0 ~ syllable_stress,
#         data = short_4syll_f0)
# summary(p)
# 
# short_4syll_f0$syllable_stress <- factor(short_4syll_f0$syllable_stress, levels = c('-1','-3','-4'))
# contrasts(short_4syll_f0$syllable_stress)
# p <- lm(rel_f0 ~ syllable_stress,
#         data = short_4syll_f0)
# summary(p)




#### Hoʻo stuff ####
hoo <- c("hoʻo", "haʻa")

shortnothoo <- short2 %>%
  filter(duration < .25) %>%
  filter(word_syllables == 4) %>%
  filter( !grepl(paste(hoo, collapse = "|"),word))

shorthoo <- short2 %>%
  filter(duration < .25) %>%
  filter(word_syllables == 4) %>%
  filter(grepl(paste(hoo, collapse = "|"),word))

pirateplot(duration ~ syllable_stress,
           data = shortnothoo )

pirateplot(duration ~ syllable_stress,
           data = shorthoo )



#### Simple models ####

### Intensity
## 2-syllable words with just short vowels, intensity
p1 <- lmer(intensity ~ syllable_number*vowel + (1|Speaker) + (1|word_unique),
           data = dat_2syl)

p2 <- lmer(intensity ~ syllable_number*vowel + (1 + syllable_number|Speaker) + (1|word_unique),
           data = dat_2syl)

anova(p1,p2) # p2 has significantly lower AIC

emmeans(p1, specs = pairwise ~ syllable_number)
emmeans(p2, specs = pairwise ~ syllable_number)


## 3-syllable words with just short vowels, intensity
p1 <- lmer(intensity ~ syllable_number*vowel + (1|Speaker) + (1|word_unique),
           data = dat_3syl)

p2 <- lmer(intensity ~ syllable_number*vowel + (1+syllable_number|Speaker) + (1|word_unique),
           data = dat_3syl)

anova(p1,p2)

emmeans(p1, specs = pairwise ~ syllable_number)
emmeans(p2, specs = pairwise ~ syllable_number)


## 4-syllable words with just short vowels, intensity
p1 <- lmer(intensity ~ syllable_number*vowel + (1|Speaker) + (1|word_unique),
           data = dat_4syl)

p2 <- lmer(intensity ~ syllable_number*vowel + (1 + syllable_number|Speaker) + (1|word_unique),
           data = dat_4syl)

anova(p1,p2)

emmeans(p1, specs = pairwise ~ syllable_number)
emmeans(p2, specs = pairwise ~ syllable_number)


## 2-syllable words with just long vowels, intensity
dat_2syl_long_nokuma <- dat_2syl_long %>%
  filter(word != "kūmā")

# p1 <- lmer(intensity ~ syllable_number*vowel + (1|Speaker) + (1|word_unique),
#          data = dat_2syl_long)

# p2 <- lmer(intensity ~ syllable_number*vowel + (1 + syllable_number|Speaker) + (1|word_unique),
#          data = dat_2syl_long)

p3 <- lmer(intensity ~ syllable_number + (1|Speaker) + (1|word_unique) + (1|vowel),
           data = dat_2syl_long_nokuma)

p4 <- lmer(intensity ~ syllable_number + (1 + syllable_number|Speaker) + (1|word_unique) + (1|vowel),
           data = dat_2syl_long_nokuma)

# anova(p1,p2)
anova(p3,p4)

# emmeans(p1, specs = pairwise ~ syllable_number)
# emmeans(p2, specs = pairwise ~ syllable_number)
emmeans(p3, specs = pairwise ~ syllable_number)
emmeans(p4, specs = pairwise ~ syllable_number)


## 2-syllable short vs. 2-syllable long, intensity

dat_2syll_shortlong_nokuma <- dat_2syll_shortlong %>%
  filter(word != "kūmā")

# p1 <- lmer(intensity ~ syllable_number*length*vowel + (1|Speaker) + (1|word_unique),
#          data = dat_2syll_shortlong)

# p2 <- lmer(intensity ~ syllable_number*length*vowel + (1 + syllable_number|Speaker) + (1|word_unique),
#          data = dat_2syll_shortlong)

p3 <- lmer(intensity ~ syllable_number*length + (1|Speaker) + (1|word_unique) + (1|vowel),
           data = dat_2syll_shortlong_nokuma)

p4 <- lmer(intensity ~ syllable_number*length + (1 + syllable_number|Speaker) + (1|word_unique) + (1|vowel),
           data = dat_2syll_shortlong_nokuma)

# anova(p1,p2)
anova(p3,p4)


# emmeans(p1, specs = pairwise ~ syllable_number*length)
# emmeans(p2, specs = pairwise ~ syllable_number*length)

emmeans(p3, specs = pairwise ~ syllable_number*length)

summary(p4)
emmeans(p4, specs = pairwise ~ syllable_number*length)



## 4-syll short vs. 2-syll long primary vs secondary, intensity
dat_primarysecondary_nokuma <- dat_primarysecondary %>%
  filter(word != "kūmā")

p1 <- lmer(intensity ~ stress*length*vowel + (1|Speaker) + (1|word_unique),
           data = dat_primarysecondary)

p2 <- lmer(intensity ~ stress*length*vowel + (1 + stress|Speaker) + (1|word_unique),
           data = dat_primarysecondary)

p3 <- lmer(intensity ~ stress*length + (1|Speaker) + (1|word_unique) + (1|vowel),
           data = dat_primarysecondary)

p4 <- lmer(intensity ~ stress*length + (1 + stress|Speaker) + (1|word_unique) +(1|vowel),
           data = dat_primarysecondary)

anova(p1,p2)
anova(p3,p4)


# emmeans(p1, specs = pairwise ~ stress*length)
# emmeans(p2, specs = pairwise ~ stress*length)
emmeans(p3, specs = pairwise ~ stress*length)
emmeans(p4, specs = pairwise ~ stress*length)



## 3-syll short vs. 4-syll short, intensity
p1 <- lmer(intensity ~ syll_comp*vowel + (1|Speaker) + (1|word_unique),
           data = dat_34syll)

p2 <- lmer(intensity ~ syll_comp*vowel + (1 + syll_comp|Speaker) + (1|word_unique),
           data = dat_34syll)

anova(p1,p2)

emmeans(p1, specs = pairwise ~ syll_comp)
emmeans(p2, specs = pairwise ~ syll_comp)




### F0 Praat
## 2-syllable words with just short vowels, f0_praat
p1 <- lmer(f0_praat ~ syllable_number*vowel + (1|Speaker) + (1|word_unique),
           data = dat_2syl)

p2 <- lmer(f0_praat ~ syllable_number*vowel + (1 + syllable_number|Speaker) + (1|word_unique),
           data = dat_2syl)

anova(p1,p2) # p2 has significantly lower AIC but gives a convergence error

emmeans(p1, specs = pairwise ~ syllable_number)
emmeans(p2, specs = pairwise ~ syllable_number)


## 3-syllable words with just short vowels, f0_praat
p1 <- lmer(f0_praat ~ syllable_number*vowel + (1|Speaker) + (1|word_unique),
           data = dat_3syl)

p2 <- lmer(f0_praat ~ syllable_number*vowel + (1+syllable_number|Speaker) + (1|word_unique),
           data = dat_3syl)

anova(p1,p2)

emmeans(p1, specs = pairwise ~ syllable_number)
emmeans(p2, specs = pairwise ~ syllable_number)


## 4-syllable words with just short vowels, f0_praat
p1 <- lmer(f0_praat ~ syllable_number*vowel + (1|Speaker) + (1|word_unique),
           data = dat_4syl)

p2 <- lmer(f0_praat ~ syllable_number*vowel + (1 + syllable_number|Speaker) + (1|word_unique),
           data = dat_4syl)

anova(p1,p2)

emmeans(p1, specs = pairwise ~ syllable_number)
emmeans(p2, specs = pairwise ~ syllable_number)


## 2-syllable words with just long vowels, f0_praat
dat_2syl_long_nokuma <- dat_2syl_long %>%
  filter(word != "kūmā")

# p1 <- lmer(f0_praat ~ syllable_number*vowel + (1|Speaker) + (1|word_unique),
#        data = dat_2syl_long)

# p2 <- lmer(f0_praat ~ syllable_number*vowel + (1 + syllable_number|Speaker) + (1|word_unique),
#         data = dat_2syl_long)

p3 <- lmer(f0_praat ~ syllable_number + (1|Speaker) + (1|word_unique) + (1|vowel),
           data = dat_2syl_long)

p4 <- lmer(f0_praat ~ syllable_number + (1 + syllable_number|Speaker) + (1|word_unique) + (1|vowel),
           data = dat_2syl_long)

# anova(p1,p2)
anova(p3,p4)

# emmeans(p1, specs = pairwise ~ syllable_number)
# emmeans(p2, specs = pairwise ~ syllable_number)
emmeans(p3, specs = pairwise ~ syllable_number)
emmeans(p4, specs = pairwise ~ syllable_number)


## 2-syllable short vs. 2-syllable long, f0_praat

dat_2syll_shortlong_nokuma <- dat_2syll_shortlong %>%
  filter(word != "kūmā")

# p1 <- lmer(f0_praat ~ syllable_number*length*vowel + (1|Speaker) + (1|word_unique),
#         data = dat_2syll_shortlong)

# p2 <- lmer(f0_praat ~ syllable_number*length*vowel + (1 + syllable_number|Speaker) + (1|word_unique),
#         data = dat_2syll_shortlong)

p3 <- lmer(f0_praat ~ syllable_number*length + (1|Speaker) + (1|word_unique) + (1|vowel),
           data = dat_2syll_shortlong_nokuma)

p4 <- lmer(f0_praat ~ syllable_number*length + (1 + syllable_number|Speaker) + (1|word_unique) + (1|vowel),
           data = dat_2syll_shortlong_nokuma)

# anova(p1,p2)
anova(p3,p4)


# emmeans(p1, specs = pairwise ~ syllable_number*length)
# emmeans(p2, specs = pairwise ~ syllable_number*length)

emmeans(p3, specs = pairwise ~ syllable_number*length)

summary(p4)
emmeans(p4, specs = pairwise ~ syllable_number*length)



## 4-syll short vs. 2-syll long primary vs secondary, f0_praat
dat_primarysecondary_nokuma <- dat_primarysecondary %>%
  filter(word != "kūmā")

# p1 <- lmer(f0_praat ~ stress*length*vowel + (1|Speaker) + (1|word_unique),
#           data = dat_primarysecondary)

# p2 <- lmer(f0_praat ~ stress*length*vowel + (1 + stress|Speaker) + (1|word_unique),
#           data = dat_primarysecondary)

p3 <- lmer(f0_praat ~ stress*length + (1|Speaker) + (1|word_unique) + (1|vowel),
           data = dat_primarysecondary_nokuma)

p4 <- lmer(f0_praat ~ stress*length + (1 + stress|Speaker) + (1|word_unique) +(1|vowel),
           data = dat_primarysecondary_nokuma)

anova(p1,p2)
anova(p3,p4)


#emmeans(p1, specs = pairwise ~ stress*length)
#emmeans(p2, specs = pairwise ~ stress*length)
emmeans(p3, specs = pairwise ~ stress*length)
emmeans(p4, specs = pairwise ~ stress*length)



## 3-syll short vs. 4-syll short, f0_praat
p1 <- lmer(f0_praat ~ syll_comp*vowel + (1|Speaker) + (1|word_unique),
           data = dat_34syll)

p2 <- lmer(f0_praat ~ syll_comp*vowel + (1 + syll_comp|Speaker) + (1|word_unique),
           data = dat_34syll)

anova(p1,p2)

emmeans(p1, specs = pairwise ~ syll_comp)
emmeans(p2, specs = pairwise ~ syll_comp)



### Duration
## 2-syllable words with just short vowels, duration
p1 <- lmer(duration ~ syllable_number*vowel + (1|Speaker) + (1|word_unique),
           data = dat_2syl)

p2 <- lmer(duration ~ syllable_number*vowel + (1 + syllable_number|Speaker) + (1|word_unique),
           data = dat_2syl)

anova(p1,p2) # p2 has significantly lower AIC but gives a convergence error

emmeans(p1, specs = pairwise ~ syllable_number)
emmeans(p2, specs = pairwise ~ syllable_number)


## 3-syllable words with just short vowels, duration
p1 <- lmer(duration ~ syllable_number*vowel + (1|Speaker) + (1|word_unique),
           data = dat_3syl)

p2 <- lmer(duration ~ syllable_number*vowel + (1+syllable_number|Speaker) + (1|word_unique),
           data = dat_3syl)

anova(p1,p2)

emmeans(p1, specs = pairwise ~ syllable_number)
emmeans(p2, specs = pairwise ~ syllable_number)


## 4-syllable words with just short vowels, duration
p1 <- lmer(duration ~ syllable_number*vowel + (1|Speaker) + (1|word_unique),
           data = dat_4syl)

p2 <- lmer(duration ~ syllable_number*vowel + (1 + syllable_number|Speaker) + (1|word_unique),
           data = dat_4syl)

anova(p1,p2)

emmeans(p1, specs = pairwise ~ syllable_number)
emmeans(p2, specs = pairwise ~ syllable_number)


## 2-syllable words with just long vowels, duration
dat_2syl_long_nokuma <- dat_2syl_long %>%
  filter(word != "kūmā")

# p1 <- lmer(duration ~ syllable_number*vowel + (1|Speaker) + (1|word_unique),
#        data = dat_2syl_long)

# p2 <- lmer(duration ~ syllable_number*vowel + (1 + syllable_number|Speaker) + (1|word_unique),
#         data = dat_2syl_long)

p3 <- lmer(duration ~ syllable_number + (1|Speaker) + (1|word_unique) + (1|vowel),
           data = dat_2syl_long_nokuma)

p4 <- lmer(duration ~ syllable_number + (1 + syllable_number|Speaker) + (1|word_unique) + (1|vowel),
           data = dat_2syl_long_nokuma)

# anova(p1,p2)
anova(p3,p4)

# emmeans(p1, specs = pairwise ~ syllable_number)
# emmeans(p2, specs = pairwise ~ syllable_number)
emmeans(p3, specs = pairwise ~ syllable_number)
emmeans(p4, specs = pairwise ~ syllable_number)


## 2-syllable short vs. 2-syllable long, duration

dat_2syll_shortlong_nokuma <- dat_2syll_shortlong %>%
  filter(word != "kūmā")

# p1 <- lmer(duration ~ syllable_number*length*vowel + (1|Speaker) + (1|word_unique),
#         data = dat_2syll_shortlong)

# p2 <- lmer(duration ~ syllable_number*length*vowel + (1 + syllable_number|Speaker) + (1|word_unique),
#         data = dat_2syll_shortlong)

p3 <- lmer(duration ~ syllable_number*length + (1|Speaker) + (1|word_unique) + (1|vowel),
           data = dat_2syll_shortlong)

p4 <- lmer(duration ~ syllable_number*length + (1 + syllable_number|Speaker) + (1|word_unique) + (1|vowel),
           data = dat_2syll_shortlong)

# anova(p1,p2)
anova(p3,p4)


# emmeans(p1, specs = pairwise ~ syllable_number*length)
# emmeans(p2, specs = pairwise ~ syllable_number*length)

emmeans(p3, specs = pairwise ~ syllable_number*length)

summary(p3)
emmeans(p3, specs = pairwise ~ syllable_number*length)

summary(p4)
emmeans(p4, specs = pairwise ~ syllable_number*length)



## 4-syll short vs. 2-syll long primary vs secondary, duration
dat_primarysecondary_nokuma <- dat_primarysecondary %>%
  filter(word != "kūmā")

# p1 <- lmer(duration ~ stress*length*vowel + (1|Speaker) + (1|word_unique),
#           data = dat_primarysecondary)

# p2 <- lmer(duration ~ stress*length*vowel + (1 + stress|Speaker) + (1|word_unique),
#           data = dat_primarysecondary)

p3 <- lmer(duration ~ stress*length + (1|Speaker) + (1|word_unique) + (1|vowel),
           data = dat_primarysecondary_nokuma)

p4 <- lmer(duration ~ stress*length + (1 + stress|Speaker) + (1|word_unique) +(1|vowel),
           data = dat_primarysecondary_nokuma)

anova(p1,p2)
anova(p3,p4)


#emmeans(p1, specs = pairwise ~ stress*length)
#emmeans(p2, specs = pairwise ~ stress*length)
summary(p3)
emmeans(p3, specs = pairwise ~ stress*length)

summary(p4)
emmeans(p4, specs = pairwise ~ stress*length)



## 3-syll short vs. 4-syll short, duration
p1 <- lmer(duration ~ syll_comp*vowel + (1|Speaker) + (1|word_unique),
           data = dat_34syll)

p2 <- lmer(duration ~ syll_comp*vowel + (1 + syll_comp|Speaker) + (1|word_unique),
           data = dat_34syll)

anova(p1,p2)

emmeans(p1, specs = pairwise ~ syll_comp)
emmeans(p2, specs = pairwise ~ syll_comp)





#### Individual BRMS models ####

library(gridExtra)

model_2syll_int <- read_rds("model_2syll_int.rds")
predictions(
  model_2syll_int,
  by = c("syllable_number", "length"))
a <- plot_predictions(model_2syll_int,
                      by = c("syllable_number", "length"))

model_2syll_f0 <- read_rds("model_2syll_f0.rds")
predictions(
  model_2syll_f0,
  by = c("syllable_number", "length"))
b <- plot_predictions(model_2syll_f0,
                      by = c("syllable_number", "length"))

model_2syll_dur <- read_rds("model_2syll_dur.rds")
predictions(
  model_2syll_dur,
  by = c("syllable_number", "length"))
c <- plot_predictions(model_2syll_dur,
                      by = c("syllable_number", "length"))


model_3syll_int <- read_rds("model_3syll_int.rds")
predictions(
  model_3syll_int,
  by = c("syllable_number", "length"))
d <- plot_predictions(model_3syll_int,
                      by = c("syllable_number", "length"))

model_3syll_f0 <- read_rds("model_3syll_f0.rds")
predictions(
  model_3syll_f0,
  by = c("syllable_number", "length"))
e <- plot_predictions(model_3syll_f0,
                      by = c("syllable_number", "length"))

model_3syll_dur <- read_rds("model_3syll_dur.rds")
predictions(
  model_3syll_dur,
  by = c("syllable_number", "length"))
f <- plot_predictions(model_3syll_dur,
                      by = c("syllable_number", "length"))


model_4syll_int <- read_rds("model_4syll_int.rds")
predictions(
  model_4syll_int,
  by = c("syllable_number", "length"))
g <- plot_predictions(model_4syll_int,
                      by = c("syllable_number", "length"))

model_4syll_f0 <- read_rds("model_4syll_f0.rds")
predictions(
  model_4syll_f0,
  by = c("syllable_number", "length"))
h <- plot_predictions(model_4syll_f0,
                      by = c("syllable_number", "length"))

model_4syll_dur <- read_rds("model_4syll_dur.rds")
predictions(
  model_4syll_dur,
  by = c("syllable_number", "length"))
i <- plot_predictions(model_4syll_dur,
                      by = c("syllable_number", "length"))


model_2syll_long_int <- read_rds("model_2syll_long_int.rds")
predictions(
  model_2syll_long_int,
  by = c("syllable_number", "length"))
j <- plot_predictions(model_2syll_long_int,
                      by = c("syllable_number", "length"))

model_2syll_long_f0 <- read_rds("model_2syll_long_f0.rds")
predictions(
  model_2syll_long_f0,
  by = c("syllable_number", "length"))
k <- plot_predictions(model_2syll_long_f0,
                      by = c("syllable_number", "length"))

model_2syll_long_dur <- read_rds("model_2syll_long_dur.rds")
predictions(
  model_2syll_long_dur,
  by = c("syllable_number", "length"))
l <- plot_predictions(model_2syll_long_dur,
                      by = c("syllable_number", "length"))

model_2syll_shortlong_int <- read_rds("model_2syll_shortlong_int.rds")
predictions(
  model_2syll_shortlong_int,
  by = c("syllable_number", "length"))
m <- plot_predictions(model_2syll_shortlong_int,
                      by = c("syllable_number", "length"))

model_2syll_shortlong_f0 <- read_rds("model_2syll_shortlong_f0.rds")
predictions(
  model_2syll_shortlong_f0,
  by = c("syllable_number", "length"))
n <- plot_predictions(model_2syll_shortlong_f0,
                      by = c("syllable_number", "length"))

model_2syll_shortlong_dur <- read_rds("model_2syll_shortlong_dur.rds")
predictions(
  model_2syll_shortlong_dur,
  by = c("syllable_number", "length"))
o <- plot_predictions(model_2syll_shortlong_dur,
                      by = c("syllable_number", "length"))


## Primary vs. secondary

model_primarysecondary_int <- read_rds("model_primarysecondary_int.rds")
p_data <- predictions(
  model_primarysecondary_int,
  by = c("stress", "length"))
p_data$stress <- factor(p_data$stress, 
                        levels = c("secondary", "primary"))
p <- ggplot(p_data, aes(x = stress, y = estimate, color = length)) +
  geom_point(position = position_dodge(width = 0.3)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0, position = position_dodge(width = 0.3)) +
  theme_minimal() +
  labs(x = "Stress", y = "Intensity")


model_primarysecondary_f0 <- read_rds("model_primarysecondary_f0.rds")
q_data <- predictions(
  model_primarysecondary_f0,
  by = c("stress", "length"))
q_data$stress <- factor(q_data$stress, 
                        levels = c("secondary", "primary"))
q <- ggplot(q_data, aes(x = stress, y = estimate, color = length)) +
  geom_point(position = position_dodge(width = 0.3)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0, position = position_dodge(width = 0.3)) +
  theme_minimal() +
  labs(x = "Stress", y = "f0")

model_primarysecondary_dur <- read_rds("model_primarysecondary_dur.rds")
r_data <- predictions(
  model_primarysecondary_dur,
  by = c("stress", "length"))
r_data$stress <- factor(r_data$stress, 
                        levels = c("secondary", "primary"))
r <- ggplot(r_data, aes(x = stress, y = estimate, color = length)) +
  geom_point(position = position_dodge(width = 0.3)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0, position = position_dodge(width = 0.3)) +
  theme_minimal() +
  labs(x = "Stress", y = "duration")

model_34syll_int <- read_rds("model_34syll_int.rds")
predictions(
  model_34syll_int,
  by = c("syllable_number", "word_syllables"))
s <- plot_predictions(model_34syll_int,
                      by = c("syllable_number", "word_syllables"))

model_34syll_f0 <- read_rds("model_34syll_f0.rds")
predictions(
  model_34syll_f0,
  by = c("syllable_number", "word_syllables"))
t <- plot_predictions(model_34syll_f0,
                      by = c("syllable_number", "word_syllables"))

model_34syll_dur <- read_rds("model_34syll_dur.rds")
predictions(
  model_34syll_dur,
  by = c("syllable_number", "word_syllables"))
u <- plot_predictions(model_34syll_dur,
                      by = c("syllable_number", "word_syllables"))

grid.arrange(#a, b, c, 
  #d, e, f, 
  #g, h, i, 
  #j, k, l,
  m, n, o, 
  s, t, u, 
  p, q, r, 
  nrow = 3, ncol = 3)


## Investigating kūmā

kuma <- dat_2syl_long %>%
  filter(word == "kūmā")
all_but_kuma <- dat_2syl_long %>%
  filter(word != "kūmā")
pirateplot(data = kuma, 
           formula = intensity ~ syllable_number)
pirateplot(data = kuma, 
           formula = f0_praat ~ syllable_number)
pirateplot(data = kuma, 
           formula = duration ~ syllable_number)
pirateplot(data = all_but_kuma, 
           formula = intensity ~ syllable_number)
pirateplot(data = all_but_kuma, 
           formula = f0_praat ~ syllable_number)
pirateplot(data = all_but_kuma, 
           formula = duration ~ syllable_number)

library(cowplot)

plot_grid(
  plotlist = list(
    capture.output(aa(), file = NULL),
    capture.output(bb(), file = NULL),
    capture.output(cc(), file = NULL)
    # Add the rest of your pirateplot functions here
  ),
  ncol = 1
)


#### Compare praat readings to reaper readings #####
ggplot(data5, aes(x = f0_praat, y = f0_reaper)) +
  geom_point() +  # Scatter plot of points
  geom_abline(slope = 0.5, intercept = 0, color = "blue", linetype = "dashed") +
  geom_abline(slope = 0.75, intercept = 0, color = "purple") +
  geom_abline(slope = 1.5, intercept = 0, color = "purple") +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  geom_abline(slope = 2, intercept = 0, color = "green", linetype = "dashed") +
  labs(title = "Scatterplot of f0_reaper vs. f0_praat",
       x = "f0_praat",
       y = "f0_reaper") +
  theme_minimal() +
  scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red"))  +
  facet_wrap(~ Speaker) 


ggplot(data5, aes(x = f0_fasttrack, y = f0_praat)) +
  geom_point() +  # Scatter plot of points
  geom_abline(slope = 0.5, intercept = 0, color = "blue", linetype = "dashed") +
  geom_abline(slope = 0.75, intercept = 0, color = "purple") +
  geom_abline(slope = 1.5, intercept = 0, color = "purple") +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  geom_abline(slope = 2, intercept = 0, color = "green", linetype = "dashed") +
  labs(title = "Scatterplot of f0_reaper vs. f0_fasttrack",
       x = "f0_praat",
       y = "f0_fasttrack") +
  theme_minimal() +
  scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red"))  +
  facet_wrap(~ Speaker) 

