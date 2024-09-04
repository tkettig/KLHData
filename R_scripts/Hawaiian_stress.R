### 2 Sept 2024
### Phonetic correlates of stress in Hawaiian
library(tidyverse)
library(phonR)
library(lme4)
library(yarrr)
library(emmeans)
library(scales)
library(cowplot)
library(caret)

setwd("/Users/Thomas/Documents/Hawaiian_Phonetics/KLHData/R_scripts/")

## Gonna give myself two types of negated %in% here
`%!in%` = Negate(`%in%`)
`%notin%` <- Negate(`%in%`)

####### Make lists of vowels/mono/diph #######

list_of_monophthongs <- c("ā","ē","ī","ō","ū","a","e","i","o","u")
list_of_diphthongs <- c("ai","au","ao","ae","ei", "ou","eu","oi","iu","āi","āe","āu","āo","ēi","ōu")
list_of_vowels <- c("ā","ē","ī","ō","ū","a","e","i","o","u","ai","au","ao","ae","ei", "ou","eu","oi","iu","āi","āe","āu","āo","ēi","ōu")
list_of_long_mono <- c("ā","ē","ī","ō","ū")
list_of_short_mono <- c("a","e","i","o","u")

## This takes a few minutes to run all the data extraction
# source("3_normalize.R")

## This just gets the .csv with all the data, pre-extracted
data <- read.csv('all_data_2Sept2024.csv')

#### Find and add in the midpoints #####

## Getting which point is the midpoint for each vowel, since some of the midpoints have now been excluded due to being outliers

midpoints <- data %>%
  group_by(filename) %>%
  mutate(midpoint = time) %>%
  slice(which.min(abs(time - 5.1))) %>%
  ungroup %>%
  select(filename,midpoint)

data_midpoints <- left_join(data,midpoints, by="filename") %>% 
  filter(time==midpoint) %>%
  rename(f1_mid = f1_normed, f2_mid = f2_normed, f3_mid = f3_normed, mahal_dist_mid = mahal_dist, time_mid = time) %>%
  ungroup()

## Now get max/min F1 and F2 points

setwd("/Users/Thomas/Documents/Hawaiian_Phonetics/Dissertation/R_scripts/")
source("max_min_points.R")

## This merges these readings into a single dataframe such that a/ā are measured at Max F1, etc.

max_f2 <- max_f2 %>% filter(vowel=="e"|vowel=="ē"|vowel=="i"|vowel=="ī")
min_f2 <- min_f2 %>% filter(vowel=="o"|vowel=="ō"|vowel=="u"|vowel=="ū"|vowel=="oa") # putting oa with /o/ rather than /a/ because when checking the midpoint plots, it looks like this is a more accurate representation of the proper measurement. still picks up on the lowering but does not force it.
max_f1 <- max_f1 %>% filter(!(vowel=="e"|vowel=="ē"|vowel=="i"|vowel=="ī"|vowel=="o"|vowel=="ō"|vowel=="u"|vowel=="ū"|vowel=="oa")) # making maxf1 for the rest of them; will eventually only want this for a and ā, but need placeholders for diphthongs

data4 <- rbind(max_f1,max_f2,min_f2) %>% 
  ungroup() %>%
  select(filename, time:f3)


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
  select(-midpoint)

## Rename so itʻs clear that f1/f2/f3 are of the inflection point

data5 <- data5 %>% rename(f1_normed_mid = f1_mid,
                          f2_normed_mid = f2_mid,
                          f3_normed_mid = f3_mid,
                          f0_reaper = meanf0,
                          f0_reaper_median = medianf0,
                          f0_praat = f0)


### Make column for position/length

data5$length <- ifelse(data5$Syllabification == "Diph", "diph",
                       ifelse(data5$Moras == 2, "long", "short"))
data5$position_length <- paste(data5$syllable_number, data5$length, sep=".")

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
  select(word_unique, word_syllables) %>%
  group_by(word_unique, word_syllables) %>%
  summarise(count = length(word_unique)) %>%
  filter(count == word_syllables)
short2 <- only_shortV %>%
  filter(word_unique %in% short1$word_unique) %>%
  arrange(word_unique, interval)


## Renaming syllables
short2$syllable_stress <- ifelse(short2$syllable_number == -1, "Final Unstressed",
                                  ifelse(short2$syllable_number == -2, "Primary Stressed",
                                         ifelse(short2$syllable_number == -3, "Antepen. Unstressed",
                                                "Secondary Stressed")))
short2$syllable_stress <- factor(short2$syllable_stress, levels=c("Secondary Stressed","Antepen. Unstressed","Primary Stressed", "Final Unstressed"))


#### Arjun Models #####

## Choose to either log transform or get rid of some duration outliers
short2$log_duration <- log(short2$duration)

##### 2 syllables ######

dat_2syl <- short2 %>% filter(word_syllables == 2, str_length(vowel) == 1, Moras == 1)

dat_2syl <- dat_2syl %>% select(f0_reaper, f0_praat, Moras,intensity, log_duration, f1_normed_inf, f2_normed_inf,syllable_number,Speaker,word_unique,vowel) %>% mutate(syllable_number = ifelse(syllable_number == -1,0,1),
                                                                                                                                                      across(c(f0_reaper,f0_praat, intensity,log_duration,f1_normed_inf,f2_normed_inf),scale)
) %>% rename(f1 = f1_normed_inf, f2 = f2_normed_inf, duration = log_duration)

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

model2 <- glm(syllable_number ~ intensity + duration + f0 + (f1 + f2)*vowel, data = dat_2syl %>% drop_na(intensity,Moras),family = 'binomial')

## Model with full interactions and by-speaker and by-utterance random intercept

model3_praat <- lme4::glmer(syllable_number ~ (intensity + duration + f0_praat + f1 + f2)*vowel + (1|Speaker) + (1|word_unique), data = dat_2syl %>% drop_na(syllable_number),family = binomial)

model3_reaper <- lme4::glmer(syllable_number ~ (intensity + duration + f0_reaper + f1 + f2)*vowel + (1|Speaker) + (1|word_unique), data = dat_2syl %>% drop_na(syllable_number),family = binomial)

## Full model summary and effects

model3 %>% summary

effects::allEffects(model3, partial.residuals=TRUE) %>% plot(multiline = T,rescale.axis=FALSE, residuals.pch=15)

effects::allEffects(model3_praat, partial.residuals=TRUE) %>% plot(multiline = T,rescale.axis=FALSE, residuals.pch=15)

effects::allEffects(model3_reaper, partial.residuals=TRUE) %>% plot(multiline = T,rescale.axis=FALSE, residuals.pch=15)


lme4::ranef(model3)

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

predict(model3)

data = dat_2syl %>% drop_na(syllable_number)

y = data$syllable_number
  
predictions = ifelse (predict(model3) > 0, 0,1)

mean(predictions == y)





model %>% summary

anova(model,model2)

exp(coef(model))

emmeans::emmeans(model,~vowel|f0|f1|f2)



# 3 syllables

dat_3syl <- short2 %>% filter(word_syllables == 3, str_length(vowel) == 1, Moras == 1)

dat_3syl %>% pull(syllable_number) %>% table

dat_3syl <- dat_3syl %>% select(F0_reaper, Moras,intensity, duration, f1_normed_inf, f2_normed_inf,syllable_number,Speaker,word_unique,vowel) %>% mutate(across(c(F0_reaper,intensity,duration,f1_normed_inf,f2_normed_inf),scale)
) %>% rename(f0 = F0_reaper, f1 = f1_normed_inf, f2 = f2_normed_inf)


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

# 4 syllables

dat_4syl <- short2 %>% filter(word_syllables == 4, str_length(vowel) == 1, Moras == 1)

dat_4syl %>% pull(syllable_number) %>% table

dat_4syl <- dat_4syl %>% select(F0_reaper, Moras,intensity, duration, f1_normed_inf, f2_normed_inf,syllable_number,Speaker,word_unique,vowel) %>% mutate(across(c(F0_reaper,intensity,duration,f1_normed_inf,f2_normed_inf),scale)
) %>% rename(f0 = F0_reaper, f1 = f1_normed_inf, f2 = f2_normed_inf)


library(nnet)

### Tables of Ns and co-occurrences
dat_4syl %>% pull(syllable_number) %>% table

dat_4syl %>% .$vowel %>% table

dat_4syl %>% group_by(vowel, syllable_number) %>%
  summarise(count = n())


# Fit the multinomial logistic regression model
model <- multinom(syllable_number ~ (intensity + duration + f0 + f1 + f2)*vowel, data = dat_4syl %>% drop_na(intensity))

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













###### Stuff from before Arjun #######

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
  group_by(syllable_stress, position_length, length, syllable_number, word_syllables) %>% 
  summarise(mean = mean(intensity, na.rm=T), 
            sd = sd(intensity, na.rm=T), 
            count = n())
short_2syll_int_means$se <- short_2syll_int_means$sd / sqrt(short_2syll_int_means$count)
short_2syll_int_means$tval <- qt(0.05/2, df = short_2syll_int_means$count - 1)
short_2syll_int_means$moe <- short_2syll_int_means$tval * short_2syll_int_means$se * -1
short_2syll_int_means$upper <- short_2syll_int_means$mean + short_2syll_int_means$moe
short_2syll_int_means$lower <- short_2syll_int_means$mean - short_2syll_int_means$moe

short_3syll_int_means = short_3syll_int %>% 
  group_by(syllable_stress, position_length, length, syllable_number, word_syllables) %>% 
  summarise(mean = mean(intensity, na.rm=T), 
            sd = sd(intensity, na.rm=T), 
            count = n())
short_3syll_int_means$se <- short_3syll_int_means$sd / sqrt(short_3syll_int_means$count)
short_3syll_int_means$tval <- qt(0.05/2, df = short_3syll_int_means$count - 1)
short_3syll_int_means$moe <- short_3syll_int_means$tval * short_3syll_int_means$se * -1
short_3syll_int_means$upper <- short_3syll_int_means$mean + short_3syll_int_means$moe
short_3syll_int_means$lower <- short_3syll_int_means$mean - short_3syll_int_means$moe

short_4syll_int_means = short_4syll_int %>% 
  group_by(syllable_stress, position_length, length, syllable_number, word_syllables) %>% 
  summarise(mean = mean(intensity, na.rm=T), 
            sd = sd(intensity, na.rm=T), 
            count = n())
short_4syll_int_means$se <- short_4syll_int_means$sd / sqrt(short_4syll_int_means$count)
short_4syll_int_means$tval <- qt(0.05/2, df = short_4syll_int_means$count - 1)
short_4syll_int_means$moe <- short_4syll_int_means$tval * short_4syll_int_means$se * -1
short_4syll_int_means$upper <- short_4syll_int_means$mean + short_4syll_int_means$moe
short_4syll_int_means$lower <- short_4syll_int_means$mean - short_4syll_int_means$moe



#### Setting up duration measures, short ####

ggplot(short2,aes(x=duration))+geom_histogram(bins=30)

short3 <- short2 %>%
  filter( ! (duration > .25))

# Now get only the words where we have full info for all syllables
short1 <- short3 %>%
  select(word_unique, word_syllables) %>%
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
  group_by(syllable_stress, position_length, length, syllable_number, word_syllables) %>% 
  summarise(mean = mean(duration), 
            sd = sd(duration), 
            count = n())
short_2syll_dur_means$se <- short_2syll_dur_means$sd / sqrt(short_2syll_dur_means$count)
short_2syll_dur_means$tval <- qt(0.05/2, df = short_2syll_dur_means$count - 1)
short_2syll_dur_means$moe <- short_2syll_dur_means$tval * short_2syll_dur_means$se * -1
short_2syll_dur_means$upper <- short_2syll_dur_means$mean + short_2syll_dur_means$moe
short_2syll_dur_means$lower <- short_2syll_dur_means$mean - short_2syll_dur_means$moe

short_3syll_dur_means = short_3syll_dur %>% 
  group_by(syllable_stress, position_length, length, syllable_number, word_syllables) %>% 
  summarise(mean = mean(duration), 
            sd = sd(duration), 
            count = n())
short_3syll_dur_means$se <- short_3syll_dur_means$sd / sqrt(short_3syll_dur_means$count)
short_3syll_dur_means$tval <- qt(0.05/2, df = short_3syll_dur_means$count - 1)
short_3syll_dur_means$moe <- short_3syll_dur_means$tval * short_3syll_dur_means$se * -1
short_3syll_dur_means$upper <- short_3syll_dur_means$mean + short_3syll_dur_means$moe
short_3syll_dur_means$lower <- short_3syll_dur_means$mean - short_3syll_dur_means$moe

short_4syll_dur_means = short_4syll_dur %>% 
  group_by(syllable_stress, position_length, length, syllable_number, word_syllables) %>% 
  summarise(mean = mean(duration), 
            sd = sd(duration), 
            count = n())
short_4syll_dur_means$se <- short_4syll_dur_means$sd / sqrt(short_4syll_dur_means$count)
short_4syll_dur_means$tval <- qt(0.05/2, df = short_4syll_dur_means$count - 1)
short_4syll_dur_means$moe <- short_4syll_dur_means$tval * short_4syll_dur_means$se * -1
short_4syll_dur_means$upper <- short_4syll_dur_means$mean + short_4syll_dur_means$moe
short_4syll_dur_means$lower <- short_4syll_dur_means$mean - short_4syll_dur_means$moe



#### Setting up F0 from Reaper measures, short ####

hist(short2$F0_reaper)

ggplot(short2,aes(x=F0_reaper))+geom_histogram(bins=30)+facet_grid(.~Speaker)

## Exclude outliers past 3sd from the mean internal to each person

cutoffs <- short2 %>%
  group_by(Speaker) %>%
  summarise(upperThreshold = mean(F0_reaper, na.rm=T) + 3*sd(F0_reaper, na.rm=T),
            lowerThreshold = mean(F0_reaper, na.rm=T) - 3*sd(F0_reaper, na.rm=T))

short4 <- left_join(short2, cutoffs, by = "Speaker")

short4 <- short4 %>%
  filter(F0_reaper > lowerThreshold) %>%
  filter(F0_reaper < upperThreshold)
#  filter(Speaker != "IN") # IN F0_reaper tracking was bad
#  filter( ! (Speaker == "DK" & F0_reaper < 90)) # Get some bad ones from DK out, if we want? should do for others too though



# Now get only the words where we have full info for all syllables
short1 <- short4 %>%
  select(word_unique, word_syllables) %>%
  group_by(word_unique, word_syllables) %>%
  summarise(count = length(word_unique)) %>%
  filter(count == word_syllables)
short4 <- short4 %>%
  filter(word_unique %in% short1$word_unique) %>%
  arrange(word_unique, interval)
ggplot(short4,aes(x=F0_reaper))+geom_histogram(bins=30)+facet_grid(.~Speaker)


short_2syll_F0_reaper <- filter(short4, word_syllables == 2)
short_3syll_F0_reaper <- filter(short4, word_syllables == 3)
short_4syll_F0_reaper <- filter(short4, word_syllables == 4)



short_2syll_F0_reaper_means = short_2syll_F0_reaper %>% 
  group_by(syllable_stress, position_length, length, syllable_number, word_syllables) %>% 
  summarise(mean = mean(F0_reaper), 
            sd = sd(F0_reaper), 
            count = n())
short_2syll_F0_reaper_means$se <- short_2syll_F0_reaper_means$sd / sqrt(short_2syll_F0_reaper_means$count)
short_2syll_F0_reaper_means$tval <- qt(0.05/2, df = short_2syll_F0_reaper_means$count - 1)
short_2syll_F0_reaper_means$moe <- short_2syll_F0_reaper_means$tval * short_2syll_F0_reaper_means$se * -1
short_2syll_F0_reaper_means$upper <- short_2syll_F0_reaper_means$mean + short_2syll_F0_reaper_means$moe
short_2syll_F0_reaper_means$lower <- short_2syll_F0_reaper_means$mean - short_2syll_F0_reaper_means$moe

short_3syll_F0_reaper_means = short_3syll_F0_reaper %>% 
  group_by(syllable_stress, position_length, length, syllable_number, word_syllables) %>% 
  summarise(mean = mean(F0_reaper), 
            sd = sd(F0_reaper), 
            count = n())
short_3syll_F0_reaper_means$se <- short_3syll_F0_reaper_means$sd / sqrt(short_3syll_F0_reaper_means$count)
short_3syll_F0_reaper_means$tval <- qt(0.05/2, df = short_3syll_F0_reaper_means$count - 1)
short_3syll_F0_reaper_means$moe <- short_3syll_F0_reaper_means$tval * short_3syll_F0_reaper_means$se * -1
short_3syll_F0_reaper_means$upper <- short_3syll_F0_reaper_means$mean + short_3syll_F0_reaper_means$moe
short_3syll_F0_reaper_means$lower <- short_3syll_F0_reaper_means$mean - short_3syll_F0_reaper_means$moe

short_4syll_F0_reaper_means = short_4syll_F0_reaper %>% 
  group_by(syllable_stress, position_length, length, syllable_number, word_syllables) %>% 
  summarise(mean = mean(F0_reaper), 
            sd = sd(F0_reaper), 
            count = n())
short_4syll_F0_reaper_means$se <- short_4syll_F0_reaper_means$sd / sqrt(short_4syll_F0_reaper_means$count)
short_4syll_F0_reaper_means$tval <- qt(0.05/2, df = short_4syll_F0_reaper_means$count - 1)
short_4syll_F0_reaper_means$moe <- short_4syll_F0_reaper_means$tval * short_4syll_F0_reaper_means$se * -1
short_4syll_F0_reaper_means$upper <- short_4syll_F0_reaper_means$mean + short_4syll_F0_reaper_means$moe
short_4syll_F0_reaper_means$lower <- short_4syll_F0_reaper_means$mean - short_4syll_F0_reaper_means$moe




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
  select(word_unique, word_syllables) %>%
  group_by(word_unique, word_syllables) %>%
  summarise(count = length(word_unique)) %>%
  filter(count == word_syllables)
long2 <- only_longV %>%
  filter(word_unique %in% long1$word_unique) %>%
  arrange(word_unique, interval)

# # Making row for relative intensity
# long2$rel_intensity <- ifelse(
#   lead(long2$end, 1) == long2$word_end,                                   # If the following row is at the end of the word, then
#   lead(long2$intensity, 1) - long2$intensity,                             # get the following row's intensity minus that row's intensity, otherwise
#   ifelse(lead(long2$end, 2) == long2$word_end,                             # if the one 2 rows ahead is at the end of the word, then
#          lead(long2$intensity, 2) - long2$intensity,             # get the one 2 rows ahead minus the following row's intensity, otherwise NA
#          NA
#   )
# )
# 
# # Making row for relative f0
# long2$rel_f0 <- ifelse(
#   lead(long2$end, 1) == long2$word_end,                                   # If the following row is at the end of the word, then
#   lead(long2$f0, 1) - long2$f0,                             # get the following row's intensity minus that row's intensity, otherwise
#   ifelse(lead(long2$end, 2) == long2$word_end,                             # if the one 2 rows ahead is at the end of the word, then
#          lead(long2$f0, 2) - long2$f0,             # get the one 2 rows ahead minus the following row's intensity, otherwise NA
#          NA
#   )
# )


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
  summarise(mean = mean(F0_reaper, na.rm=T), 
            sd = sd(F0_reaper, na.rm=T), 
            count = sum(!is.na(F0_reaper)))
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







### Intensity stats and plots ####


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
  select(-rel_f0_reaper, -vowel.stress)

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
  select(-rel_f0_reaper, -vowel.stress)

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
  select(-rel_f0_reaper, -vowel.stress)

short_3syll_int_comp <- short_3syll_int %>% 
  select(-rel_f0_reaper, -vowel.stress)

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




#### F0 stats and plots ####


## 2-syllable words with just short vowels, F0_reaper
# Stats
short_2syll_F0_reaper$syllable_stress <- as.factor(short_2syll_F0_reaper$syllable_stress)
p <- lmer(F0_reaper ~ syllable_stress + (1|Speaker) + (1|word_unique) + (1|vowel),
          data = short_2syll_F0_reaper)
summary(p)
emmeans(p, specs = pairwise ~ syllable_stress)
# Plot
my_colors <- c( "#009E73")
ggplot(data = short_2syll_F0_reaper_means, aes(x = syllable_stress, y = mean, fill = length))+
  apatheme+
  labs(x = "Syllable position", y = "f0, Hz") +
  geom_violin(data = short_2syll_F0_reaper, aes(x = syllable_stress, y = F0_reaper))+
  geom_point(data = short_2syll_F0_reaper_means, aes(y = mean, x=syllable_stress), size = 3) +
  geom_errorbar(ymax= short_2syll_F0_reaper_means$upper, ymin=short_2syll_F0_reaper_means$lower, width = 0.5) +
  scale_x_discrete(labels = label_wrap(5))+
  scale_fill_manual(values=my_colors) +
  theme(legend.position = "none")

## 3-syllable words with just short vowels, F0_reaper
# Stats
short_3syll_F0_reaper$syllable_stress <- as.factor(short_3syll_F0_reaper$syllable_stress)
p <- lmer(F0_reaper ~ syllable_stress + (1|Speaker) + (1|word_unique) + (1|vowel),
          data = short_3syll_F0_reaper)
summary(p)
emmeans(p, specs = pairwise ~ syllable_stress)
# Plot
my_colors <- c( "#009E73")
ggplot(data = short_3syll_F0_reaper_means, aes(x = syllable_stress, y = mean, fill = length))+
  apatheme+
  labs(x = "Syllable position", y = "f0, Hz") +
  geom_violin(data = short_3syll_F0_reaper, aes(x = syllable_stress, y = F0_reaper))+
  geom_point(data = short_3syll_F0_reaper_means, aes(y = mean, x=syllable_stress), size = 3) +
  geom_errorbar(ymax= short_3syll_F0_reaper_means$upper, ymin=short_3syll_F0_reaper_means$lower, width = 0.5) +
  scale_x_discrete(labels = label_wrap(5)) +
  scale_fill_manual(values=my_colors) +
  theme(legend.position = "none")


## 4-syllable words with just short vowels, F0_reaper
# Stats
short_4syll_F0_reaper$syllable_stress <- as.factor(short_4syll_F0_reaper$syllable_stress)
p <- lmer(F0_reaper ~ syllable_stress + (1|Speaker) + (1|word_unique) + (1|vowel),
          data = short_4syll_F0_reaper)
summary(p)
emmeans(p, specs = pairwise ~ syllable_stress)
# Plot
my_colors <- c( "#009E73")
ggplot(data = short_4syll_F0_reaper_means, aes(x = syllable_stress, y = mean, fill = length))+
  apatheme+
  labs(x = "Syllable position", y = "f0, Hz") +
  geom_violin(data = short_4syll_F0_reaper, aes(x = syllable_stress, y = F0_reaper)) +
  geom_point(data = short_4syll_F0_reaper_means, aes(y = mean, x=syllable_stress), size = 3) +
  geom_errorbar(ymax= short_4syll_F0_reaper_means$upper, ymin=short_4syll_F0_reaper_means$lower, width = 0.8) +
  scale_x_discrete(labels = label_wrap(5)) +
  scale_fill_manual(values=my_colors) +
  theme(legend.position = "none")




## 2-syllable words with just long vowels, f0 (Reaper)
long2$syllable_stress <- as.factor(long2$syllable_stress)
p <- lmer(F0_reaper ~ syllable_stress + (1|Speaker) + (1|word_unique)  + (1|vowel),
          data = long2)
summary(p)
emmeans(p, specs = pairwise ~ syllable_stress)

# Plot
my_colors <- c("#56B4E9")
ggplot(data = long_2syll_f0_means, aes(x = syllable_stress, y = mean, fill = length))+
  apatheme+
  labs(x = "Syllable position", y = "f0, Hz") +
  geom_violin(data = long2, aes(x = syllable_stress, y = F0_reaper))+
  geom_point(data = long_2syll_f0_means, aes(y = mean, x=syllable_stress), size = 3) +
  geom_errorbar(ymax= long_2syll_f0_means$upper, ymin=long_2syll_f0_means$lower, width = 0.5) +
  scale_x_discrete(labels = label_wrap(5)) +
  scale_fill_manual(values=my_colors) +
  theme(legend.position = "none") +
  scale_x_discrete(limits = rev)
  


## 2-syll short vs. 2-syll long
short_2syll_f0_comp <- short_2syll_F0_reaper %>% 
  select(-rel_f0_reaper, -vowel.stress, -upperThreshold, -lowerThreshold)

long_short_f0 <- rbind(short_2syll_f0_comp, long2)

short_2syll_F0_reaper_means$length <- "short"
long_2syll_f0_means$length <- "long"

long_short_f0_means <- rbind(short_2syll_F0_reaper_means, long_2syll_f0_means)

long_short_f0_means$position_length = factor(long_short_f0_means$position_length, levels= c("-2.long", "-2.short","-1.long","-1.short"))
long_short_f0$position_length = factor(long_short_f0$position_length, levels= c("-2.long", "-2.short","-1.long","-1.short"))

# Plot
my_colors <- c("#56B4E9", "#009E73")
ggplot(data = long_short_f0_means, aes(x = position_length, y = mean, fill = length))+
  apatheme+
  labs(x = "Syllable position", y = "f0, Hz") +
  geom_violin(data = long_short_f0, aes(x = position_length, y = F0_reaper, fill = length))+
  geom_point(data = long_short_f0_means, aes(y = mean, x=position_length, fill = length), size = 3) +
  geom_errorbar(ymax= long_short_f0_means$upper, ymin=long_short_f0_means$lower, width = 0.5) +
  scale_x_discrete(labels = label_wrap(5)) +
  scale_fill_manual(values=my_colors) +
  theme(legend.position = "right")

# Stats
p <- lmerTest::lmer(F0_reaper ~ length * syllable_number + (1|Speaker) + (1|word_unique)  + (1|vowel),
                data = long_short_f0)
summary(p)
emmeans(p, specs = pairwise ~ length)
emmeans(p, specs = pairwise ~ syllable_number)
emmeans(p, specs = pairwise ~ length * syllable_number)



## 4-syll short vs. 2-syll long
short_4syll_f0_comp <- short_4syll_F0_reaper %>% 
  select(-rel_f0_reaper, -vowel.stress, -upperThreshold, -lowerThreshold)

long2_short4_f0 <- rbind(short_4syll_f0_comp, long2)

short_4syll_F0_reaper_means$length <- "short"
long_2syll_f0_means$length <- "long"

long2_short4_means_f0 <- rbind(short_4syll_F0_reaper_means, long_2syll_f0_means)

long2_short4_means_f0$position_length = factor(long2_short4_means_f0$position_length, levels= c("-4.short","-3.short", "-2.long", "-2.short","-1.long","-1.short"))
long2_short4_f0$position_length = factor(long2_short4_f0$position_length, levels= c("-4.short","-3.short","-2.long", "-2.short","-1.long","-1.short"))

# Plot
my_colors <- c("#56B4E9", "#009E73")
ggplot(data = long2_short4_means_f0, aes(x = position_length, y = mean, fill = length))+
  apatheme+
  labs(x = "Syllable position", y = "F0, Hz") +
  geom_violin(data = long2_short4_f0, aes(x = position_length, y = F0_reaper, fill = length))+
  geom_point(data = long2_short4_means_f0, aes(y = mean, x=position_length, fill = length), size = 3) +
  geom_errorbar(ymax= long2_short4_means_f0$upper, ymin=long2_short4_means_f0$lower, width = 0.5) +
  scale_x_discrete(labels = label_wrap(5))+
  scale_fill_manual(values=my_colors) +
  theme(legend.position = "right")

# Stats

p <- lmer(F0_reaper ~ position_length + (1|Speaker) + (1|word_unique)  + (1|vowel),
          data = long2_short4_f0)
summary(p)
emmeans(p, specs = pairwise ~ position_length)



## 3-syll short vs. 4-syll short

short_4syll_f0_comp <- short_4syll_F0_reaper %>% 
  select(-rel_f0_reaper, -vowel.stress)

short_3syll_f0_comp <- short_3syll_F0_reaper %>% 
  select(-rel_f0_reaper, -vowel.stress)

short3_short4_f0 <- rbind(short_4syll_f0_comp, short_3syll_f0_comp)
short3_short4_f0$syll_comp <- paste(short3_short4_f0$syllable_number, short3_short4_f0$word_syllables, sep = "_")
short3_short4_f0$word_syllables <- as.factor(short3_short4_f0$word_syllables)

short3_short4_means_f0 <- rbind(short_4syll_F0_reaper_means, short_3syll_F0_reaper_means)
short3_short4_means_f0$syll_comp <- paste(short3_short4_means_f0$syllable_number, short3_short4_means_f0$word_syllables, sep = "_")
short3_short4_means_f0$word_syllables <- as.factor(short3_short4_means_f0$word_syllables)

# Plot
my_colors <- c("#a1d99b", "#009E73")
ggplot(data = short3_short4_means_f0, aes(x = syll_comp, y = mean, fill = word_syllables))+
  apatheme+
  labs(x = "Syllable position", y = "F0, Hz") +
  geom_violin(data = short3_short4_f0, aes(x = syll_comp, y = F0_reaper, fill = word_syllables))+
  geom_point(data = short3_short4_means_f0, aes(y = mean, x=syll_comp, fill = word_syllables), size = 3) +
  geom_errorbar(ymax= short3_short4_means_f0$upper, ymin=short3_short4_means_f0$lower, width = 0.5) +
  scale_x_discrete(limits = rev)+
  scale_fill_manual(values=my_colors) +
  theme(legend.position = "right")


# Stats

p <- lmer(F0_reaper ~ syll_comp + (1|Speaker) + (1|word_unique)  + (1|vowel),
          data = short3_short4_f0)
summary(p)
emmeans(p, specs = pairwise ~ syll_comp)





#### Duration stats and plots ####

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
  select(-rel_f0_reaper, -vowel.stress)

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
    select(-rel_f0_reaper, -vowel.stress)

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
  select(-rel_f0_reaper, -vowel.stress)

short_3syll_dur_comp <- short_3syll_dur %>% 
  select(-rel_f0_reaper, -vowel.stress)

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

library(ggdensity)

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


# short4 filtered out outlier F0_reapers

primary_f0 <- short4 %>% 
  filter(word_syllables == 2 | word_syllables == 3 | word_syllables == 4) %>%
  filter(stress == "primary")


primary_f0$word_syllables <- as.factor(primary_f0$word_syllables)

yarrr::pirateplot(data = primary_f0,
                  formula = F0_reaper ~ word_syllables)

p <- lmer(F0_reaper ~ word_syllables + (1|Speaker) + (1|vowel),
          data = primary_f0)
summary(p)
emmeans(p, specs = pairwise ~ word_syllables)










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





### Attempt at BRMS modeling of the F1/F2 and stress type ###


library(emmeans)
library(brms)
library(bmmb)
library(bayestestR)
library(parameters)
library(see)
library(effects)
library(ggeffects)

short_a <- short2_a %>%
  rename(f1z = f1_normed_inf,
         f2z = f2_normed_inf)

model_formula <- bf( mvbind(f1z, f2z) ~ syllable_stress + (1|p|Speaker), # |p| indicates that f1 and f2 are correlated within speaker
                     sigma ~ syllable_stress)

model <- brm(model_formula, data=short_a, 
             chains=4, cores=4, warmup=1000, iter=2500, thin=2, family="student")
summary(model)
fixef (model)
emmeans(model, specs = pairwise ~ syllable_stress)
hello <- conditional_effects(model, "syllable_stress", resp = "f1z")
conditional_effects(model, "syllable_stress", resp = "f2z")

# plot(hello$f1normedinf.f1normedinf_syllable_stress$Speaker)

# ggemmeans(model, terms = "syllable_stress")
ggpredict(model, terms = "syllable_stress")

theme_set(theme_modern())
plot(parameters(model))
result <- estimate_density(model)
plot(result, stack = F)


prior_summary(model)
get_prior(model)


short_hypothesis (
  model, 
  c("f1z_syllable_stressPrimaryStressed = f1z_stressFinalUnstressed",                 # F1 Primary Stressed vs. F1 Final Unstressed
    "sigma_f1z_syllable_stressPrimaryStressed = sigma_f1z_syllable_stressFinalUnstressed",     # F1 spread Primary Stressed vs. F1 spread Final Unstressed
    "f2z_syllable_stressPrimaryStressed = f2z_syllable_stressFinalUnstressed",                 # F2 Primary Stressed vs. F2 Final Unstressed
    "sigma_f2z_syllable_stressPrimaryStressed = sigma_f2z_syllable_stressFinalUnstressed",     # F2 spread Primary Stressed vs. F2 spread Final Unstressed
    
    "f1z_syllable_stressPrimaryStressed = f1z_syllable_stressAntepen.Unstressed",                 # F1 Primary Stressed vs. F1 Antepen. Unstressed
    "sigma_f1z_syllable_stressPrimaryStressed = sigma_f1z_syllable_stressAntepen.Unstressed",     # F1 spread Primary Stressed vs. F1 spread Antepen. Unstressed
    "f2z_syllable_stressPrimaryStressed = f2z_syllable_stressAntepen.Unstressed",                 # F2 Primary Stressed vs. F2 Antepen. Unstressed
    "sigma_f2z_syllable_stressPrimaryStressed = sigma_f2z_syllable_stressAntepen.Unstressed",     # F2 spread Primary Stressed vs. F2 spread Antepen. Unstressed
    
    "f1z_syllable_stressPrimaryStressed = f1z_syllable_stressSecondaryStressed",                 # F1 Primary Stressed vs. F1 Secondary Stressed
    "sigma_f1z_syllable_stressPrimaryStressed = sigma_f1z_syllable_stressSecondaryStressed",     # F1 spread Primary Stressed vs. F1 spread Secondary Stressed
    "f2z_syllable_stressPrimaryStressed = f2z_syllable_stressSecondaryStressed",                 # F2 Primary Stressed vs. F2 Secondary Stressed
    "sigma_f2z_syllable_stressPrimaryStressed = sigma_f2z_syllable_stressSecondaryStressed",     # F2 spread Primary Stressed vs. F2 spread Secondary Stressed
    
    "f1z_syllable_stressFinalUnstressed = f1z_syllable_stressAntepen.Unstressed",                 # F1 Final Unstressed vs. F1 Antepen. Unstressed
    "sigma_f1z_syllable_stressFinalUnstressed = sigma_f1z_syllable_stressAntepen.Unstressed",     # F1 spread Final Unstressed vs. F1 spread Antepen. Unstressed
    "f2z_syllable_stressFinalUnstressed = f2z_syllable_stressAntepen.Unstressed",                 # F2 Final Unstressed vs. F2 Antepen. Unstressed
    "sigma_f2z_syllable_stressFinalUnstressed = sigma_f2z_syllable_stressAntepen.Unstressed",     # F2 spread Final Unstressed vs. F2 spread Antepen. Unstressed
    
    "f1z_syllable_stressSecondaryStressed = f1z_syllable_stressAntepen.Unstressed",                 # F1 Secondary Stressed vs. F1 Antepen. Unstressed
    "sigma_f1z_syllable_stressSecondaryStressed = sigma_f1z_syllable_stressAntepen.Unstressed",     # F1 spread Secondary Stressed vs. F1 spread Antepen. Unstressed
    "f2z_syllable_stressSecondaryStressed = f2z_syllable_stressAntepen.Unstressed",                 # F2 Secondary Stressed vs. F2 Antepen. Unstressed
    "sigma_f2z_syllable_stressSecondaryStressed = sigma_f2z_syllable_stressAntepen.Unstressed",     # F2 spread Secondary Stressed vs. F2 spread Antepen. Unstressed
    
    "f1z_syllable_stressSecondaryStressed = f1z_syllable_stressFinalUnstressed",                 # F1 Secondary Stressed vs. F1 Final Unstressed
    "sigma_f1z_syllable_stressSecondaryStressed = sigma_f1z_syllable_stressFinalUnstressed",     # F1 spread Secondary Stressed vs. F1 spread Final Unstressed
    "f2z_syllable_stressSecondaryStressed = f2z_syllable_stressFinalUnstressed",                 # F2 Secondary Stressed vs. F2 Final Unstressed
    "sigma_f2z_syllable_stressSecondaryStressed = sigma_f2z_syllable_stressFinalUnstressed"     # F2 spread Secondary Stressed vs. F2 spread Final Unstressed
  ))




# # Making row for relative intensity
# short2$rel_intensity <- ifelse(
#   short2$end == short2$word_end,                                   # If it's at the end of the word, then
#   lag(short2$intensity, 1) - short2$intensity,                     # get the previous row's intensity minus that row's intensity, otherwise
#   ifelse(short2$stress == "unstressed",                            # if it's unstressed,
#          lead(short2$intensity, 1) - short2$intensity,             # get the following row's intensity minus that row's intensity, otherwise
#          ifelse(short2$stress == "secondary",                      # if it's secondary,
#                 lead(short2$intensity, 2) - short2$intensity,      # get the one 2 rows ahead minus that row's intensity, otherwise NA
#                 NA
#          )     
#   )
# )
# 
# # Making row for relative f0 (Praat)
# 
# short2$rel_f0 <- ifelse(
#   short2$end == short2$word_end,                                   # If it's at the end of the word, then
#   lag(short2$f0, 1) - short2$f0,                                   # get the previous row's f0 minus that row's f0, otherwise
#   ifelse(short2$stress == "unstressed",                            # if it's unstressed,
#          lead(short2$f0, 1) - short2$f0,                           # get the following row's f0 minus that row's f0, otherwise
#          ifelse(short2$stress == "secondary",                      # if it's secondary,
#                 lead(short2$f0, 2) - short2$f0,                    # get the one 2 rows ahead minus that row's f0, otherwise NA
#                 NA
#          )
#   )
# )
# 
# 
# # Making row for relative f0 (Reaper mean)
# short2$rel_f0_reaper <- ifelse(
#   short2$end == short2$word_end,                                   # If it's at the end of the word, then
#   lag(short2$F0_reaper, 1) - short2$F0_reaper,                                   # get the previous row's f0 minus that row's f0, otherwise
#   ifelse(short2$stress == "unstressed",                            # if it's unstressed,
#          lead(short2$F0_reaper, 1) - short2$F0_reaper,                           # get the following row's f0 minus that row's f0, otherwise
#          ifelse(short2$stress == "secondary",                      # if it's secondary,
#                 lead(short2$F0_reaper, 2) - short2$F0_reaper,                    # get the one 2 rows ahead minus that row's f0, otherwise NA
#                 NA
#          )
#   )
# )
