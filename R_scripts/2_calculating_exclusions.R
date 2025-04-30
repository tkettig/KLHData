setwd("/Users/Thomas/Documents/Hawaiian_Phonetics/KLHData/R_scripts")
source("0_data_preparation.R")
source("1_reselect_winners.R")
library(joeyr)

## Assigning a list of problematic adjacent consonants
problematic_consonants <- c("ʻ","w","h")

## Getting a column unique to each word
data$word_unique <- paste(data$word, data$word_start, sep="")

## Get a column for number of syllables in a particular word
data2 <- data %>%
  select(word_unique) %>%
  group_by(word_unique) %>%
  summarise(word_syllables = length(word_unique))

data <- left_join(data, data2, by="word_unique")

######## EXCLUSIONS AND FILTERING ############


## Adding to the omit column any of the tokens that I've marked to omit while investigating outliers
## (Note: This can be run even if the outlier investigation has not been done yet for a particular speaker)

data$omit <- ifelse(data$filename %in% list_to_omit_LV | 
                    data$filename %in% list_to_omit_IN |
                    data$filename %in% list_to_omit_HM |
                    data$filename %in% list_to_omit_JM |
                    data$filename %in% list_to_omit_SB |
                    data$filename %in% list_to_omit_DK |
                    data$filename %in% list_to_omit_RM |
                    data$filename %in% list_to_omit_AA , 1, data$omit)

## Taking out tokens that should be omitted

filtered <- data %>% 
  filter(omit == 0)

## Recoding the ones that could be o/a as oa

filtered$vowel <- ifelse(filtered$aole==1, 
                         ifelse(filtered$stress=="primary","oa", filtered$vowel), filtered$vowel)

### Taking out just articles, particles, function words, demonstratives, interrogatives, mea, manawa
### Leaving in pronouns, ʻae, directionals because I may want to include those in later calculations to beef up 
### some vowel categories, and would be better to maybe exclude outliers at this point?

filtered <- filtered %>% filter(articles == 0,
                                particles == 0,
                                funct == 0,
                                demons == 0,
                                interrogatives == 0,
                                mea == 0,
                                manawa == 0)


######### SPREADING DATA INTO TALL FORMAT #########

## Get a spread dataset

datagather <- filtered %>% gather(Formant.Time, Hertz, f1.1:f3.9)
datagather <- data.frame(datagather, reshape2::colsplit(datagather$Formant.Time, pattern="\\.", names = c("formant", "time")))
datagather <- datagather[,names(datagather) != "Formant.Time"]
dataspread <- datagather %>% spread(formant, Hertz)

## Adding a column for mahalanobis distance

dataspread <- dataspread %>% 
  group_by(Speaker) %>% 
  group_by(vowel) %>% 
  mutate(mahal_dist = tidy_mahalanobis(f1, f2))

## Going to rearrange data by mahal distance for each speaker

speakers <- c("LV", "HM", "IN", "JM", "SB", "DK", "RM", "AA")

arranged_data <- speakers %>% 
  map(~ dataspread %>% 
        filter(Speaker == .x) %>% 
        arrange(mahal_dist)) %>% 
  purrr::set_names(speakers)

## Find the 95% and 99% points in them in order to just explore worst 5% of tokens and throw out worst 1% of measurements


mahal95 <- arranged_data %>% 
  map(~ .[round(nrow(.) * 0.95), ncol(.)])

mahal99 <- arranged_data %>% 
  map(~ .[round(nrow(.) * 0.99), ncol(.)])


## Let's group by token first and then figure out the tokens that have the worst mahal means and medians
## of all 9 of their readings. Then write that to a .csv and manually take a look at all of the worst 5%.

mahal_means <- map2(
  arranged_data,
  mahal95,
  ~ .x %>%
    group_by(filename, vowel, word, start, stress) %>%
    summarise(mahal_mean = mean(mahal_dist), mahal_median = median(mahal_dist)) %>%
    arrange(desc(mahal_mean)) %>%
    filter(mahal_mean > .y)
)

# write.csv(mahal_meansAA, "/Users/Thomas/Documents/Hawaiian_Phonetics/Dissertation/Manaleo/AA/output/mahal_means.csv")


#### Filtering out worst 1% of measurements ####

LV <- arranged_data$LV %>% filter(mahal_dist < mahal99$LV$mahal_dist)
HM <- arranged_data$HM %>% filter(mahal_dist < mahal99$HM$mahal_dist)
IN <- arranged_data$IN %>% filter(mahal_dist < mahal99$IN$mahal_dist)
SB <- arranged_data$SB %>% filter(mahal_dist < mahal99$SB$mahal_dist)
DK <- arranged_data$DK %>% filter(mahal_dist < mahal99$DK$mahal_dist)
RM <- arranged_data$RM %>% filter(mahal_dist < mahal99$RM$mahal_dist)
AA <- arranged_data$AA %>% filter(mahal_dist < mahal99$AA$mahal_dist)
JM <- arranged_data$JM %>% filter(mahal_dist < mahal99$JM$mahal_dist)

## Merge together all the nice cleaned individual speaker datasets

data <- rbind(AA,DK,HM,IN,JM,LV,RM,SB)

## Set wd again

setwd("/Users/Thomas/Documents/Hawaiian_Phonetics/KLHData/R_scripts/")

### Inspect durations to mark extra-long ones to possibly exclude #####

duration_inspection <- data %>%
  filter(duration > .3) %>%
  filter(vowel %in% list_of_monophthongs) %>%
  filter(word_syllables > 1) %>%
  select(Speaker, filename, start, vowel, word, duration, comment1) %>%
  unique() %>%
  arrange(filename)

write.csv(duration_inspection, "duration_inspection.csv")

### Inspect for kēlā and kēnā type words when demonstratives have been left in the dataset

long_e_inspection <- data %>%
  select(Speaker, filename, start, vowel, word, duration, demons, comment1) %>%
  filter(vowel %in% c("a","ā","e","ē")) %>%
  unique()

long_e_inspection$demons <- as.factor(long_e_inspection$demons)
  
ggplot(data = long_e_inspection, aes(x = vowel, y = duration, color = demons))+
  apatheme+
  geom_boxplot(position = position_dodge(0.5)) +
  ylim(.03,.3)


