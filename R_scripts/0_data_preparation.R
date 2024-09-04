## This script prepares the data, using input that has been put through a Praat script to
## merge diphthongs on the phone tier before being put through FastTrack and then
## aggregated into 9 bins.
library(tidyverse)
library(reshape2)

## Gonna give myself two types of negated %in% here
`%!in%` = Negate(`%in%`)
`%notin%` <- Negate(`%in%`)

setwd("/Users/Thomas/Documents/Hawaiian_Phonetics/KLHData/R_scripts/")

## Has Fast Track aggregation function
source("aggregatedata_arjun.R")
source("aggregatedata_original.R")

# Then load in the data

source("load_speaker/AA_data.R")
source("load_speaker/DK_data.R")
source("load_speaker/HM_data.R")
source("load_speaker/IN_data.R")
source("load_speaker/JM_data.R")
source("load_speaker/LV_data.R")
source("load_speaker/RM_data.R")
source("load_speaker/SB_data.R")

# Bind them all together
data <- rbind(AA,DK,HM,IN,JM,LV,RM,SB) %>% select(-comment2)

##### Add in frequency info ######

freq_catherine <- read.csv("/Users/Thomas/Documents/Hawaiian_Phonetics/Dissertation/frequencies_catherine.csv", header=TRUE, stringsAsFactors=FALSE)
names(freq_catherine)[1] <- "rank"
names(freq_catherine)[2] <- "word"
data <- left_join(data, freq_catherine, by="word")

###### Label speakers #######

data <- data %>% 
  mutate(Speaker = case_when( str_detect(filename, "KLH016") ~ 'JM', 
                                     str_detect(filename, "KLH032") ~ 'LV', 
                                     str_detect(filename, "KLH013") ~ 'IN', 
                                     str_detect(filename, "KLH021") ~ 'HM', 
                                     str_detect(filename, "KLH063") ~ 'DK', 
                                     str_detect(filename, "KLH033") ~ 'SB', 
                                     str_detect(filename, "KLH014") ~ 'RM', 
                                     str_detect(filename, "KLH057") ~ 'AA', TRUE ~ '0'))

# data$Speaker <- ifelse(grepl("KLH016", data$filename),"JM",
#                ifelse(grepl("KLH032", data$filename),"LV",
#                ifelse(grepl("KLH013", data$filename),"IN",
#                ifelse(grepl("KLH021", data$filename),"HM",
#                ifelse(grepl("KLH063", data$filename),"DK",
#                ifelse(grepl("KLH033", data$filename),"SB",
#                ifelse(grepl("KLH014", data$filename),"RM",
#                ifelse(grepl("KLH057", data$filename),"AA",
#                            0))))))))

##### Get a token label in original order and correct some spellings #######

data <- data %>% mutate(original_order = 1:length(filename),
                        across(.cols = c(word, next_word, previous_word),.fns = ~recode(.x,
                                                                                        aikane = "aikāne", ### these first ones all need their vowel to be changed, too
                                                                                        kaikunane = "kaikunāne",
                                                                                        ahea = "āhea",
                                                                                        ʻanela = "ʻānela",
                                                                                        ʻapana = "ʻāpana",
                                                                                        kapena = "kāpena",
                                                                                        mahele = "māhele",
                                                                                        nahelehele = "nāhelehele",
                                                                                        punawai = "pūnāwai",
                                                                                        ʻaue = "auē",
                                                                                        keikamahine = "kaikamahine",
                                                                                        nāhae = "nahae",
                                                                                        nāʻauao = "naʻauao",
                                                                                        nupepa = "nūpepa",
                                                                                        pukiki = "pukikī",
                                                                                        loʻa = "loaʻa", ### these other ones don't need vowel to be changed, just the word
                                                                                        lokoo = "loko",
                                                                                        puʻa = "puaʻa",
                                                                                        ʻauē = "auē",
                                                                                        iʻini = "ʻiʻini",
                                                                                        apuʻuwaʻawaʻa = "puʻuwaʻawaʻa",
                                                                                        ʻaʻale = "ʻaʻole",
                                                                                        ʻaʻahe = "ʻaʻohe"
                                                                                        )))

## change vowels that need to be changed

data <- data %>%
  mutate(
    vowel = case_when(
      vowel == "a" & word_end != end & word %in% c("aikāne", "āhea", "ʻānela", "kāpena", "māhele", "pūnāwai", "nāhelehele") ~ "ā",
      vowel == "a" & word_end != end & word == "ʻāpana" ~ "ā",
      vowel == "u" & word %in% c("pūnāwai", "nūpepa") ~ "ū",
      vowel == "e" & word == "auē" ~ "ē",
      vowel == "i" & word == "pukikī" ~ "ī",
      vowel == "ei" & word == "kaikamahine" ~ "ai",
      vowel == "ā" & word %in% c("nahae", "naʻauao") ~ "a",
      TRUE ~ vowel
    )
  )

## omit these because too difficult to change now, but should go back into original transcripts and change
data$omit <-ifelse(data$word == "makuhine" |
                    data$word == "ʻoʻlea"|
                     data$word == "ʻolonā"|
                     data$word == "ʻūhai"|
                     data$word == "hoʻōnaona"|
                     data$word == "ʻunuhi"|
                     data$word == "keikikāne"|
                     data$word == "kupunakāne"|
                     data$word == "kupunawahine"|
                     data$word == "kūmāmākolu"|
                     data$word == "hapanui"|
                    data$word == "wa", 1, data$omit)


######## Getting overall number of tokens in each vowel class #########

## Gets number of tokens per vowel
vowel_token_freq <- as.data.frame(table(data$vowel))

# starwars$hair_color %>% janitor::tabyl() %>% janitor::adorn_pct_formatting()

# vowel_token_freq <- vowel_token_freq %>% rename(vowel = Var1)

# speaker_token_freq <- as.data.frame(table(data$Speaker)) %>%
#  rename(speaker = Var1) %>%
#  filter(speaker != "0")
  
 # ggplot(vowel_token_freq, aes(x=reorder(vowel,-Freq), y=Freq)) +
 #  geom_bar(stat = "identity")+
 #  ggtitle("Token frequency by vowel, entire dataset before exclusions") +
 #  theme_classic() + 
 #  xlab("Vowel") + ylab("Frequency")+
 #  geom_text(aes(label=Freq), position=position_dodge(width=0.9), vjust=-0.25)
 # ggsave("/Users/Thomas/Documents/Hawaiian_Phonetics/Dissertation/Manaleo/normed/R_outputs/overall_tokens_vowel.png", height = 5, width = 10, units = "in")
 # 
 # 
 # ggplot(speaker_token_freq, aes(x=reorder(speaker,-Freq), y=Freq)) +
 #   geom_bar(stat = "identity")+
 #   ggtitle("Token frequency by speaker, entire dataset before exclusions") +
 #   theme_classic() + 
 #   xlab("Speaker") + ylab("Frequency")+
 #   geom_text(aes(label=Freq), position=position_dodge(width=0.9), vjust=-0.25)
 # ggsave("/Users/Thomas/Documents/Hawaiian_Phonetics/Dissertation/Manaleo/normed/R_outputs/overall_tokens_speaker.png", height = 5, width = 10, units = "in")
 # 

# t <- speaker_token_freq
# t
 

 
# ggplot(vowel_token_freq, aes(x=reorder(vowel,-Freq), y=Freq)) +
#   geom_bar(stat = "identity")+
#   ggtitle("Token frequency by vowel") +
#   xlab("Vowel") + ylab("Frequency")+
#   geom_col(aes(fill=Freq)) +
#   scale_fill_gradient(low = "white", high = "blue") +
#   theme(legend.title=element_blank(), plot.title = element_text(hjust = 0.5)) +
#   geom_text(aes(label=Freq), position=position_dodge(width=0.9), vjust=-0.25)


####### Make lists of vowels/mono/diph #######

list_of_monophthongs <- c("ā","ē","ī","ō","ū","a","e","i","o","u")
list_of_diphthongs <- c("ai","au","ao","ae","ei", "ou","eu","oi","iu","āi","āe","āu","āo","ēi","ōu")
list_of_vowels <- c("ā","ē","ī","ō","ū","a","e","i","o","u","ai","au","ao","ae","ei", "ou","eu","oi","iu","āi","āe","āu","āo","ēi","ōu")
list_of_long_mono <- c("ā","ē","ī","ō","ū")
list_of_short_mono <- c("a","e","i","o","u")

##### Make column for number of moras ######
######## Make column for diphthong status #######
######## Make all silences same #######
data <- data %>%
  mutate(
    Moras = case_when(
      vowel %in% c('a', 'e', 'i', 'o', 'u') ~ 1,
      vowel %in% c('ā', 'ē', 'ī', 'ō', 'ū') ~ 2,
      vowel %in% c('ai', 'ae', 'ao', 'au', 'oi', 'ou', 'ei', 'eu', 'iu') ~ 2,
      vowel %in% c('āi', 'āē', 'āu', 'āō', 'ōu', 'ēi') ~ 3,
      TRUE ~ NA_integer_ # Add default condition if needed
    ),
    Syllabification = ifelse(vowel %in% list_of_monophthongs, "Mono", "Diph"),
    previous_sound = recode(previous_sound,
                            `-` = "sil",
                            sp = "sil",
                            sil = "sil"),
    next_sound = recode(next_sound,
                        `-` = "sil",
                        sp = "sil",
                        sil = "sil")
  )

###### DEALING WITH WORDS TO EXCLUDE ########

####### Make "aole type" coluumn for variants of ʻaʻole (except ʻole), ʻaʻohe (except ʻohe), and hope/hape #######

data <- data %>%
  mutate(
    aole = ifelse(word %in% c("ʻaʻole", "ʻale", "ʻaʻale", "ʻaʻohe", "ʻaʻahe", "ʻahe", "ʻohe", "hope", "hape"), 1, 0),
    kaika = ifelse(str_detect(word, "^kaika") | str_detect(word, "^kaiku"), 1, 0),
    maikai = ifelse(str_detect(word, "maikaʻi") | str_detect(word, "maitaʻi"), 1, 0),
    Kinney1956 = ifelse(word %in% c("laila", "ikaika") | maikai == 1 | kaika == 1, 1, 0),
    articles = ifelse(word %in% c("ka", "ke", "kekahi", "kahi", "ta", "te", "nā"), 1, 0),
    demons = ifelse(word %in% c("kēia", "kēlā", "kēnā", "tēia", "tēlā", "tēnā", "pēia", "pēlā", "pēnā"), 1, 0),
    particles = ifelse(word %in% c("nō", "paha", "hoʻi", "wale", "pēnei"), 1, 0),
    directionals = ifelse(word %in% c("mai", "aku", "aʻe", "iho", "ala", "maila", "akula", "aʻela", "ihola", "nei"), 1, 0),
    pronouns = ifelse(word %in% c("au", "wau", "ʻoe", "ia", "māua", "mākou", "kāua", "kākou", "ʻolua", "ʻoukou", "lāua", "lākou", 
                                  "koʻu", "kou", "kāu", "kaʻu", "kuʻu", "kona", "kāna", "aʻu", "āu", "āna", "oʻu", "ou", "ona", 
                                  "naʻu", "nāu", "nāna", "noʻu", "nou", "nona", "iaʻu"), 1, 0),
    interrogatives = ifelse(word %in% c("aha", "hea", "pehea"), 1, 0),
    funct = ifelse(word %in% c("a", "o", "ā", "me", "i", "nō", "ʻo", "e", "ma", "he", "inā", "ua", "ʻia", "no", "ʻana", "ana", "ai",
                               "mau", "aia", "eia", "no", "na", "ko", "kā", "iā"), 1, 0),
    manawa = ifelse(word == "manawa", 1, 0),
    mea = ifelse(word == "mea", 1, 0),
    ae = ifelse(word %in% c("ʻae", "ʻē", "ʻeā"), 1, 0)
  )

####### Make column for stress #######

data <- data %>%
  mutate(
    stress = case_when(
      Moras > 1 & end == word_end ~ "primary",
      Moras > 1 & word_end == lead(end, 1) & lead(Moras, 1) == 1 ~ "primary",
      Moras > 1 ~ "secondary",
      Moras == 1 & end == word_end ~ "unstressed",
      Moras == 1 & word_end == lead(end, 1) & lead(Moras, 1) == 1 ~ "primary",
      Moras == 1 & word_end == lead(end, 2) & lead(Moras, 2) > 1 & lead(Moras, 1) == 1 ~ "secondary",
      Moras == 1 & word_end == lead(end, 3) & word_end != lag(word_end, 1) & lead(Moras, 1) == 1 ~ "secondary",
      TRUE ~ "unstressed"
    )
  )

####### Make column for number of syllables from right edge #######

data <- data %>%
  mutate(
    syllable_number = case_when(
      end == word_end ~ -1,
      word_end == lead(end, 1) ~ -2,
      word_end == lead(end, 2) ~ -3,
      word_end == lead(end, 3) ~ -4,
      word_end == lead(end, 4) ~ -5,
      word_end == lead(end, 5) ~ -6,
      TRUE ~ NA_integer_
    )
  )


check <- dplyr::select(data, c("filename","start","vowel","word","Moras","Syllabification","stress","syllable_number"))


####### Make column for following vowel #######

data$following_vowel <- ifelse(data$next_sound == "sil","sil", lead(data$vowel))
data$previous_vowel <- ifelse(data$previous_sound == "sil","sil", lag(data$vowel))


######## Rename formant columns to make splitting easier ########

data <- data %>% rename_at(vars(starts_with("f1")), funs(str_replace(., "f1", "f1.")))
data <- data %>% rename_at(vars(starts_with("f2")), funs(str_replace(., "f2", "f2.")))
data <- data %>% rename_at(vars(starts_with("f3")), funs(str_replace(., "f3", "f3.")))


## Arjun: fix with regex

# data <- data %>% rename_with(.cols = f11:f39, .fn = ~paste0(.x, '.'))

