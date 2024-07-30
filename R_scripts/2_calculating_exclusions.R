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

### Gets number of tokens per vowel

# vowel_token_freq <- as.data.frame(table(filtered$vowel))
# vowel_token_freq <- vowel_token_freq %>% rename(vowel = Var1)
# 
# ggplot(vowel_token_freq, aes(x=reorder(vowel,-Freq), y=Freq)) +
#   geom_bar(stat = "identity")+
#   ggtitle("Token frequency by vowel") +
#   xlab("Vowel") + ylab("Frequency")+
#   geom_text(aes(label=Freq), position=position_dodge(width=0.9), vjust=-0.25)

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

arranged_data <- speakers %>% map(~ dataspread %>% filter(Speaker == .x) %>% arrange(mahal_dist)) %>% purrr::set_names(speakers)

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

setwd("/Users/Thomas/Documents/Hawaiian_Phonetics/Dissertation/R_scripts/")



####### EXPLORING OUTLIERS GRAPHICALLY #######

## aggregate all 9 measurements together to get a mean
# 
# means <- dataspread %>% 
#   filter(Speaker=="LV") %>%
#   group_by(filename,vowel,original_order,word,Syllabification,Moras) %>%
#   summarise(meanF1 = mean(f1),
#             meanF2 = mean(f2),
#             mahal_dist = mean(mahal_dist))
# 
# library(phonR)
# 
# mono <- means %>% filter(Syllabification=="Mono") %>% ungroup()
# i <- mono %>% filter(vowel == "i")
# ī <- mono %>% filter(vowel == "ī")
# 
# e <- mono %>% filter(vowel == "e")
# ē <- mono %>% filter(vowel == "ē")
# 
# 
# a <- mono %>% filter(vowel == "a")
# ā <- mono %>% filter(vowel == "ā")
# 
# o <- mono %>% filter(vowel == "o")
# ō <- mono %>% filter(vowel == "ō")
# 
# ū <- mono %>% filter(vowel == "ū")
# u <- mono %>% filter(vowel == "u")
# 
# with(i, plotVowels(meanF1, meanF2, vowel=vowel, pretty=T, legend.kwd = "bottomright", plot.tokens = T, plot.means = T, pch.means = vowel, pch.tokens = filename, ellipse.line = T, ellipse.conf = 0.95, alpha.tokens = 0.5))
# with(i, plotVowels(meanF1, meanF2, vowel=vowel, pretty=T, legend.kwd = "bottomright", plot.tokens = T, plot.means = T, pch.means = vowel, pch.tokens = round(mahal_dist), ellipse.line = T, ellipse.conf = 0.95, alpha.tokens = 0.5))
# 
# 
# with(ī, plotVowels(meanF1, meanF2, vowel=vowel, pretty=T, legend.kwd = "bottomright", plot.tokens = T, plot.means = T, pch.means = vowel, pch.tokens = filename, ellipse.line = T, ellipse.conf = 0.95, alpha.tokens = 0.5))
# with(ī, plotVowels(meanF1, meanF2, vowel=vowel, pretty=T, legend.kwd = "bottomright", plot.tokens = T, plot.means = T, pch.means = vowel, pch.tokens = round(mahal_dist), ellipse.line = T, ellipse.conf = 0.95, alpha.tokens = 0.5))
# 
# with(e, plotVowels(meanF1, meanF2, vowel=vowel, pretty=T, legend.kwd = "bottomright", plot.tokens = T, plot.means = T, pch.means = vowel, pch.tokens = filename, ellipse.line = T, ellipse.conf = 0.95, alpha.tokens = 0.5))
# with(e, plotVowels(meanF1, meanF2, vowel=vowel, pretty=T, legend.kwd = "bottomright", plot.tokens = T, plot.means = T, pch.means = vowel, pch.tokens = round(mahal_dist), ellipse.line = T, ellipse.conf = 0.95, alpha.tokens = 0.5))
# 
# with(ē, plotVowels(meanF1, meanF2, vowel=vowel, pretty=T, legend.kwd = "bottomright", plot.tokens = T, plot.means = T, pch.means = vowel, pch.tokens = filename, ellipse.line = T, ellipse.conf = 0.95, alpha.tokens = 0.5))
# with(ē, plotVowels(meanF1, meanF2, vowel=vowel, pretty=T, legend.kwd = "bottomright", plot.tokens = T, plot.means = T, pch.means = vowel, pch.tokens = round(mahal_dist), ellipse.line = T, ellipse.conf = 0.95, alpha.tokens = 0.5))
# 
# with(a, plotVowels(meanF1, meanF2, vowel=vowel, pretty=T, legend.kwd = "bottomright", plot.tokens = T, plot.means = T, pch.means = vowel, pch.tokens = filename, ellipse.line = T, ellipse.conf = 0.95, alpha.tokens = 0.5))
# with(a, plotVowels(meanF1, meanF2, vowel=vowel, pretty=T, legend.kwd = "bottomright", plot.tokens = T, plot.means = T, pch.means = vowel, pch.tokens = round(mahal_dist), ellipse.line = T, ellipse.conf = 0.95, alpha.tokens = 0.5))
# 
# with(ā, plotVowels(meanF1, meanF2, vowel=vowel, pretty=T, legend.kwd = "bottomright", plot.tokens = T, plot.means = T, pch.means = vowel, pch.tokens = filename, ellipse.line = T, ellipse.conf = 0.95, alpha.tokens = 0.5))
# with(ā, plotVowels(meanF1, meanF2, vowel=vowel, pretty=T, legend.kwd = "bottomright", plot.tokens = T, plot.means = T, pch.means = vowel, pch.tokens = round(mahal_dist), ellipse.line = T, ellipse.conf = 0.95, alpha.tokens = 0.5))
# 
# 
# 
# 
# 
# 
# ####### Looking at outlier diphthongs now #######
# 
# ai <- filtered %>% filter(vowel=="ai", Speaker =="LV")
# au <- filtered %>% filter(vowel=="au")
# 
# 
# with(ai, plotVowels(cbind(f1.3, f1.4, f1.5, f1.6, f1.7), cbind(f2.3, f2.4, f2.5, f2.6, f2.7), 
#                     vowel, plot.tokens = TRUE, pch.tokens = filename, alpha.tokens = 0.2, plot.means = TRUE, 
#                     pch.means = vowel, cex.means = 2, var.col.by = vowel, pretty = TRUE, 
#                     diph.arrows = TRUE, diph.args.tokens = list(lwd = 0.8), diph.args.means = list(lwd = 3), 
#                     family = "Charis SIL"))
# 
# with(au, plotVowels(cbind(f1.3, f1.4, f1.5, f1.6, f1.7), cbind(f2.3, f2.4, f2.5, f2.6, f2.7), 
#                     vowel, plot.tokens = TRUE, pch.tokens = filename, alpha.tokens = 0.2, plot.means = TRUE, 
#                     pch.means = vowel, cex.means = 2, var.col.by = vowel, pretty = TRUE, 
#                     diph.arrows = TRUE, diph.args.tokens = list(lwd = 0.8), diph.args.means = list(lwd = 3), 
#                     family = "Charis SIL"))
# 
# 
# 
# 
# 
# 

# 
# means_diph <- dataspread %>% 
#   group_by(vowel,Syllabification,Moras,time) %>%
#   summarise(meanF1 = mean(f1),
#             meanF2 = mean(f2))
# 
# ## re-gather, re-spread
# 
# means_diph_gath <- means_diph %>% gather(formant, hertz, meanF1:meanF2)
# means_diph_gath$formant.time <- paste(means_diph_gath$formant,means_diph_gath$time, sep=".")
# means_diph_spread <- means_diph_gath %>% select(-time,-formant) %>% spread(formant.time, hertz)

