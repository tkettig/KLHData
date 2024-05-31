####################################
#### Trajectories w/o pronouns #####
####################################

library(reshape2)
library(stringr)
library(directlabels)

setwd("/Users/Thomas/Documents/Hawaiian_Phonetics/Dissertation/R_scripts/")

source("ggplot2_trajectory_functions.R")
source("data_preparation.R")
source("calculating_exclusions.R")

## Select speaker and filter out ones that come directly before or after another vowel. Also filter out Kinney 1956 words.

data <- AA %>% 
  filter(previous_sound %!in% list_of_vowels | next_sound %!in% list_of_vowels) %>%
  filter(Kinney1956 == 0) %>%
  filter(pronouns==0)

## Set directory for plots and tables to be saved into that matches the speaker selected

setwd("/Users/Thomas/Documents/Hawaiian_Phonetics/Dissertation/Manaleo/AA/R_outputs")


### Here are the color palettes to use that are colorblind friendly ####
my_colors <- c("#000000", "#999999", "#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7", "#F0E442") # 9
my_colors <- c("#000000", "#999999", "#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7") # 8
my_colors <- c("#000000", "#999999", "#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00") # 7
my_colors <- c("#000000", "#999999", "#E69F00", "#56B4E9", "#009E73", "#0072B2") # 6
my_colors <- c("#000000", "#999999", "#E69F00", "#56B4E9", "#009E73") # 5
my_colors <- c("#000000", "#E69F00", "#56B4E9", "#009E73") # 4
my_colors <- c("#000000", "#E69F00", "#56B4E9") # 3



###### All aV and āV diphthongs together, primary and secondary stress #####

au <- get_vowel_primary_secondary(data, "au")
ae <- get_vowel_primary_secondary(data,"ae")
ai <- get_vowel_primary_secondary(data,"ai")
ao <- get_vowel_primary_secondary(data,"ao")
āi <- get_vowel_primary_secondary(data, "āi")
āu <- get_vowel_primary_secondary(data,"āu")
means <- rbind(ae,ai,ao,au,āi,āu)

tokens <- data %>% 
  filter(vowel=="ai" |
           vowel=="ae" |
           vowel=="ao" | 
           vowel=="au" |
           vowel=="āi" |
           vowel=="āu") %>% 
  mutate(time2 = (time - 1)/8)

my_colors <- c("#000000", "#999999", "#E69F00", "#56B4E9", "#009E73", "#0072B2") # 6

plot <- traj_means(means, "Speaker AA: aV and āV, means, primary and secondary stress, no pronouns")

# 
# short <- mono_primary_secondary %>% filter(vowel == "a" |vowel == "e" |vowel == "i" |vowel == "o" |vowel == "u" )
# short_means <- mono_primary_secondary_means %>% filter(vowel == "a" |vowel == "e" |vowel == "i" |vowel == "o" |vowel == "u" )

direct.label(plot, list("bottom.points", cex=2))

# Save the plot
ggsave("AA_aV_āV_primary_secondary_means_nopronouns.png", height = 8, width = 8, units = "in")


traj_means_tokens(means, tokens, "Speaker AA: aV and āV, means and tokens, primary and secondary stress, no pronouns")

# Save the plot
ggsave("AA_aV_āV_primary_secondary_nopronouns.png", height = 8, width = 8, units = "in")

t <- tokens %>%
  ungroup() %>%
  select(vowel,filename) %>%
  distinct() %>%
  group_by(vowel) %>%
  summarise(tokens = n())
t

# Save the table
write.table(t, file = "AA_aV_āV_primary_secondary_nopronouns.txt", sep = ",", quote = FALSE, row.names = F)





###### ou, ō, ei, ē, ī, ū  "non-low long vowels", primary and secondary #####

ou <- get_vowel_primary_secondary(data, "ou")
ō <- get_vowel_primary_secondary(data,"ō")
ei <- get_vowel_primary_secondary(data,"ei")
ē <- get_vowel_primary_secondary(data,"ē")
ī <- get_vowel_primary_secondary(data,"ī")
ū <- get_vowel_primary_secondary(data,"ū")

means <- rbind(ou,ō,ei,ī,ē,ū)

tokens <- data %>% 
  filter(vowel=="ou" |
           vowel=="ō" |
           vowel=="ei" | 
           vowel=="ē" |
           vowel=="ī" |
           vowel=="ū") %>% 
  mutate(time2 = (time - 1)/8)

my_colors <- c("#000000", "#999999", "#E69F00", "#56B4E9", "#009E73", "#0072B2") # 6


plot <- traj_means(means, "Speaker AA: Selected non-low vowels, means, primary and secondary stress")
direct.label(plot, list("bottom.points", cex=2))

# Save the plot
ggsave("AA_nonlow_primary_secondary_means_nopronouns.png", height = 8, width = 8, units = "in")

traj_means_tokens(means, tokens, "Speaker AA: Selected non-low vowels, means and tokens, primary and secondary stress, no pronouns")

# Save the plot
ggsave("AA_nonlow_primary_secondary_nopronouns.png", height = 8, width = 8, units = "in")

t <- tokens %>%
  ungroup() %>%
  select(vowel,filename) %>%
  distinct() %>%
  group_by(vowel) %>%
  summarise(tokens = n())
t

# Save the table
write.table(t, file = "AA_nonlow_primary_secondary_nopronouns.txt", sep = ",", quote = FALSE, row.names = F)







##### All long monopthongs, primary and secondary stress #####

ī <- get_vowel_primary_secondary(data, "ī")
ē <- get_vowel_primary_secondary(data,"ē")
ā <- get_vowel_primary_secondary(data,"ā")
ō <- get_vowel_primary_secondary(data,"ō")
ū <- get_vowel_primary_secondary(data,"ū")

means <- rbind(ī,ē,ā,ō,ū)

tokens <- data %>% 
  filter(vowel=="ī" |
           vowel=="ē" |
           vowel=="ā" | 
           vowel=="ō" |
           vowel=="ū") %>% 
  filter(stress == "primary" |stress=="secondary") %>%
  mutate(time2 = (time - 1)/8)

my_colors <- c("#000000", "#999999", "#E69F00", "#56B4E9", "#009E73") # 5

##### Means alone

plot <- traj_means(means, "Speaker AA: Long monophthong trajectories, primary and stress, means")
direct.label(plot, list("bottom.points", cex=2))

# Save the plot
ggsave("AA_long_mono_primary_secondary_means.png", height = 8, width = 8, units = "in")

##### Means and individual tokens together

traj_means_tokens(means, tokens, "Speaker AA: Long monophthong trajectories, means and tokens, primary and secondary stress")

# Save the plot
ggsave("AA_long_mono_primary_secondary.png", height = 8, width = 8, units = "in")

t <- tokens %>%
  ungroup() %>%
  select(vowel,filename) %>%
  distinct() %>%
  group_by(vowel) %>%
  summarise(tokens = n())
t

# Save the table
write.table(t, file = "AA_long_mono_primary_secondary.txt", sep = ",", quote = FALSE, row.names = F)









##### All short diphthongs (primary and secondary stress) ######

au <- get_vowel_primary_secondary(data,"au")
ae <- get_vowel_primary_secondary(data,"ae")
ai <- get_vowel_primary_secondary(data,"ai")
ao <- get_vowel_primary_secondary(data,"ao")
ei <- get_vowel_primary_secondary(data,"ei")
eu <- get_vowel_primary_secondary(data,"eu")
ou <- get_vowel_primary_secondary(data,"ou")
oi <- get_vowel_primary_secondary(data,"oi")
iu <- get_vowel_primary_secondary(data,"iu")

means <- rbind(ae,ai,ao,au,ei,ou,oi,iu)

tokens <- data %>% 
  filter(vowel=="ai" |
           vowel=="ae" |
           vowel=="ao" | 
           vowel=="au" |
           vowel=="eu" |
           vowel=="ou" |
           vowel=="oi" |
           vowel=="iu" |
           vowel=="ei") %>%
  mutate(time2 = (time - 1)/8)

my_colors <- c("#000000", "#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") # 9

##### Means alone

plot <- traj_means(means, "Speaker AA: Short diphthongs, means, primary and secondary stress")
direct.label(plot, list("bottom.points", cex=2))


# Save the plot
ggsave("AA_short_diph_primary_secondary_means.png", height = 8, width = 8, units = "in")

##### Means and individual tokens together

traj_means_tokens(means, tokens, "Speaker AA: Short diphthongs, means and tokens, primary and secondary stress")

# Save the plot
ggsave("AA_short_diph_primary_secondary.png", height = 8, width = 8, units = "in")

t <- tokens %>%
  ungroup() %>%
  select(vowel,filename) %>%
  distinct() %>%
  group_by(vowel) %>%
  summarise(tokens = n())
t

# Save the table
write.table(t, file = "AA_short_diph_primary_secondary.txt", sep = ",", quote = FALSE, row.names = F)


















# 
# ##### All long monopthongs, primary stress #####
# 
# ī <- get_vowel_primary(data, "ī")
# ē <- get_vowel_primary(data,"ē")
# ā <- get_vowel_primary(data,"ā")
# ō <- get_vowel_primary(data,"ō")
# ū <- get_vowel_primary(data,"ū")
# 
# means <- rbind(ī,ē,ā,ō,ū)
# 
# tokens <- data %>% 
#   filter(vowel=="ī" |
#            vowel=="ē" |
#            vowel=="ā" | 
#            vowel=="ō" |
#            vowel=="ū") %>% 
#   filter(stress == "primary") %>%
#   mutate(time2 = (time - 1)/8)
# 
# 
# my_colors <- c("#000000", "#999999", "#E69F00", "#56B4E9", "#009E73") # 5
# 
# ##### Means alone
# 
# plot <- traj_means(means, "Speaker AA: Long monophthong trajectories, primary stress, means")
# my.dl <- list(box.color="black", "draw.polygons")
# direct.label(plot, list("bottom.points","calc.boxes", "enlarge.box", "my.dl"))
# 
# # Save the plot
# ggsave("AA_long_mono_primary_means.png", height = 8, width = 8, units = "in")
# 
# ##### Means and individual tokens together
# 
# traj_means_tokens(means, tokens, "Speaker AA: Long monophthong trajectories, means and tokens, primary stress")
# 
# # Save the plot
# ggsave("AA_long_mono_primary.png", height = 8, width = 8, units = "in")
# 
# t <- tokens %>%
#   ungroup() %>%
#   select(vowel,filename) %>%
#   distinct() %>%
#   group_by(vowel) %>%
#   summarise(tokens = n())
# t
# 
# # Save the table
# write.table(t, file = "AA_long_mono_primary_means.txt", sep = ",", quote = FALSE, row.names = F)
# 
# 
# 
# ###### All long diphthongs (primary and secondary stress) ######
# 
# āi <- get_vowel_primary_secondary(data, "āi")
# # āe <- get_vowel_primary_secondary(data,"āe") # none for AA
# āu <- get_vowel_primary_secondary(data,"āu")
# # āo <- get_vowel_primary_secondary(data,"āo") # none for AA
# # ēi <- get_vowel_primary_secondary(data,"ēi") # none for AA
# # ōu <- get_vowel_primary_secondary(data,"ōu") # none for AA
# 
# 
# means <- rbind(āi,āu)
# 
# tokens <- data %>% 
#   filter(vowel=="āi" |
#            vowel=="āu"  ) %>% 
#   mutate(time2 = (time - 1)/8)
# 
# my_colors <- c("#000000", "#E69F00") # 2
# 
# traj_means(means, "Speaker AA: Long diphthongs, means, primary and secondary stress")
# 
# # Save the plot
# ggsave("long_diph_primary_secondary_means.png", height = 8, width = 8, units = "in")
# 
# traj_means_tokens(means, tokens, "Speaker AA: Long diphthongs, means and tokens, primary and secondary stress")
# 
# # Save the plot
# ggsave("long_diph_primary_secondary.png", height = 8, width = 8, units = "in")
# 
# t <- tokens %>%
#   ungroup() %>%
#   select(vowel,filename) %>%
#   distinct() %>%
#   group_by(vowel) %>%
#   summarise(tokens = n())
# t
# 
# # Save the table
# write.table(t, file = "long_diph_primary_secondary.txt", sep = ",", quote = FALSE, row.names = F)
# 
# 
# 
# 
# ##### All short aV diphthongs, primary and secondary stress #####
# 
# au <- get_vowel_primary_secondary(data, "au")
# ae <- get_vowel_primary_secondary(data,"ae")
# ai <- get_vowel_primary_secondary(data,"ai")
# ao <- get_vowel_primary_secondary(data,"ao")
# means <- rbind(ae,ai,ao,au)
# 
# tokens <- data %>% 
#   filter(vowel=="ai" |
#            vowel=="ae" |
#            vowel=="ao" | 
#            vowel=="au") %>% 
#   mutate(time2 = (time - 1)/8)
# 
# my_colors <- c("#000000", "#E69F00", "#56B4E9", "#009E73") # 4
# 
# plot <- traj_means(means, "Speaker AA: aV trajectories, means, primary and secondary stress")
# my.dl <- list(box.color="black", "draw.polygons")
# direct.label(plot, list("bottom.points","calc.boxes", "enlarge.box", "my.dl"))
# 
# # Save the plot
# ggsave("aV_primary_secondary_means.png", height = 8, width = 8, units = "in")
# 
# traj_means_tokens(means, tokens, "Speaker AA: aV trajectories, means and tokens, primary and secondary stress")
# 
# # Save the plot
# ggsave("aV_primary_secondary.png", height = 8, width = 8, units = "in")
# 
# t <- tokens %>%
#   ungroup() %>%
#   select(vowel,filename) %>%
#   distinct() %>%
#   group_by(vowel) %>%
#   summarise(tokens = n())
# t
# 
# # Save the table
# write.table(t, file = "aV_primary_secondary.txt", sep = ",", quote = FALSE, row.names = F)
# 







