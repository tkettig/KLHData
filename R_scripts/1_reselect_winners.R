### Merge manually edited list of winners with winners file so Fast Track to can re-run winner selection

library(tidyverse)

###### LV updated May 2023 ######

# Use previously calculated omits from dissertation, but calculate new mahals from new data
setwd("/Users/Thomas/Documents/Hawaiian_Phonetics/Dissertation/Manaleo/LV/output_dissertation")

# Read in the csv where I've manually looked at everything
mahal_means <- read.csv("mahal_means.csv", header=TRUE, stringsAsFactors=FALSE) %>%
  rename(file = filename) %>%
  select(file, edit)

# Now get a list of the ones that I want to omit in next round
omit <- mahal_means %>% filter(edit==0)
list_to_omit_LV <- omit$file

# # 
# ###### LV used for dissertation ######
# 
# setwd("/Users/Thomas/Documents/Hawaiian_Phonetics/Dissertation/Manaleo/LV/output_dissertation")
# 
# # Read in the csv where I've manually looked at everything
# mahal_means <- read.csv("mahal_means.csv", header=TRUE, stringsAsFactors=FALSE) %>%
#   rename(file = filename) %>%
#   select(file, edit)
# 
# # Read in the original winners file
# winners <- read.csv("winners.csv", header=TRUE, stringsAsFactors=FALSE) %>%
#   select(-edit)
# 
# # Join the two together
# winners <- left_join(winners,mahal_means, by="file")
# 
# # Get the new winners into place
# winners$winner <- ifelse(is.na(winners$edit) | winners$edit == 0, winners$winner, winners$edit)
# winners$F1 <- ifelse(is.na(winners$edit) | winners$edit == 0, winners$winner, winners$edit)
# winners$F2 <- ifelse(is.na(winners$edit) | winners$edit == 0, winners$winner, winners$edit)
# winners$F3 <- ifelse(is.na(winners$edit) | winners$edit == 0, winners$winner, winners$edit)
# 
# # Now get a list of the ones that I want to omit in next round
# omit <- winners %>% filter(edit==0)
# list_to_omit_LV <- omit$file
# 
# # Got to set "edit" as 0 now for it to work in Fast Track
# winners$edit <- 0
# 
# # Write back to where it needs to be in the folder
# # write.csv(winners, "/Users/Thomas/Documents/Hawaiian_Phonetics/Dissertation/Manaleo/LV/output/winners.csv", row.names = F, quote = F)


###### IN ######

setwd("/Users/Thomas/Documents/Hawaiian_Phonetics/Dissertation/Manaleo/IN/output")

# Read in the csv where I've manually looked at everything
mahal_means <- read.csv("mahal_means.csv", header=TRUE, stringsAsFactors=FALSE) %>% 
  rename(file = filename) %>% 
  select(file, edit)

# Read in the original winners file
winners <- read.csv("winners.csv", header=TRUE, stringsAsFactors=FALSE) %>% 
  select(-edit)

# Join the two together
winners <- left_join(winners,mahal_means, by="file") 

# Get the new winners into place
winners$winner <- ifelse(is.na(winners$edit) | winners$edit == 0, winners$winner, winners$edit)
winners$F1 <- ifelse(is.na(winners$edit) | winners$edit == 0, winners$winner, winners$edit)
winners$F2 <- ifelse(is.na(winners$edit) | winners$edit == 0, winners$winner, winners$edit)
winners$F3 <- ifelse(is.na(winners$edit) | winners$edit == 0, winners$winner, winners$edit)

# Now get a list of the ones that I want to omit in next round
omit <- winners %>% filter(edit==0)
list_to_omit_IN <- omit$file

# Got to set "edit" as 0 now for it to work in Fast Track
winners$edit <- 0

# Write back to where it needs to be in the folder
# write.csv(winners, "/Users/Thomas/Documents/Hawaiian_Phonetics/Dissertation/Manaleo/IN/output/winners.csv", row.names = F, quote = F)



###### HM ######

setwd("/Users/Thomas/Documents/Hawaiian_Phonetics/Dissertation/Manaleo/HM/output")

# Read in the csv where I've manually looked at everything
mahal_means <- read.csv("mahal_means.csv", header=TRUE, stringsAsFactors=FALSE) %>% 
  rename(file = filename) %>% 
  select(file, edit)

# Read in the original winners file
winners <- read.csv("winners.csv", header=TRUE, stringsAsFactors=FALSE) %>% 
  select(-edit)

# Join the two together
winners <- left_join(winners,mahal_means, by="file") 

# Get the new winners into place
winners$winner <- ifelse(is.na(winners$edit) | winners$edit == 0, winners$winner, winners$edit)
winners$F1 <- ifelse(is.na(winners$edit) | winners$edit == 0, winners$winner, winners$edit)
winners$F2 <- ifelse(is.na(winners$edit) | winners$edit == 0, winners$winner, winners$edit)
winners$F3 <- ifelse(is.na(winners$edit) | winners$edit == 0, winners$winner, winners$edit)

# Now get a list of the ones that I want to omit in next round
omit <- winners %>% filter(edit==0)
list_to_omit_HM <- omit$file

# Got to set "edit" as 0 now for it to work in Fast Track
winners$edit <- 0

# Write back to where it needs to be in the folder
# write.csv(winners, "/Users/Thomas/Documents/Hawaiian_Phonetics/Dissertation/Manaleo/HM/output/winners.csv", row.names = F, quote = F)



###### JM ######

setwd("/Users/Thomas/Documents/Hawaiian_Phonetics/Dissertation/Manaleo/JM/output")

# Read in the csv where I've manually looked at everything
mahal_means <- read.csv("mahal_means.csv", header=TRUE, stringsAsFactors=FALSE) %>% 
  rename(file = filename) %>% 
  select(file, edit)

# Read in the original winners file
winners <- read.csv("winners.csv", header=TRUE, stringsAsFactors=FALSE) %>% 
  select(-edit)

# Join the two together
winners <- left_join(winners,mahal_means, by="file") 

# Get the new winners into place
winners$winner <- ifelse(is.na(winners$edit) | winners$edit == 0, winners$winner, winners$edit)
winners$F1 <- ifelse(is.na(winners$edit) | winners$edit == 0, winners$winner, winners$edit)
winners$F2 <- ifelse(is.na(winners$edit) | winners$edit == 0, winners$winner, winners$edit)
winners$F3 <- ifelse(is.na(winners$edit) | winners$edit == 0, winners$winner, winners$edit)

# Now get a list of the ones that I want to omit in next round
omit <- winners %>% filter(edit==0)
list_to_omit_JM <- omit$file

# Got to set "edit" as 0 now for it to work in Fast Track
winners$edit <- 0

# Write back to where it needs to be in the folder
# write.csv(winners, "/Users/Thomas/Documents/Hawaiian_Phonetics/Dissertation/Manaleo/JM/output/winners.csv", row.names = F, quote = F)


###### DK ######

setwd("/Users/Thomas/Documents/Hawaiian_Phonetics/Dissertation/Manaleo/DK/output")

# Read in the csv where I've manually looked at everything
mahal_means <- read.csv("mahal_means.csv", header=TRUE, stringsAsFactors=FALSE) %>% 
  rename(file = filename) %>% 
  select(file, edit)

# Read in the original winners file
winners <- read.csv("winners.csv", header=TRUE, stringsAsFactors=FALSE) %>% 
  select(-edit)

# Join the two together
winners <- left_join(winners,mahal_means, by="file") 

# Get the new winners into place
winners$winner <- ifelse(is.na(winners$edit) | winners$edit == 0, winners$winner, winners$edit)
winners$F1 <- ifelse(is.na(winners$edit) | winners$edit == 0, winners$winner, winners$edit)
winners$F2 <- ifelse(is.na(winners$edit) | winners$edit == 0, winners$winner, winners$edit)
winners$F3 <- ifelse(is.na(winners$edit) | winners$edit == 0, winners$winner, winners$edit)

# Now get a list of the ones that I want to omit in next round
omit <- winners %>% filter(edit==0)
list_to_omit_DK <- omit$file

# Got to set "edit" as 0 now for it to work in Fast Track
winners$edit <- 0

# Write back to where it needs to be in the folder
 write.csv(winners, "/Users/Thomas/Documents/Hawaiian_Phonetics/Dissertation/Manaleo/DK/output/winners.csv", row.names = F, quote = F)



###### SB ######

setwd("/Users/Thomas/Documents/Hawaiian_Phonetics/Dissertation/Manaleo/SB/output")

# Read in the csv where I've manually looked at everything
mahal_means <- read.csv("mahal_means.csv", header=TRUE, stringsAsFactors=FALSE) %>%
  rename(file = filename) %>%
  select(file, edit)

# Read in the original winners file
winners <- read.csv("winners.csv", header=TRUE, stringsAsFactors=FALSE) %>%
  select(-edit)

# Join the two together
winners <- left_join(winners,mahal_means, by="file")

# Get the new winners into place
winners$winner <- ifelse(is.na(winners$edit) | winners$edit == 0, winners$winner, winners$edit)
winners$F1 <- ifelse(is.na(winners$edit) | winners$edit == 0, winners$winner, winners$edit)
winners$F2 <- ifelse(is.na(winners$edit) | winners$edit == 0, winners$winner, winners$edit)
winners$F3 <- ifelse(is.na(winners$edit) | winners$edit == 0, winners$winner, winners$edit)

# Now get a list of the ones that I want to omit in next round
omit <- winners %>% filter(edit==0)
list_to_omit_SB <- omit$file

# Got to set "edit" as 0 now for it to work in Fast Track
winners$edit <- 0

# Write back to where it needs to be in the folder
# write.csv(winners, "/Users/Thomas/Documents/Hawaiian_Phonetics/Dissertation/Manaleo/SB/output/winners.csv", row.names = F, quote = F)




###### RM ######

setwd("/Users/Thomas/Documents/Hawaiian_Phonetics/Dissertation/Manaleo/RM/output")

# Read in the csv where I've manually looked at everything
mahal_means <- read.csv("mahal_means.csv", header=TRUE, stringsAsFactors=FALSE) %>% 
  rename(file = filename) %>% 
  select(file, edit)

# Read in the original winners file
winners <- read.csv("winners.csv", header=TRUE, stringsAsFactors=FALSE) %>% 
  select(-edit)

# Join the two together
winners <- left_join(winners,mahal_means, by="file") 

# Get the new winners into place
winners$winner <- ifelse(is.na(winners$edit) | winners$edit == 0, winners$winner, winners$edit)
winners$F1 <- ifelse(is.na(winners$edit) | winners$edit == 0, winners$winner, winners$edit)
winners$F2 <- ifelse(is.na(winners$edit) | winners$edit == 0, winners$winner, winners$edit)
winners$F3 <- ifelse(is.na(winners$edit) | winners$edit == 0, winners$winner, winners$edit)

# Now get a list of the ones that I want to omit in next round
omit <- winners %>% filter(edit==0)
list_to_omit_RM <- omit$file

# Got to set "edit" as 0 now for it to work in Fast Track
winners$edit <- 0

# Write back to where it needs to be in the folder
# write.csv(winners, "/Users/Thomas/Documents/Hawaiian_Phonetics/Dissertation/Manaleo/RM/output/winners.csv", row.names = F, quote = F)




###### AA ######

setwd("/Users/Thomas/Documents/Hawaiian_Phonetics/Dissertation/Manaleo/AA/output")

# Read in the csv where I've manually looked at everything
mahal_means <- read.csv("mahal_means.csv", header=TRUE, stringsAsFactors=FALSE) %>%
  rename(file = filename) %>%
  select(file, edit)

# Read in the original winners file
winners <- read.csv("winners.csv", header=TRUE, stringsAsFactors=FALSE) %>%
  select(-edit)

# Join the two together
winners <- left_join(winners,mahal_means, by="file")

# Get the new winners into place
winners$winner <- ifelse(is.na(winners$edit) | winners$edit == 0, winners$winner, winners$edit)
winners$F1 <- ifelse(is.na(winners$edit) | winners$edit == 0, winners$winner, winners$edit)
winners$F2 <- ifelse(is.na(winners$edit) | winners$edit == 0, winners$winner, winners$edit)
winners$F3 <- ifelse(is.na(winners$edit) | winners$edit == 0, winners$winner, winners$edit)

# Now get a list of the ones that I want to omit in next round
omit <- winners %>% filter(edit==0)
list_to_omit_AA <- omit$file

# Got to set "edit" as 0 now for it to work in Fast Track
winners$edit <- 0

# Write back to where it needs to be in the folder
# write.csv(winners, "/Users/Thomas/Documents/Hawaiian_Phonetics/Dissertation/Manaleo/AA/output/winners.csv", row.names = F, quote = F)
 
