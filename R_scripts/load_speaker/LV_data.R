## Reading in LV data
library(tidyverse)

setwd("/Users/Thomas/Documents/Hawaiian_Phonetics/KLHData/LV/output_May2023")

seg_info <- read.csv("segmentation_information.csv", header=TRUE, stringsAsFactors=FALSE) %>%
  select(-inputfile) %>%
  rename(filename = outputfile)

# formants <- read.csv("processed_data/aggregated_data.csv", header=TRUE, stringsAsFactors=FALSE) %>% select(-duration,-label,-number,-color,-group)

formants <- aggregatedata_myself() %>%
  select(-duration)
names(formants)[1] <- "filename"
formants$filename <- gsub(".wav", "", formants$filename)

LV1 <- left_join(seg_info, formants, by="filename")
LV1$f0 <- NA
LV1$intensity <- NA


### Add in the f0 measurements from reaper

setwd("/Users/Thomas/Documents/Hawaiian_Phonetics/KLHData/LV/output_dissertation/sounds/")

# Get list of .f0 files
file_list = list.files(pattern = "*.f0")

# Make it into a data frame
data_list=lapply(file_list, read.table, header = T, sep = ",")
for (i in 1:length(data_list)){
  data_list[[i]]<-cbind(data_list[[i]],file_list[i])
  names(data_list[[i]]) <- c("info", "filename")
  data_list[[i]] <- data_list[[i]] %>%
    separate(info, c('time', 'voicing', 'f0'), sep = " ") %>%
    filter(row_number() <= (n()-5)) %>% # Filtering out last 5 rows
    filter(row_number() >= 5) %>% # Filtering out first 5 rows
    filter(voicing == 1) %>% # Getting only voiced sections
    group_by(filename) %>%
    summarise(meanf0 = mean(as.numeric(f0)),
              medianf0 = median(as.numeric(f0))) # Getting mean/median
}

df <- do.call("rbind", data_list)
df$filename <- df$filename %>% str_remove(".wav.f0")

LV <- left_join(LV1, df, by="filename")

setwd("/Users/Thomas/Documents/Hawaiian_Phonetics/Dissertation/R_scripts/")


## All this stuff isn't working for some reason. It should be that I can get f0 and intensity measures from this,
## but it's not liking the files and I can't figure out why

setwd("/Users/Thomas/Documents/Hawaiian_Phonetics/KLHData/LV/output_dissertation")

seg_info <- read.csv("segmentation_information.csv", header=TRUE, stringsAsFactors=FALSE) %>%
  select(-inputfile) %>%
  rename(filename = outputfile)

# formants <- read.csv("processed_data/aggregated_data.csv", header=TRUE, stringsAsFactors=FALSE) %>% select(-duration,-label,-number,-color,-group)

formants <- aggregatedata_myself() %>%
   select(-duration)
names(formants)[1] <- "filename"
formants$filename <- gsub(".wav", "", formants$filename)

LV2 <- left_join(seg_info, formants, by="filename")

LV <- LV1
LV$f0 <- LV2$f0
LV$intensity <- LV2$intensity
setwd("/Users/Thomas/Documents/Hawaiian_Phonetics/Dissertation/R_scripts/")

