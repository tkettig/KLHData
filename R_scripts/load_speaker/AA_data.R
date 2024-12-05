## Reading in AA data

library(tidyverse)

setwd("/Users/Thomas/Documents/Hawaiian_Phonetics/KLHData/AA/output")

seg_info <- read.csv("segmentation_information.csv", header=TRUE, stringsAsFactors=FALSE) %>%
  select(-inputfile) %>%
  rename(filename = outputfile)

# formants <- read.csv("processed_data/aggregated_data.csv", header=TRUE, stringsAsFactors=FALSE) %>% select(-duration,-label,-number,-color,-group)

formants <- aggregatedata_original() %>%
  select(-duration)
names(formants)[1] <- "filename"
formants$filename <- gsub(".wav", "", formants$filename)

AA <- left_join(seg_info, formants, by="filename")


# Get list of .f0 files
setwd("/Users/Thomas/Documents/Hawaiian_Phonetics/KLHData/AA/output/sounds/")
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

AA <- left_join(AA, df, by="filename")


## Get new f0 measures instead of trusting the ones from FastTrack
## AA is now extracted with 75-320 as min and max in Praat

df <- read.delim("f0_measurements.txt", sep = "\t")

df <- df %>% 
  select(-syll) %>%
  mutate_at(c(3:11), as.numeric)

df <- df %>%
  rowwise() %>% 
  mutate(f0_praat = median(c(f0_2, f0_3, f0_4, f0_5, f0_6,f0_7, f0_8), na.rm = TRUE))

df <- df %>%
  select(filename, f0_praat)

df$filename <- str_replace(df$filename, ".wav", "") 

AA <- left_join(AA, df, by="filename")

setwd("/Users/Thomas/Documents/Hawaiian_Phonetics/KLHData/R_scripts/")
