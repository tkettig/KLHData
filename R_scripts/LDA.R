library(MASS)
library(readxl)
library(tidyverse)
library(lmerTest)
library(caret)
library(devtools)
library(ggord)
library(klaR)
library(partykit)

dat_2syl <- read.csv("dat_2syl.csv")
dat_2syl_z <- read.csv("dat_2syl_z.csv")


## CIT

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
                     control=ctree_control(minbucket=400))
plot(ctree_model)



## LDA

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



