## ggplot2 functions that I need for graphing trajectories
library(ggplot2)
library(directlabels)

###### DOING VOWEL PLOTTING WITH GGPLOT2 #####

loess_smoothing <- function(df, f1, f2) {
  batch <- df %>% mutate(time2 = (time - 1)/8)
  f1_mod <- loess(f1 ~ time2, data = batch, na.action=na.exclude)
  f2_mod <- loess(f2 ~ time2, data = batch, na.action=na.exclude)
  pred <- data.frame(time2 = seq(0,1, length = 100))
  pred$f1 <- predict(f1_mod, newdata = pred)
  pred$f2 <- predict(f2_mod, newdata = pred)
  return(pred)
}

##### Can plot multiple vowel trajectories on same graph, with colors separating the instances #####

traj_means <- function(means, title_of_chart)  {
  ggplot(means, aes(f2, f1, color=vowel)) +
    geom_path(arrow = arrow(), size = 1.5)+
    scale_color_manual(values=my_colors) +
    labs(title = title_of_chart, x = "F2 (Hz)", y = "F1 (Hz)") +
    theme_classic() +
    theme(legend.title=element_blank(), plot.title = element_text(hjust = 0.5)) +
    scale_y_reverse()+
    scale_x_reverse()
}

traj_means_pron <- function(means, title_of_chart)  {
  ggplot(means, aes(f2, f1, color=vowel, linetype=class)) +
    geom_path(arrow = arrow(), size = 1.5)+
    scale_color_manual(values=my_colors) +
    scale_linetype_manual(values=my_linetype) +
    labs(title = title_of_chart, x = "F2 (Hz)", y = "F1 (Hz)") +
    theme_classic() +
    theme(legend.title=element_blank(), plot.title = element_text(hjust = 0.5)) +
    scale_y_reverse()+
    scale_x_reverse()
}

traj_means_tokens <- function(means, tokens, title_of_chart)  {
  ggplot() +
    geom_path(data = means, aes(f2, f1, color=vowel), arrow = arrow(), size = 1, alpha = 1)+
    geom_path(data = tokens, aes(f2, f1, group=filename, color=vowel), arrow = arrow(), size=0.2, alpha=1)+
    scale_color_manual(values=my_colors) +
    labs(title = title_of_chart, x = "F2 (Hz)", y = "F1 (Hz)") +
    theme_classic() +
    theme(legend.title=element_blank(), plot.title = element_text(hjust = 0.5)) +
    scale_y_reverse()+
    scale_x_reverse()
}


get_vowel_primary <- function(df, label) {
  batch <- df %>% filter(vowel==label, stress=="primary")
  df <- loess_smoothing(batch)
  df$vowel <- label
  return(df)
}

get_vowel_primary_secondary <- function(df, label) {
  batch <- df %>% filter(vowel==label, stress=="primary" | stress=="secondary")
  df <- loess_smoothing(batch)
  df$vowel <- label
  return(df)
}

get_vowel_unstressed <- function(df, label) {
  batch <- df %>% filter(vowel==label, stress=="unstressed")
  df <- loess_smoothing(batch)
  df$vowel <- label
  return(df)
}
