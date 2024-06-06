### Filtering the data to get maximum and minimum points ####

## This gets only the rows with max f1 for /a/ and /ā/

max_f1 <- data %>% 
  group_by(original_order) %>% 
  mutate(max_f1 = max(f1)) %>%
  filter(f1 == max_f1)

## This chooses from those multiple ones which is the F1 max closest to the midpoint (time 5)

max_f1 <- max_f1 %>%
  group_by(original_order) %>%
  slice(which.min(abs( - 5.1)))

## This gets only the rows with max f1, from between time 1-5 (good for measurement of upgliding diphthongs)

max_f1_upglides <- max_f1 %>% 
  filter(time<5) %>%
  group_by(original_order) %>% 
  mutate(max_f1 = max(f1)) %>%
  filter(f1 == max_f1)

## This chooses from those multiple ones which is the F1 max closest to time 3

max_f1_upglides <- max_f1_upglides %>%
  group_by(original_order) %>%
  slice(which.min(abs( - 3.1)))

## This gets only the rows with max f2 for /i, ī, e, ē/

max_f2 <- data %>% 
  group_by(original_order) %>% 
  mutate(max_f2 = max(f2)) %>%
  filter(f2 == max_f2)

## This chooses from those multiple ones which is the F2 max closest to the midpoint (time 5)

max_f2 <- max_f2 %>%
  group_by(original_order) %>%
  slice(which.min(abs( - 5.1)))

## This gets only the rows with min f2 for /u, ū, o, ō/

min_f2 <- data %>% 
  group_by(original_order) %>% 
  mutate(min_f2 = min(f2)) %>%
  filter(f2 == min_f2)

## This chooses from those multiple ones which is the F2 min closest to the midpoint (time 5)

min_f2 <- min_f2 %>%
  group_by(original_order) %>%
  slice(which.min(abs( - 5.1)))

