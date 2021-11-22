# 09 appendix and extras 

source(here::here("scripts", "07_small_data.R"))


#### more descriptive stats 

# Absolute VOT desc values

## pooled
all_df %>% 
  group_by(language) %>% 
  summarise(mean = mean(vot_ms), sd = sd(vot_ms))

## per consonant
all_df %>% 
  group_by(language, text) %>% 
  summarise(mean = mean(vot_ms), sd = sd(vot_ms))

# L2 subset
## pooled
l2_df %>% 
  group_by(language) %>% 
  summarise(mean = mean(vot_ms), sd = sd(vot_ms))

## per consonant
l2_df %>% 
  group_by(language, text) %>% 
  summarise(mean = mean(vot_ms), sd = sd(vot_ms))


# L1 subset
## pooled
l1_df %>% 
  group_by(language) %>% 
  summarise(mean = mean(vot_ms), sd = sd(vot_ms))

## per consonant
l1_df %>% 
  group_by(language, text) %>% 
  summarise(mean = mean(vot_ms), sd = sd(vot_ms))


# Relative VOT of the full dataset per vowel, consonant and per language

vowel_desc <- all_df %>% 
  group_by(text, vowel, language) %>% 
  summarise(mean_vot = mean(relative_vot), sd_vot = sd(relative_vot))

# Plot of VOT by vocalic context 

vowel_desc %>% 
  ggplot(aes(x = language, y = mean_vot, color = text, shape = vowel)) + geom_point()


### disqulified participants desc  

## relative vot 
### pooled
dis_df %>% 
  group_by(language) %>% 
  summarise(mean = mean(relative_vot), sd = sd(relative_vot))
### per consonant
dis_df %>% 
  group_by(language, text) %>% 
  summarise(mean = mean(relative_vot), sd = sd(relative_vot))

## absolute vot 
### pooled
dis_df %>% 
  group_by(language) %>% 
  summarise(mean = mean(vot_ms), sd = sd(vot_ms))
### per consonant
dis_df %>% 
  group_by(language, text) %>% 
  summarise(mean = mean(vot_ms), sd = sd(vot_ms))

