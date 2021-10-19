# analyze the vot of the stimuli 

list_of_files <- list.files(path = here("exp", "stim"), recursive = TRUE,
                            pattern = "\\.TextGrid$", 
                            full.names = TRUE) %>% 
  as.data.frame()

df2 <- character()

for (iteration in 1:nrow(list_of_files)) {
  df <- read_textgrid(list_of_files$.[iteration]) %>% 
    add_words() %>% 
    mutate(file = list_of_files$.[iteration])
  df2 <- rbind(df, df2)
}



tidy_df <- df2 %>%
  mutate(file = str_remove(file, 
                           "/Users/kyleparrish/Documents/GitHub/l3_production_study/exp/stim/")) %>% 
  mutate(vot = xmax - xmin) %>%
  mutate(relative_vot = vot/duration) %>% 
  dplyr::select(text, tier_name, word, duration, vot, relative_vot) %>% 
  filter(text == "t" | text == "p" | text == "k") %>% 
  filter(!(text == "t" & word == "quitte")) %>% 
  filter(!(text == "t" & word == "patte")) %>% 
  filter(!(word == "quette")) %>% 
  mutate(vot_ms = vot*1000)

# Spanish-like!! 
mean(tidy_df$relative_vot)
sd(tidy_df$relative_vot)

mean(tidy_df$vot)
sd(tidy_df$vot)

tidy_df %>% 
  write.csv(here("data", "tidy", "stim.csv"))