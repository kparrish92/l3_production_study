
# autovot comp with hand correction 

source(here::here("scripts", "00_libs.R"))
source(here("scripts", "01_helpers.R"))

# find all Textgrid files under "all_uploads"
list_of_files <- list.files(path = here("data", "all_uploads", "data_out"), recursive = TRUE,
                            pattern = "\\.TextGrid$", 
                            full.names = TRUE) %>% 
  as.data.frame()


# loop readtextgrid and add_words fuctions over the list of files
df2 <- character()

for (iteration in 1:nrow(list_of_files)) {
  df <- read_textgrid(list_of_files$.[iteration]) %>% 
    add_words() %>% 
    mutate(file = list_of_files$.[iteration])
  df2 <- rbind(df, df2)
}



english_list <- c("tipping","teller","tacky","penny","pass","parrot","kitten","kennel","cabbage")
spanish_list <- c("tiro","tema","talla","quiso","queja", "cama", "piso","pena","pato")
french_list <- c("tir","terre","tasse","quitte","quelle","pile","pere","patte")





tidy_df <- df2 %>%
  mutate(file = str_remove(file, 
                           "/Users/kyleparrish/Documents/GitHub/l3_production_study/data/all_uploads/data_out/")) %>% 
  separate(file, into = c("participant", "other", sep = "_")) %>% 
  mutate(vot = xmax - xmin) %>%
  mutate(relative_vot = vot/duration) %>% 
  dplyr::select(participant, text, tier_name, word, duration, vot, relative_vot) %>% 
  filter(text == "t" | text == "p" | text == "k") %>% 
  mutate(language = case_when(word %in% english_list ~ "english",
                              word %in% spanish_list ~ "spanish", 
                              word %in% french_list ~ "french")) %>% 
  filter(!(text == "p" & word == "tipping")) %>% 
  filter(!(text == "k" & word == "tacky")) %>% 
  filter(!(text == "t" & word == "kitten")) %>% 
  filter(!(text == "t" & word == "parrot")) %>% 
  filter(!(text == "t" & word == "quitte")) %>% 
  filter(!(text == "t" & word == "patte")) %>% 
  filter(!(text == "t" & word == "pato")) %>% 
  mutate(vot_ms = vot*1000) %>% 

# filter extreme values over 150ms
filter_tidy_df <- tidy_df %>% 
  filter(vot_ms < 100)

# how many values were over 150ms?
excluded_dat <- nrow(tidy_df) - nrow(filter_tidy_df)

# how many participants?

nparticipants <- length(unique(filter_tidy_df$participant))

# desc stats of pooled relative VOT per language 
filter_tidy_df %>% 
  group_by(language) %>% 
  summarise(mean = mean(relative_vot), sd = sd(relative_vot))
# desc stats of absolute vot
filter_tidy_df %>% 
  group_by(language) %>% 
  summarise(mean = mean(vot_ms), sd = sd(vot_ms))

# participant pooled means 
participant_df <- filter_tidy_df %>% 
  group_by(participant, language) %>% 
  summarise(vot = mean(vot_ms), sd = sd(vot_ms), n = n())

# how many participants did not aspirate english (vot < 50)?
participant_df %>% 
  filter(vot < 50 & language == "english")

# how many aspirate French? (vot > 50)
fr_asp <- participant_df %>% 
  filter(vot > 50 & language == "french")

asp_df <- tidy_df %>% 
filter(participant %in% fr_asp$participant)

asp_df %>% 
  group_by(language) %>% 
  summarise(mean = mean(relative_vot), sd = sd(relative_vot))


desc_df <- filter_tidy_df %>% 
  group_by(language) %>% 
  summarise(mean = mean(relative_vot), sd = sd(relative_vot))

abs_df <- filter_tidy_df %>% 
  group_by(language) %>% 
  summarise(mean = mean(vot), sd = sd(vot))

length(unique(tidy_df$participant))

# fren span
## relative vot
TOSTER::TOSTpaired(n = 91, m1 = desc_df$mean[2], m2 = desc_df$mean[3], 
                   sd1 = desc_df$sd[2], sd2 = desc_df$sd[3],
                   r12 = .5, low_eqbound_dz = -.4, high_eqbound_dz = .4)

## abs vot 
TOSTER::TOSTpaired(n = 91, m1 = abs_df$mean[2], m2 = abs_df$mean[3], 
                   sd1 = abs_df$sd[2], sd2 = abs_df$sd[3],
                   r12 = .5, low_eqbound_dz = -.4, high_eqbound_dz = .4)



# eng fren
## relative vot
TOSTER::TOSTpaired(n = 91, m1 = desc_df$mean[2], m2 = desc_df$mean[1], 
                   sd1 = desc_df$sd[2], sd2 = desc_df$sd[1],
                   r12 = .5, low_eqbound_dz = -.4, high_eqbound_dz = .4)
## asb vot 
TOSTER::TOSTpaired(n = 91, m1 = abs_df$mean[2], m2 = abs_df$mean[1], 
                   sd1 = abs_df$sd[2], sd2 = abs_df$sd[1],
                   r12 = .5, low_eqbound_dz = -.4, high_eqbound_dz = .4)

# eng span
## relative vot
TOSTER::TOSTpaired(n = 75, m1 = desc_df$mean[1], m2 = desc_df$mean[3], 
                   sd1 = desc_df$sd[1], sd2 = desc_df$sd[3],
                   r12 = .5, low_eqbound_dz = -.4, high_eqbound_dz = .4)
## abs vot
TOSTER::TOSTpaired(n = 75, m1 = abs_df$mean[3], m2 = abs_df$mean[1], 
                   sd1 = abs_df$sd[3], sd2 = abs_df$sd[1],
                   r12 = .5, low_eqbound_dz = -.4, high_eqbound_dz = .4)


mod0_b <- brm(relative_vot ~ language + (1 | participant) + (1 | word), data = filter_tidy_df)

summary(mod0_b)

fixef(mod0_b)

mod0_b_df <- mod0_b %>% 
  as.data.frame()

mod0_b_df %>%
  dplyr::select(b_Intercept, b_languagefrench, b_languagespanish) %>% 
  pivot_longer(cols = everything(), names_to = "parameter", 
               values_to = "estimate") %>% 
  ggplot(., aes(x = estimate, y = parameter)) + geom_vline(xintercept = 0, lty = 3) +
  tidybayes::stat_halfeye(pch = 21, point_fill = "white", point_size = 3, 
                          .width = c(0.66, 0.95))

filter_tidy_df %>% 
  group_by(language, text) %>% 
  summarise(vot = mean(vot_ms), sd = sd(vot_ms), n = n())



  


