
# Source libs -----------------------------------------------------------------

source(here::here("scripts", "00_libs.R"))
source(here("scripts", "01_helpers.R"))

# -----------------------------------------------------------------------------

# find all Textgrid files under "participant_uploads"
list_of_files <- list.files(path = here("data", "participant_uploads"), recursive = TRUE,
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


english_list <- c("tipping","teller","tacky","penny","pass","parrot","kitten","kennel","cabbage")
spanish_list <- c("tiro","tema","talla","quiso","queja", "cama", "piso","pena","pato")
french_list <- c("tir","terre","tasse","quitte","quelle","pile","pere","patte")


tidy_df <- df2 %>%
  mutate(file = str_remove(file, 
  "/Users/kyleparrish/Documents/GitHub/l3_production_study/data/participant_uploads/")) %>% 
  separate(file, into = c("participant", "other", sep = "/")) %>% 
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
  mutate(vot_ms = vot*1000)


tidy_df %>% 
  write.csv(here("data", "tidy", "tidy_df.csv"))


# segmented participants 

participants <- c(307935, 307937, 307938, 307939, 307942, 
                  307943, 307945, 307949, 307964, 307982,
                  307983, 307986, 307996, 308010, 308018, 
                  308427, 313303, 314727, 314744, 314747,
                  314748, 314752, 314753, 314755, 314759,
                  314760, 314761, 314763, 314767, 314768)

# check 307999

subset_df <- tidy_df %>% 
  filter(participant %in% participants) %>% 
  filter(!is.na(language))
  

subset_df %>% 
  write.csv(here("data", "tidy", "subset_df.csv"))

# box plots 
## relative vot 
subset_df %>% 
  ggplot(aes(x = relative_vot, y = language)) + geom_boxplot()

## absolute vot 
subset_df %>% 
  ggplot(aes(x = vot, y = language)) + geom_boxplot()

 
desc_df <- subset_df %>% 
  group_by(language, text) %>% 
  summarise(mean = mean(relative_vot), sd = sd(relative_vot))

unique(subset_df$participant)

abs_df <- subset_df %>% 
  group_by(language, text) %>% 
  summarise(mean = mean(vot), sd = sd(vot))



# absolute vot - small es = 11ms, med es = 20ms 
# fren span 
TOSTER::TOSTpaired(n = 75, m1 = abs_df$mean[2], m2 = abs_df$mean[1], 
                   sd1 = abs_df$sd[2], sd2 = abs_df$sd[1],
                   r12 = .5, low_eqbound_dz = -.4, high_eqbound_dz = .4)

TOSTER::TOSTpaired(n = 75, m1 = abs_df$mean[2], m2 = abs_df$mean[3], 
                   sd1 = abs_df$sd[2], sd2 = abs_df$sd[3],
                   r12 = .5, low_eqbound_dz = -.4, high_eqbound_dz = .4)

# relative vot - French/English
TOSTER::TOSTpaired(n = 25, m1 = desc_df$mean[2], m2 = desc_df$mean[1], 
                   sd1 = desc_df$sd[2], sd2 = desc_df$sd[1],
                   r12 = .5, low_eqbound_dz = -.4, high_eqbound_dz = .4)

# relative vot - French/span
TOSTER::TOSTpaired(n = 25, m1 = desc_df$mean[2], m2 = desc_df$mean[3], 
                   sd1 = desc_df$sd[2], sd2 = desc_df$sd[3],
                   r12 = .5, low_eqbound_dz = -.4, high_eqbound_dz = .4)



## re-segment - wrong textgrids assigned 
#1 307940
#2 307944
#3 307948
#4 307953
#5 308008
#6 308010
#7 308018
#8 308376
#9 308378
#10 308379
#11 308427
#12 308487
#13 307995
#14 307999
# 15 311348

# ready 
#1 
#2 
#3 
#4 
#5 
#6 
#7 
#8 
#9