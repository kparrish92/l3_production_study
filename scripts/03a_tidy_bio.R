source(here::here("scripts", "00_libs.R"))

tidy_df <- read.csv(here("data", "tidy", "subset_df.csv"))


bio_data <- read.csv(here::here("data", "tidy", "bio.csv")) %>% 
  filter(!is.na(prolific_id)) %>% 
  filter(!is.na(current_age)) %>% 
  select(age, country_now, country_origin, current_age, Do.you.speak.a.third.language., L2,
         l3, language, self_prof, sex, completed, exp_subject_id, rec_session_id, prolific_id) %>% 
  rename(participant = rec_session_id) %>% 
  rename("l1" = language)

# monolingual data 
bio_data_mono <- read.csv(here::here("data", "tidy", "mono_bio_data.csv")) %>% 
  filter(!is.na(prolific_id)) %>% 
  filter(!is.na(current_age)) %>% 
  select(country_now, country_origin, eng, completed, 
         exp_subject_id, rec_session_id, prolific_id) %>% 
  rename(participant = rec_session_id) 

# how many answered `no` to `do you speak English` & completed the exp?

nrow(bio_data %>% filter(eng == "no" & completed == "yes"))

bio_data %>% filter(!eng == "no") 


#### some participants somehow did the experiment twice

unique(bio_data$prolific_id)

tidy_bio <- bio_data %>% 
  filter(completed == "yes") %>% 
  filter(Do.you.speak.a.third.language. == "no")


tidy_df_bio <- left_join(bio_data, tidy_df, by = "participant")


tidy_bio %>% 
  write.csv(here("data", "tidy", "tidy_bio.csv"))

length(unique(tidy_df_bio$participant))

# what is the prof per group subset and aoa? 
# make chart and plot 