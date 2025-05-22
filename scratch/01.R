
# Source libs -----------------------------------------------------------------

source(here::here("scripts", "00_libs.R"))

# -----------------------------------------------------------------------------
# rum analyses of tidy data  

sub_df <- read.csv(here("data", "tidy", "subset_df.csv"))


ind = sub_df %>% 
  filter(participant == "314864")

view(ind)

sub_df %>% 
  group_by(participant) %>% 
  summarize(n = n())

unique(sub_df$participant)

sub_df %>% 
  group_by(language) %>% 
  summarise(mean = mean(vot_ms),
            sd = sd(vot_ms))



sub_df %>% 
  group_by(participant, language) %>% 
  summarise(mean_v = mean(vot_ms),
            sd_v = sd(vot_ms)) %>% 
  group_by(language) %>% 
  summarise(mean = mean(mean_v),
            sd = sd(mean_v))


library(lmerTest)
mod = lmer(vot_ms ~ language + (language | participant) + (1 | word), data = sub_df)

summary(mod)
