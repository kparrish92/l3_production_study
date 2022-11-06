
library(tidyverse)
library(here)

l2_df <- read.csv(here::here("data", "tidy", "l2_subset.csv"))
l1_df <- read.csv(here::here("data", "tidy", "non_l2_subset.csv"))

all_df <- rbind(l1_df, l2_df)


all_df %>% 
  group_by(text, language) %>% 
  summarize(mean_vot = mean(vot),
            sd_vot = sd(vot),
            min_vot = min(vot),
            max_vot = max(vot))

# Fit model to get credible interval for plotting
lextale_mod <- brm(
  score ~ 1, data = lextale_scored_data, 
  control = list(adapt_delta = 0.99, max_treedepth = 15), 
  warmup = 1000, iter = 2000, chains = 4, cores = 4, 
  prior = lex_priors, 
  file = here("data", "models", "mod_lextale")
)

all_df %>% 
  ggplot(aes(x = vot, fill = language)) + geom_density(alpha = .5)

all_df %>% 
  ggplot(aes(x = vot, y = as.factor(participant), color = language)) + 
  geom_point()


all_df %>% 
  filter(language == "french") %>% 
  ggplot(aes(x = vot, y = as.factor(participant), color = language)) + 
  geom_point()




all_df %>% 
  ggplot(aes(x = relative_vot, fill = language)) + geom(alpha = .5) + 
  facet_wrap(~ participant)

all_df %>% 
  ggplot(aes(x = relative_vot, y = language)) + geom_boxplot() + 
  facet_wrap(~ participant)

all_df %>% 
  ggplot(aes(x = relative_vot, y = language)) + geom_boxplot() 

l2_df %>% 
  ggplot(aes(x = relative_vot, fill = language)) + geom_density(alpha = .5) + 
  facet_wrap(~ participant)


all_df %>% 
  group_by(participant, language, text) %>%
  summarise(vot_m = mean(vot), sd = sd(vot)) %>% 
  ggplot(aes(x = vot_m, fill = language)) + geom_density(alpha = .5) + 
  facet_grid(~text)

all_df %>% 
  group_by(participant, language, text) %>%
  summarise(vot_m = mean(vot), sd = sd(vot)) %>% 
  ggplot(aes(x = vot_m*1000, y = language)) + geom_boxplot() +  
  facet_grid(~text)

all_df %>% 
  filter(language == "french") %>% 
  ggplot(aes(x = vot*1000, y = as.factor(participant))) + geom_point() 
  geom_rect(aes(xmin=0, xmax=40, ymin=0, ymax=Inf, color = "blue"), 
            alpha = .01) +
  geom_rect(aes(xmin=65, xmax=155, ymin=0, ymax=Inf, color = "blue"), 
            alpha = .01) + 
    facet_grid(~ text)

  
  
all_df %>% 
    filter(language == "french") %>%
    ggplot(aes(x = vot*1000)) + geom_density(fill = "seagreen") + 
  facet_grid(~ text) + xlim(0, 150)


all_df %>% 
  filter(language == "english") %>%
  ggplot(aes(x = vot*1000)) + geom_density(fill = "skyblue") + 
  facet_grid(~ text) + xlim(0, 150)

all_df %>% 
  filter(language == "spanish") %>%
  ggplot(aes(x = vot*1000)) + geom_density(fill = "orange") + 
  facet_grid(~ text) + xlim(0, 150)



all_df %>% 
  mutate(language = case_when(
    language == "english" ~ "L2 English",
    language == "french" ~ "L3 French",
    language == "spanish" ~ "L1 Spanish"
  )) %>% 
  ggplot(aes(x = relative_vot, fill = language)) + geom_density(alpha = .7) + 
  facet_grid(~ text) + xlab("Relative Voice-onset Time") + 
  ggsave(here("docs", "plots", "posthoc2.png"), dpi = 300)
  

all_df %>% 
  mutate(language = case_when(
    language == "english" ~ "L2 English",
    language == "french" ~ "L3 French",
    language == "spanish" ~ "L1 Spanish"
  )) %>% 
  ggplot(aes(x = vot*1000, fill = language)) + geom_density(alpha = .7) + 
  facet_grid(~ text) + xlab("Voice-onset Time") + 
  ggsave(here("docs", "plots", "posthoc1.png"), dpi = 300)


all_df %>% 
  ggplot(aes(x = vot*1000, color = language)) + geom_histogram(alpha = .7) + 
  facet_grid(~ text) + xlab("Voice-onset Time")

  
all_df %>% 
  group_by(language) %>%
  summarise(vot_m = mean(vot), sd = sd(vot))


all_df %>% 
  group_by(language, text) %>%
  summarise(rvot_m = mean(relative_vot), sd = sd(vot))


all_df %>% 
  mutate(language = case_when(
    language == "english" ~ "L2 English",
    language == "french" ~ "L3 French",
    language == "spanish" ~ "L1 Spanish"
  )) %>%
  ggplot(aes(x = vot*1000, y = language)) + geom_point(alpha = .25) +
  facet_grid(~ text) + xlab("Voice-onset Time") + xlim(0,150)
  ggsave(here("docs", "plots", "posthoc1.png"), dpi = 300)
  