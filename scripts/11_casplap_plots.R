library(tidyverse)
library(here)
library(lme4)

l1_df <- read.csv(here::here("data", "tidy", "non_l2_subset.csv"))
l2_df <- read.csv(here::here("data", "tidy", "l2_subset.csv"))
tidy_df <- rbind(l1_df, l2_df) %>% mutate(group = "bilingual") %>% 
  select(participant, text, word, duration, vot, relative_vot, language, group)
mono_df <- read.csv(here("data", "tidy", "mono_df.csv")) %>% 
  mutate(group = "monolingual") %>% 
  select(participant, text, word, duration, vot, relative_vot, language, group)


comb_df <- rbind(tidy_df, mono_df)


# full dataset of bilingual participants  
tidy_df %>%
  ggplot(aes(x = relative_vot, y = language, fill = language)) + 
  geom_boxplot(color = "black", outlier.size = 0) +
  xlim(0, .3) +
  scale_fill_manual(values=c("#90ff82", "#0dbaff", "#fa6652"))  + 
  xlab("Relative VOT") + ylab("Language") +
  theme_bw() +
  theme(panel.background = element_rect(fill = "grey79"),
        legend.position = "bottom") + theme(legend.position = "none") + 
  ggtitle("Relative VOT per language in bilinguals") + 
  ggsave(here("slides", "images", "full.png"))

# monolinguals 
mono_df %>%
  ggplot(aes(x = relative_vot, y = language, fill = language)) + 
  geom_boxplot(color = "black", outlier.size = 0) +
  xlim(0, .3) +
  scale_fill_manual(values=c("#0dbaff", "#fa6652")) + 
  xlab("Relative VOT") + ylab("Language") +
  theme_bw() +
  theme(panel.background = element_rect(fill = "grey79"),
        legend.position = "none") + 
  ggtitle("Relative VOT per language in monolinguals") + 
  ggsave(here("slides", "images", "mono.png"))

# mono and bilingual comparison 

comb_df %>%
  ggplot(aes(x = relative_vot, y = group, fill = language)) + 
  geom_boxplot(outlier.size = 0) +
  xlim(0, .3) +
  scale_fill_manual(values=c("#90ff82", "#0dbaff", "#fa6652")) + 
  xlab("Relative VOT") + ylab("Group") +
  theme_bw() +
  theme(panel.background = element_rect(fill = "grey79"),
        legend.position = "bottom") + 
  ggtitle("Relative VOT of each language per group") + 
  ggsave(here("slides", "images", "comb.png"))



l1_df %>% 
  ggplot(aes(x = relative_vot, y = language, fill = language)) + 
  geom_boxplot(outlier.size = 0) +
  xlim(0, .3) +
  scale_fill_manual(values=c("#90ff82", "#0dbaff", "#fa6652")) + 
  xlab("Relative VOT") + ylab("Language") +
  theme_bw() +
  theme(panel.background = element_rect(fill = "grey79"),
        legend.position = "none") + 
  ggtitle("Relative VOT of each language in L1 subset") + 
  ggsave(here("slides", "images", "l1_subset.png")) 

l2_df %>% 
  ggplot(aes(x = relative_vot, y = language, fill = language)) + 
  geom_boxplot(outlier.size = 0) +
  xlim(0, .3) +
  scale_fill_manual(values=c("#90ff82", "#0dbaff", "#fa6652")) + 
  xlab("Relative VOT") + ylab("Language") +
  theme_bw() +
  theme(panel.background = element_rect(fill = "grey79"),
        legend.position = "none") + 
  ggtitle("Relative VOT of each language in L2 subset") + 
  ggsave(here("slides", "images", "l2_subset.png")) 




# comb_df analysis 

modn = lmer(relative_vot ~ 1 + (1 | participant), data = comb_df)
mod0 = lmer(relative_vot ~ group + (1 | participant), data = comb_df)
mod1 = lmer(relative_vot ~ group + language + (1 | participant), data = comb_df)
mod2 = lmer(relative_vot ~ group*language + (1 | participant), data = comb_df)

anova(modn, mod0, mod1, mod2)

summary(mod2)

mod2 %>% 
  write_rds(here("data", "models", "full_model_casp.rds"))

# are the L1 subset French and monolingual subset production equivalent? 

l1_df_fr = l1_df %>% 
  filter(language == "french")

mono_df_fr = mono_df %>% 
  filter(language == "french")


TOSTER::TOSTtwo(m1 = mean(l1_df_fr$relative_vot), 
                m2 = mean(mono_df_fr$relative_vot), 
                sd1 = sd(l1_df_fr$relative_vot), 
                sd2 = sd(mono_df_fr$relative_vot), 
                n1 = nrow(l1_df_fr), 
                n2 = nrow(mono_df_fr), 
                low_eqbound_d = -.4,
                high_eqbound_d = .4)

