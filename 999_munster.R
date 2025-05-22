#### For Muster talk 2024 - randomly sampling from the data 

# libs
library(lme4)
library(lmerTest)
library(tidyverse)
library(here)


# load data 
l2_df <- read.csv(here::here("data", "tidy", "l2_subset.csv"))
l1_df <- read.csv(here::here("data", "tidy", "non_l2_subset.csv"))
all_df <- rbind(l2_df, l1_df) 

ind_df = all_df %>% 
  group_by(text, participant, language) %>% 
  summarize(mean_vot = mean(vot_ms),
            sd_vot = sd(vot_ms)) %>% 

ind_df %>% 
  filter(!is.na(sd_vot)) %>%
  group_by(language) %>% 
  summarize(mean_sd = mean(sd_vot),
            sd_sd = sd(sd_vot))


fr_var = ind_df %>% 
  filter(!is.na(sd_vot)) %>%
  filter(language == "french")

quantile(fr_var$sd_vot, probs = c(.2, .5, .8))


ind_df %>% 
  ggplot(aes(x = language, y = mean_vot, color = text)) + geom_boxplot()

mean(!is.na(ind_df$sd_vot))

ind_df %>% 
  ggplot(aes(x = language, y = sd_vot)) + geom_boxplot()


all_df %>% 
  ggplot(aes(y = language, x = vot_ms, fill = language)) + geom_boxplot()
# new plot
ggsave(here("new", "all_df.png"))

# new model 
all_df$language = as.factor(all_df$language)
all_df$language = relevel(all_df$language, ref = "french")

model1 <- lmer(vot_ms ~ language  + (language | participant) + 
                 (1 | word), data = all_df)

summary(model1)

sjPlot::tab_model(model1)


resl = list()

for (i in 1:100) {
  

slice = all_df %>% 
  sample_n(size = 100)

slice$language = as.factor(slice$language)
slice$language = relevel(slice$language, ref = "french")

model1 <- lmer(vot_ms ~ language  + (1 | participant) + 
                 (1 | word), data = slice)

eng = ifelse(summary(model1)[["coefficients"]][2,5] < .05, 1, 0)
span = ifelse(summary(model1)[["coefficients"]][3,5] < .05, 1, 0)

resl[[i]] = data.frame(eng,span)

}

simdf = do.call(rbind,resl) %>% 
  mutate(failedrep = case_when(
         eng == 1 & span == 1 ~ "Successful Replication",
         eng == 0 & span == 1 ~ "No effect French-English",
         eng == 1 & span == 0 ~ "No effect French-Spanish",
         eng == 0 & span == 0 ~ "Neither effect")) %>%
  mutate(supports = case_when(
    eng == 1 & span == 1 ~ "Supports LPM",
    eng == 0 & span == 1 ~ "Inconclusive",
    eng == 1 & span == 0 ~ "Inconclusive",
    eng == 0 & span == 0 ~ "Neither effect"))

pdf = simdf %>% 
  group_by(failedrep, supports) %>% 
  summarize(n = n()) 


# Basic piechart
ggplot(pdf, aes(x="", y=n, fill=failedrep)) +
  scale_fill_manual(name = "", values=c("#999999", "#E69F00", "#56B4E9", "seagreen")) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  coord_polar("y", start=0) + theme_void() 
  


ggplot(pdf, aes(x="", y=n, fill=supports)) +
  scale_fill_manual(values=c("#E69F00", "#56B4E9", "#999999")) + 
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) + theme_void()


simdf %>% 
  group_by(failedrep, supports) %>% 
  summarize(n = n()) 

length(unique(l1_df$participant))
  
length(unique(l2_df$participant))


span_df_g = l1_df %>% 
  filter(language == "spanish")

fren_df_g = l1_df %>% 
  filter(language == "french")

t_TOST(fren_df_g$vot_ms, span_df_g$vot_ms,
       eqb = 16)

## Group level 

results = list()

ppt_list = unique(all_df$participant)

for (thisone in 1:length(ppt_list)) {
  
  #thisone = 3
  span_df = all_df %>% 
    filter(participant == ppt_list[thisone]) %>% 
    filter(language == "spanish")
  
  fren_df = all_df %>% 
    filter(participant == ppt_list[thisone]) %>% 
    filter(language == "french")
  eng_df = all_df %>% 
    filter(participant == ppt_list[thisone]) %>% 
    filter(language == "english")
  
  obj = t_TOST(fren_df$vot_ms, span_df$vot_ms,
               eqb = 16)[["TOST"]]
  
  obj_fr_eng = t_TOST(fren_df$vot_ms, eng_df$vot_ms,
                      eqb = 20)[["TOST"]]
  
  es_l1_l3 = t_TOST(fren_df$vot_ms, span_df$vot_ms,
         eqb = 20)
  
  es_l2_l3 = t_TOST(fren_df$vot_ms, eng_df$vot_ms,
                    eqb = 20)
  
  t_test_sig_l1_l3 = ifelse(obj[1,4] < .05, 1, 0)
  t_test_sig_l2_l3 = ifelse(obj_fr_eng[1,4] < .05, 1, 0)
  
  tost_sig_l1_l3 = ifelse(max(obj[2,4], obj[3,4]) < .05, 1, 0)
  tost_sig_l2_l3 = ifelse(max(obj_fr_eng[2,4], obj_fr_eng[3,4]) < .05, 1, 0)
  
df = data.frame(t_test_sig_l1_l3, t_test_sig_l2_l3, 
                tost_sig_l1_l3, tost_sig_l2_l3,
                ppt = thisone, 
                mean_difference_l1_l3 = es_l1_l3[["effsize"]][1,1],
                mean_difference_l2_l3 = es_l2_l3[["effsize"]][1,1])

results[[thisone]] = df 
}

compiled_data = do.call(rbind, results) %>% 
  mutate(supports = case_when(
    t_test_sig_l1_l3 == 1 & t_test_sig_l2_l3 == 1 & 
      tost_sig_l1_l3 == 0 & tost_sig_l2_l3 == 0 ~ "LPM",
      tost_sig_l1_l3 == 1 | tost_sig_l2_l3 == 1 ~ "TPM",
    t_test_sig_l1_l3 == 0 & t_test_sig_l2_l3 == 0 & 
      tost_sig_l1_l3 == 0 & tost_sig_l2_l3 == 0 ~ "Inc",
    t_test_sig_l1_l3 == 0 & t_test_sig_l2_l3 == 1 & 
      tost_sig_l1_l3 == 0 & tost_sig_l2_l3 == 0 ~ "Inc",
    t_test_sig_l1_l3 == 1 & t_test_sig_l2_l3 == 0 & 
      tost_sig_l1_l3 == 0 & tost_sig_l2_l3 == 0 ~ "Inc"))

compiled_data %>% 
  group_by(supports) %>% 
  summarize(n = n())


compiled_data %>% 
  ggplot(aes(x = mean_difference_l1_l3, 
             y = as.factor(ppt), color = supports)) + 
  geom_point() + ylab("Partcicipant number") + xlab("L1-L3 difference") +
  theme_classic() + geom_vline(xintercept = 0, linetype = "dashed")







