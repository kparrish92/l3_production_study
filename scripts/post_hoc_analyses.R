# dqed participants vs others cat of spanish 

bil_df <- temp %>% 
  mutate(group = "bilingual")

mon_df <- dis_df %>% 
  mutate(group = "monolingual")

between_groups_df <- rbind(bil_df, mon_df) %>% 
  filter(language == "spanish" | language == "french")

# dqed participants vs others cat of french  


between_groups_df %>% 
  ggplot(aes(x = relative_vot, y = language,color = group)) + geom_boxplot() + xlim(0,.3)

between_groups_df %>% 
  group_by(language, group) %>% 
  summarise(vot = mean(relative_vot), sd = sd(relative_vot))

mean_df <- between_groups_df %>% 
  group_by(language, group, participant) %>% 
  summarise(vot = mean(relative_vot), sd = sd(relative_vot))


# subset for t.test 

bil_fr <- mean_df %>% 
  filter(language == "french" & group == "bilingual")

mon_fr <- mean_df %>%
  filter(language == "french" & group == "monolingual")

bil_sp  <- mean_df %>% 
  filter(language == "spanish" & group == "bilingual")

mon_sp  <- mean_df %>% 
  filter(language == "spanish") %>% 
  filter(group == "monolingual")

# Are the french proudctions of the two groups distinct? 

t.test(bil_fr$vot, mon_fr$vot) # not quite

effsize::cohen.d(bil_fr$vot, mon_fr$vot)

# subset l2 inf 
fren_l2_mean <- l2_subset_df_fr %>% 
  group_by(language, participant) %>% 
  summarise(vot = mean(relative_vot), sd = sd(relative_vot))


t.test(fren_l2_mean$vot, mon_fr$vot) # yes for subset

effsize::cohen.d(fren_l2_mean$vot, mon_fr$vot) # large eff for subset 


### subset of full df 

subset_df2 <- temp %>% 
  group_by(language, participant) %>% 
  summarise(vot = mean(relative_vot), sd = sd(relative_vot))

eng_sdf2 <- subset_df2 %>% 
  filter(language == "english")
fr_sdf2 <- subset_df2 %>% 
  filter(language == "french")
sp_sdf2 <-subset_df2 %>% 
  filter(language == "spanish")

# are all lang pairs distinct? 
"e_s_t.test" = t.test(eng_sdf2$vot, sp_sdf2$vot) # sp eng yes
"e_f_t.test" = t.test(eng_sdf2$vot, fr_sdf2$vot) # eng fr yes
"f_s_t.test" = t.test(fr_sdf2$vot, sp_sdf2$vot) # fr sp yes

# what are the effect sizes?
"e_s_es" = effsize::cohen.d(eng_sdf2$vot, sp_sdf2$vot) # sp en = 2.7 (2.2, 3.2)
"e_f_es" = effsize::cohen.d(eng_sdf2$vot, fr_sdf2$vot) # eng fr = 1.2 (.81, 1.65)
"f_s_es" = effsize::cohen.d(sp_sdf2$vot, fr_sdf2$vot) # fr sp = -1.25 (.84, 1.67)

desc_results_full <- data.frame(t.test = c(e_s_t.test$p.value, e_f_t.test$p.value, f_s_t.test$p.value),
                           eff_size = c(e_s_es$estimate, e_f_es$estimate, f_s_es$estimate),
                           ci_low = c(e_s_es$conf.int[1], e_f_es$conf.int[1], f_s_es$conf.int[1]),
                           ci_hi = c(e_s_es$conf.int[2], e_f_es$conf.int[2], f_s_es$conf.int[2]),
                           pair = c("English-Spanish", "English-French", "French-Spanish")) %>% 
  mutate(set = "full dataset")


### subset of l2 df  

subset_df3 <- l2_subset_df %>% 
  group_by(language, participant) %>% 
  summarise(vot = mean(relative_vot), sd = sd(relative_vot))

eng_sdf3 <- subset_df3 %>% 
  filter(language == "english")
fr_sdf3 <- subset_df3 %>% 
  filter(language == "french")
sp_sdf3 <-subset_df3 %>% 
  filter(language == "spanish")

# are all lang pairs distinct? 
"e_s_t.test" = t.test(eng_sdf3$vot, sp_sdf3$vot) # sp eng yes
"e_f_t.test" = t.test(eng_sdf3$vot, fr_sdf3$vot) # eng fr yes
"f_s_t.test" = t.test(fr_sdf3$vot, sp_sdf3$vot) # fr sp yes

# what are the effect sizes?
"e_s_es" = effsize::cohen.d(eng_sdf3$vot, sp_sdf3$vot) # sp en = 2.4 (1.85, 3.1)
"e_f_es" = effsize::cohen.d(eng_sdf3$vot, fr_sdf3$vot) # eng fr = .76 (.28, 1.25)
"f_s_es" = effsize::cohen.d(sp_sdf3$vot, fr_sdf3$vot) # fr sp = -1.78 (-2.34, -1.23)

desc_results_l2 <- data.frame(t.test = c(e_s_t.test$p.value, e_f_t.test$p.value, f_s_t.test$p.value),
                                eff_size = c(e_s_es$estimate, e_f_es$estimate, f_s_es$estimate),
                                ci_low = c(e_s_es$conf.int[1], e_f_es$conf.int[1], f_s_es$conf.int[1]),
                                ci_hi = c(e_s_es$conf.int[2], e_f_es$conf.int[2], f_s_es$conf.int[2]),
                                pair = c("English-Spanish", "English-French", "French-Spanish")) %>% 
  mutate(set = "L2 subset")


# Spanish subset 


subset_df4 <- non_l2_subset_df %>% 
  group_by(language, participant) %>% 
  summarise(vot = mean(relative_vot), sd = sd(relative_vot))

eng_sdf4 <- subset_df4 %>% 
  filter(language == "english")
fr_sdf4 <- subset_df4 %>% 
  filter(language == "french")
sp_sdf4 <-subset_df4 %>% 
  filter(language == "spanish")

# are all lang pairs distinct? 
"e_s_t.test" = t.test(eng_sdf4$vot, sp_sdf4$vot) # sp eng yes
"e_f_t.test" = t.test(eng_sdf4$vot, fr_sdf4$vot) # eng fr yes
"f_s_t.test" = t.test(fr_sdf4$vot, sp_sdf4$vot) # fr sp no

# what are the effect sizes?
"e_s_es" = effsize::cohen.d(eng_sdf4$vot, sp_sdf4$vot)# sp en = 3.3 (2.3, 4.4) 
"e_f_es" = effsize::cohen.d(eng_sdf4$vot, fr_sdf4$vot) # eng fr = 2.88 (1.9, 3.8)
"f_s_es" = effsize::cohen.d(sp_sdf4$vot, fr_sdf4$vot) # fr sp = -.48 (-1.16, .2)

desc_results_l1 <- data.frame(t.test = c(e_s_t.test$p.value, e_f_t.test$p.value, f_s_t.test$p.value),
                              eff_size = c(e_s_es$estimate, e_f_es$estimate, f_s_es$estimate),
                              ci_low = c(e_s_es$conf.int[1], e_f_es$conf.int[1], f_s_es$conf.int[1]),
                              ci_hi = c(e_s_es$conf.int[2], e_f_es$conf.int[2], f_s_es$conf.int[2]),
                              pair = c("English-Spanish", "English-French", "French-Spanish")) %>% 
  mutate(set = "L1 subset")


### save from here 
all_desc <- (rbind(desc_results_l1, desc_results_l2, desc_results_full)) 

all_desc %>% 
  write.csv(here("data", "tidy", "all_desc.csv"))


all_desc %>% 
  ggplot(aes(x = eff_size, y = set, color = pair)) + geom_point() +
  geom_linerange(aes(xmin = ci_low, xmax = ci_hi)) +
  geom_vline(xintercept = 0) + geom_vline(xintercept = .4, linetype = "dashed", alpha = .5) +
  geom_vline(xintercept = -.4, linetype = "dashed", alpha = .5) +
  ggtitle("Effect sizes per dataset and language pairing")


  