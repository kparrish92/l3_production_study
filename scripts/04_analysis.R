
# Source libs -----------------------------------------------------------------

source(here::here("scripts", "00_libs.R"))
source(here("scripts", "01_helpers.R"))

# -----------------------------------------------------------------------------
# rum analyses of tidy data  

sub_df <- read.csv(here("data", "tidy", "subset_df.csv")) %>% 
  mutate("vowel" = case_when(
    word %in% a_list ~ "a",
    word %in% e_list ~ "e",
    word %in% i_list ~ "i"
  ))

bio_df <- read.csv(here("data", "tidy", "tidy_bio.csv"))


## loop function to run t.tests on productions of fren/eng per participant 

participant_df <- unique(sub_df$participant)
values_fr_eng <- matrix(nrow = length(participant_df), ncol = 2)
values_fr_span <- matrix(nrow = length(participant_df), ncol = 2)
values_eng_span <- matrix(nrow = length(participant_df), ncol = 2)

for (thisRun in 1:length(participant_df)) {
  values_fr_eng[thisRun, 1] = fren_eng_t(sub_df, participant_no = participant_df[thisRun])  
  values_fr_eng[thisRun, 2] = participant_df[thisRun]  
}

for (thisRun in 1:length(participant_df)) {
  values_fr_span[thisRun, 1] = fren_span_t(sub_df, participant_no = participant_df[thisRun])  
  values_fr_span[thisRun, 2] = participant_df[thisRun]  
}

for (thisRun in 1:length(participant_df)) {
  values_eng_span[thisRun, 1] = eng_span_t(sub_df, participant_no = participant_df[thisRun])  
  values_eng_span[thisRun, 2] = participant_df[thisRun]  
}

# combine value dataframes and combine with sub_df

values_fr_eng <- as.data.frame(values_fr_eng) %>% 
  rename(fr_eng_p = V1, participant = V2)
values_eng_span <- as.data.frame(values_eng_span) %>% 
  rename(sp_eng_p = V1, participant = V2)
values_fr_span <- as.data.frame(values_fr_span) %>% 
  rename(fr_sp_p = V1, participant = V2)

# filter from subdf those who are eligible based on bio data 

tidy_df <- left_join(sub_df, values_eng_span, by = "participant") %>% 
  left_join(., values_fr_span, by = "participant") %>% 
  left_join(., values_fr_eng, by = "participant")


tidy_df_dis <- left_join(sub_df, values_eng_span, by = "participant") %>% 
  left_join(., values_fr_span, by = "participant") %>% 
  left_join(., values_fr_eng, by = "participant")

tidy_df_bio <- tidy_df %>% 
  filter(participant %in% bio_df$participant)
  
# How many participants categorized English and Spanish differently? 


### duplicates
duplicates <- c(314848, 314847, 314792, 314746, 314746, 
                314765, 314756, 314841, 314804, 314759, 
                314822, 314777, 307996, 307994, 307991, 
                307989, 307946, 308378, 308376)

eligible_df <- tidy_df_bio %>% 
  filter(sp_eng_p < .05) %>% 
  filter(!participant %in% duplicates)
  
# duplicates 

length(unique(eligible_df$participant)) # was 42 <- # 39 now 


# standardize variables

eligible_df <- eligible_df %>% 
  mutate("relative_vot_z" = (relative_vot - mean(relative_vot))/sd(relative_vot))

# disclused participants due to not producing different spanish and english vot 

dis_df <- tidy_df_dis %>% 
  filter(!sp_eng_p < .05) %>% 
  mutate("vowel" = case_when(
    word %in% a_list ~ "a",
    word %in% e_list ~ "e",
    word %in% i_list ~ "i"
  ))

length(unique(dis_df$participant))

dis_df %>% 
  write.csv(here("data", "tidy", "dis_df.csv"))

# How many participants categorized Spanish and French differently?
temp_2 <- eligible_df %>% 
  filter(fr_sp_p < .05)

length(unique(temp_2$participant))


# How many eligible_df categorized English and French differently?
temp_3 <- eligible_df %>% 
  filter(fr_eng_p < .05)

length(unique(temp_3$participant))





# tost of full dataset


tidy_df_fr <- eligible_df %>% 
  filter(language == "french")

tidy_df_en <- eligible_df %>% 
  filter(language == "english")


TOSTER::TOSTpaired(n = length(unique(tidy_df_fr$participant)), 
                   m1 = mean(tidy_df_fr$relative_vot), 
                   m2 = mean(tidy_df_en$relative_vot), 
                   sd1 = sd(tidy_df_fr$relative_vot), 
                   sd2 = sd(tidy_df_en$relative_vot),
                   r12 = .44, # use mean df to find cor
                   low_eqbound_dz = -.4, 
                   high_eqbound_dz = .4)


# How many participants categorized English and Spanish
# but French and English the same(ish)? (L2 status evidence)

temp_4 <- eligible_df %>% 
  filter(fr_eng_p > .05) %>%  
  filter(sp_eng_p < .05)

length(unique(temp_4$participant))

l2_subset <- unique(temp_4$participant)

l2_subset_df <- eligible_df %>% 
  filter(participant %in% l2_subset)

l2_subset_df_fr <- l2_subset_df %>% 
  filter(language == "french")

l2_subset_df_en <- l2_subset_df %>% 
  filter(language == "english")



# all 3 different 

temp_5 <- tidy_df %>% 
  filter(fr_eng_p < .05) 

length(unique(temp_5$participant))

# TOST to see whether a subset of participants are practically equivalent with
# eng and french 

TOSTER::TOSTpaired(n = length(unique(non_l2_subset_df$participant)), 
                   m1 = mean(non_l2_subset_df_fr$relative_vot), 
                   m2 = mean(non_l2_subset_df_sp$relative_vot), 
                   sd1 = sd(non_l2_subset_df_fr$relative_vot), 
                   sd2 = sd(non_l2_subset_df_sp$relative_vot),
                   r12 = .44, # use mean df to find cor
                   low_eqbound_dz = -.4, 
                   high_eqbound_dz = .4)

TOSTER::TOSTpaired(n = length(unique(l2_subset_df$participant)), 
                   m1 = mean(l2_subset_df_fr$relative_vot), 
                   m2 = mean(l2_subset_df_en$relative_vot), 
                   sd1 = sd(l2_subset_df_fr$relative_vot), 
                   sd2 = sd(l2_subset_df_en$relative_vot),
                   r12 = .44, # use mean df to find cor 
                   low_eqbound_dz = -.4, 
                   high_eqbound_dz = .4)

# non-l2 subset 

non_l2_subset_df <- eligible_df %>% 
  filter(!participant %in% l2_subset)

non_l2_subset_df_fr <- non_l2_subset_df %>% 
  filter(language == "french")

non_l2_subset_df_en <- non_l2_subset_df %>% 
  filter(language == "english")

non_l2_subset_df_sp <- non_l2_subset_df %>% 
  filter(language == "spanish")

TOSTER::TOSTpaired(n = length(unique(non_l2_subset_df$participant)), 
                   m1 = mean(non_l2_subset_df_fr$relative_vot), 
                   m2 = mean(non_l2_subset_df_sp$relative_vot), 
                   sd1 = sd(non_l2_subset_df_fr$relative_vot), 
                   sd2 = sd(non_l2_subset_df_sp$relative_vot),
                   r12 = .44, # use mean df to find cor
                   low_eqbound_dz = -.4, 
                   high_eqbound_dz = .4)


TOSTER::TOSTpaired(n = 30, 
                   m1 = mean(non_l2_subset_df_fr$relative_vot), 
                   m2 = mean(non_l2_subset_df_sp$relative_vot), 
                   sd1 = sd(non_l2_subset_df_fr$relative_vot), 
                   sd2 = sd(non_l2_subset_df_sp$relative_vot),
                   r12 = .44, # use mean df to find cor
                   low_eqbound_dz = -.4, 
                   high_eqbound_dz = .4)



# boxplot of L2 subset 

l2_subset_df %>% 
  ggplot(aes(x = relative_vot, y = language)) + geom_boxplot()

l2_subset_df %>% 
  ggplot(aes(x = relative_vot, y = language, color = text)) + geom_boxplot()

no_t_l2 <- l2_subset_df %>% 
  filter(!text == "t") %>% 
  ggplot(aes(x = relative_vot, y = language)) + geom_boxplot()


# box plot of spanish like l3ers 
non_l2_subset_df %>% 
  ggplot(aes(x = relative_vot, y = language)) + geom_boxplot()

# per consonatn 

non_l2_subset_df %>% 
  ggplot(aes(x = relative_vot, y = language, color = text)) + geom_boxplot()

# boxplot of dqed participants 
dis_df %>% 
  ggplot(aes(x = relative_vot, y = language)) + geom_boxplot()



# GLMM 
mod0 <- lmer(relative_vot_z ~ language + text + (0 + language | participant) + (1 | word), data = eligible_df)
summary(mod0)

fixef(mod0)

mod1 <- lmer(relative_vot_z ~ language + text + (0 + language | participant) + (1 | word), data = l2_subset_df)
summary(mod1)

mod2 <- lmer(relative_vot_z ~ language + text + (0 + language | participant) + (1 | word), data = non_l2_subset_df)
summary(mod2)


# Bayesian 
#mod0_b <- brm(relative_vot ~ language*text + (1 | participant) + (1 | word), data = sub_df)

#fixef(mod0_b)

# subset bayesian models 
#mod1_b <- brm(relative_vot ~ language*text + (1 | participant) + (1 | word), data = l2_subset_df)
#mod2_b <- brm(relative_vot ~ language*text + (1 | participant) + (1 | word), data = non_l2_subset_df)

#fixef(mod1_b)

#fixef(mod2_b)


#mod0_b_df <- mod0_b %>% 
  #as.data.frame()

#mod0_b_df %>%
 # dplyr::select(b_Intercept, b_languagefrench, b_languagespanish,
  #              b_textp, b_textt) %>% 
  #pivot_longer(cols = everything(), names_to = "parameter", 
   #            values_to = "estimate") %>% 
#ggplot(., aes(x = estimate, y = parameter)) + geom_vline(xintercept = 0, lty = 3) +
#  tidybayes::stat_halfeye(pch = 21, point_fill = "white", point_size = 3, 
                         # .width = c(0.66, 0.95))


#mod0_b_df %>%
#  dplyr::select(b_Intercept, b_languagefrench, b_languageenglish,
#                b_textp, b_textt) %>% 
#  pivot_longer(cols = everything(), names_to = "parameter", 
#               values_to = "estimate") %>% 
#  ggplot(., aes(x = estimate, y = parameter)) + geom_vline(xintercept = 0, lty = 3) +
#  tidybayes::stat_halfeye(pch = 21, point_fill = "white", point_size = 3, 
#                          .width = c(0.66, 0.95))




#mean(mod0_b_df$b_languagefrench)
#sd(mod0_b_df$b_languagefrench)

#mean(mod0_b_df$b_languageenglish)
#sd(mod0_b_df$b_languageenglish)


TOSTER::TOSTpaired(n = 40, 
                   m1 = mean(mod0_b_df$b_languagefrench), 
                   m2 = mean(mod0_b_df$b_languageenglish), 
                   sd1 = sd(mod0_b_df$b_languagefrench), 
                   sd2 = sd(mod0_b_df$b_languageenglish),
                   r12 = cor(mod0_b_df$b_languageenglish,
                             mod0_b_df$b_languagefrench), 
                   low_eqbound_d = -.4, 
                   high_eqbound_d = .4)

  

#mod2_b_df <- mod2_b %>% 
#  as.data.frame()

#mod2_b_df %>% 
#dplyr::select(b_Intercept, b_languagefrench, b_languagespanish,
#           b_textp, b_textt) %>% 
  # pivot_longer(cols = everything(), names_to = "parameter", 
#              values_to = "estimate") %>% 
# ggplot(., aes(x = estimate, y = parameter)) + geom_vline(xintercept = 0, lty = 3) +
# tidybayes::stat_halfeye(pch = 21, point_fill = "white", point_size = 3, 
                          .width = c(0.66, 0.95))

###save 


participant_count_df <- data.frame(l2_subset_no = length(unique(temp_4$participant)),
non_l2_no = length(unique(non_l2_subset_df$participant)),
dqed_no = length(unique(dis_df$participant))) 

participant_count_df %>% 
  write.csv(here("data", "tidy", "participant_count.csv"))

l2_subset_df %>% 
  write.csv(here("data", "tidy", "l2_subset.csv"))

non_l2_subset_df %>% 
  write.csv(here("data", "tidy", "non_l2_subset.csv"))

l2_subset_df %>% 
  ggplot(aes(x = relative_vot, y = language)) + geom_boxplot()

non_l2_subset_df %>% 
  ggplot(aes(x = relative_vot, y = language)) + geom_boxplot()


mod0 %>% 
  write_rds(here("data", "models", "full_model.RDS"))

  
mod1 %>% 
  write_rds(here("data", "models", "l2_sub_model.RDS"))


mod2 %>% 
  write_rds(here("data", "models", "l1_sub_model.RDS"))

# relevel
#non_l2_subset_df$language <- as.factor(non_l2_subset_df$language)
#non_l2_subset_df$language <- relevel(non_l2_subset_df$language, ref = "french")
#mod2 <- lmer(relative_vot ~ language + text + (1 | participant) + (1 | word), data = non_l2_subset_df)
#summary(mod2)

#mod2 %>% 
 # write_rds(here("data", "models", "l1_sub_model.RDS"))


