## nested model comparisons to find the best fitting models per subset

# libs
library(lme4)
library(lmerTest)
library(tidyverse)
library(here)


# load data 
l2_df <- read.csv(here::here("data", "tidy", "l2_subset.csv"))
l1_df <- read.csv(here::here("data", "tidy", "non_l2_subset.csv"))
all_df <- rbind(l2_df, l1_df)


# all data - model 2 is the best fit 
int_model <- lmer(relative_vot_z ~ 1 + (language | participant) + (1 | word), data = all_df)
model1 <- lmer(relative_vot_z ~ language  + (language | participant) + (1 | word), data = all_df)
model2 <- lmer(relative_vot_z ~ language + text + (language | participant) + (1 | word), data = all_df)
model3 <- lmer(relative_vot_z ~ language*text + (language | participant) + (1 | word), data = all_df)

# assign to object
nested_full <- anova(int_model, model1, model2, model3) 
# save output
nested_full %>% 
  write.csv(here("data", "tidy", "nmc_full.csv"))

#  L2 - model 2 is the best fit again
int_model <- lmer(relative_vot_z ~ 1 + (language | participant) + (1 | word), data = l2_df)
model1 <- lmer(relative_vot_z ~ language  + (language | participant) + (1 | word), data = l2_df)
model2 <- lmer(relative_vot_z ~ language + text + (language | participant) + (1 | word), data = l2_df)
model3 <- lmer(relative_vot_z ~ language*text + (language | participant) + (1 | word), data = l2_df)

summary(model3)

# assign to object
nested_l2 <- anova(int_model, model1, model2, model3) 
# save output
nested_l2 %>% 
  write.csv(here("data", "tidy", "nmc_l2.csv"))


#  L1 - model 2 is the best fit again
int_model <- lmer(relative_vot_z ~ 1 + (language | participant) + (1 | word), data = l1_df)
model1 <- lmer(relative_vot_z ~ language  + (language | participant) + (1 | word), data = l1_df)
model2 <- lmer(relative_vot_z ~ language + text + (language | participant) + (1 | word), data = l1_df)
model3 <- lmer(relative_vot_z ~ language*text + (language | participant) + (1 | word), data = l1_df)

summary(model2)

# assign to object
nested_l1 <- anova(int_model, model1, model2, model3) 
# save output
nested_l1 %>% 
  write.csv(here("data", "tidy", "nmc_l1.csv"))

