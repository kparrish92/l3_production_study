library(tidyverse)
library(sjPlot)
library(here)
library(lme4)

participants <- read.csv(here::here("data", "tidy", "participant_count.csv"))

l2_df <- read.csv(here::here("data", "tidy", "l2_subset.csv"))
l1_df <- read.csv(here::here("data", "tidy", "non_l2_subset.csv"))

# load models 
mod0 <- readRDS(here("data", "models", "full_model.RDS"))
l1_mod <- read_rds(here("data", "models", "l1_sub_model.RDS"))
l2_mod <- read_rds(here("data", "models", "l2_sub_model.RDS"))

all_desc_df <- read.csv(here("data", "tidy", "all_desc.csv"))
stim_df <- read.csv(here("data", "tidy", "stim.csv"))

total_participants <- sum(participants$l2_subset_no, participants$non_l2_no, participants$dqed_no)

total_participants_q <- sum(participants$l2_subset_no, participants$non_l2_no)

source(here::here("scripts", "07_small_data.R"))


all_df %>% 
  ggplot(aes(x = relative_vot, y = language)) + 
  geom_boxplot(outlier.size =  = 0) + xlim(0,.3) + 
  theme_minimal()

