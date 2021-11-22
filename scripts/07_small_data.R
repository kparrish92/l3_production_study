# Source libs -----------------------------------------------------------------
# data used in slides and manuscripts

source(here::here("scripts", "00_libs.R"))
source(here("scripts", "01_helpers.R"))


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


all_df <- rbind(l1_df, l2_df)
all_part <- length(unique(all_df$participant))

temp_4 <- all_df %>% 
  filter(fr_eng_p > .05) %>%  
  filter(sp_eng_p < .05)

l2_subset <- unique(temp_4$participant)


non_l2_subset_df_fr <- l1_df %>% 
  filter(language == "french")
non_l2_subset_df_sp <- l1_df %>% 
  filter(language == "spanish")

bio_data <- read.csv(here("data", "tidy", "tidy_bio.csv"))

# load participants 
participants <- read.csv(here::here("data", "tidy", "participant_count.csv"))

# load data 
l1_df <- read.csv(here::here("data", "tidy", "non_l2_subset.csv"))
l2_df <- read.csv(here::here("data", "tidy", "l2_subset.csv"))
tidy_df <- rbind(l1_df, l2_df)
all_desc_df <- read.csv(here("data", "tidy", "all_desc.csv"))

# load models
mod0 <- readRDS(here("data", "models", "full_model.RDS"))
model1 <- readRDS(here("data", "models", "l2_sub_model.RDS"))
model2 <- readRDS(here("data", "models", "l1_sub_model.RDS"))

# load nested model comps 
nmc_f <- read.csv(here("data", "tidy", "nmc_full.csv"))
nmc_l1 <- read.csv(here("data", "tidy", "nmc_l1.csv"))
nmc_l2 <- read.csv(here("data", "tidy", "nmc_l2.csv"))

# load r_squared df 
r_dq_df <- read.csv(here("data", "tidy", "r_sq_df.csv"))

fm_2rm <- r_dq_df$R2m[1] %>% round(digits = 3)*100 # model full r2m
l2_r2m <- r_dq_df$R2m[2] %>% round(digits = 3)*100 # model l2 r2m
l1_r2m <- r_dq_df$R2m[3] %>% round(digits = 3)*100 # model l1 r2m

fm_2rc <- r_dq_df$R2c[1] %>% round(digits = 3)*100 # model full r2c
l2_r2c <- r_dq_df$R2c[2] %>% round(digits = 3)*100 # model l2 r2c
l1_r2c <- r_dq_df$R2c[3] %>% round(digits = 3)*100 # model l1 r2c

# make fixed effects a df 
mod0_fix_ef <- fixef(mod0) %>% 
  as.data.frame() # full df 

mod1_fix_ef <- fixef(model1) %>% 
  as.data.frame() # L2 subset 

mod2_fix_ef <- fixef(model2) %>% 
  as.data.frame() # L1 subset 


# full model effects 
full_fr <- mod0_fix_ef$.[2] %>% round(digit = 2) # fr
full_sp <- mod0_fix_ef$.[3] %>% round(digit = 2) # sp

# l2 effects 
l2_fr <- mod1_fix_ef$.[2] %>% round(digit = 2) # fr
l2_sp <- mod1_fix_ef$.[3] %>% round(digit = 2) # sp

# l1 effects 
l1_fr <- mod2_fix_ef$.[2] %>% round(digit = 2) # fr
l1_sp <- mod2_fix_ef$.[3] %>% round(digit = 2) # sp

participants <- read.csv(here::here("data", "tidy", "participant_count.csv"))
total_participants <- sum(participants$l2_subset_no, participants$non_l2_no, participants$dqed_no)

total_participants_q <- sum(participants$l2_subset_no, participants$non_l2_no)

stim_df <- read.csv(here("data", "tidy", "stim.csv"))

power_df <- read.csv(here("data", "tidy", "powerdf.csv"))

dis_df <- read.csv(here("data", "tidy", "dis_df.csv"))

mono_data <- read.csv(here("data", "tidy", "mono_df.csv"))

mono_df <- mono_data %>% 
  group_by(language) %>% 
  summarise(mean = mean(relative_vot), sd = sd(relative_vot))

