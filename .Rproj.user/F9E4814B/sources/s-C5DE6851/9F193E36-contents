# Load data -------------------------------------------------------------------
#
# Last update: 2021-03-27
# Describe what this script does here.
#
# -----------------------------------------------------------------------------




# Source libs -----------------------------------------------------------------

source(here::here("scripts", "00_libs.R"))
source(here("scripts", "01_tidy2afc.R"))


# -----------------------------------------------------------------------------




# Source libs -----------------------------------------------------------------

all_2afc_long <- read_csv(here("data", "tidy", "all_2afc.csv")) %>% 
  filter(!is.na(l1)) %>% # filter out 3 particpants with no L1/L2 info
  mutate(step_std = (step_cont - mean(step_cont)) / sd(step_cont)) %>% 
  write.csv(here("data", "tidy", "all_2afc_long.csv"))
  

# Preprocessing for eLog omnibus model 
all_2afc_wide <- all_2afc_long %>% 
  group_by(., participant, exp, l1, language, step_std) %>%
  mutate(rep_n = seq_along(step_std)) %>% 
  summarise(., n = length(unique(rep_n)), 
               resp_1 = sum(response), 
               resp_0 = n - resp_1, 
               prop = resp_1 / n, 
               eLog = log((resp_1 + 0.5) / (n - resp_1 + 0.5)), 
               wts = 1 / (resp_1 + 0.5) + 1 / (n - resp_1 + 0.5)) %>% 
  mutate(., bin = seq_along(step_std)) %>% 
  ungroup(.) 

ids_needed <- readRDS(here("data", "tidy", "ids_needed.rds"))

# -----------------------------------------------------------------------------
