# Tidy 2afc task --------------------------------------------------------------
#
# Last update: 2021-03-27
# Describe what this script does here.
#
# -----------------------------------------------------------------------------





# Source libs -----------------------------------------------------------------

source(here::here("scripts", "00_libs.R"))

# -----------------------------------------------------------------------------




# Helper functions ------------------------------------------------------------

# This is the tidying pipeline that you essentially repeat for every 
# language dataset. 
# Since you are going to be repeating it over and over again, it makes 
# sense to turn it into a function. 

tidy_2afc <- . %>% 
  map_dfr(read_csv, .id = "source", col_types = cols(.default = "c")) %>% 
  dplyr::select(step_cont = stimuli, participant, date, expName, resp_2.keys, resp_3.keys) %>% 
  mutate(exp = if_else(is.na(resp_2.keys), "stops", "vowels"), 
         step_cont = str_remove(step_cont, "stim/VC_"), 
         step_cont = str_remove(step_cont, "stim2/pa"), 
         step_cont = str_remove(step_cont, "\\+"), 
         step_cont = str_remove(step_cont, ".wav"), 
         step_cont = as.numeric(step_cont), 
         response = case_when(
           resp_2.keys == "left"  ~ 0, 
           resp_2.keys == "right" ~ 1, 
           resp_3.keys == "left"  ~ 1, 
           resp_3.keys == "right" ~ 0)) %>% 
  filter(nchar(as.character(participant)) == 24)

# -----------------------------------------------------------------------------




# Import data -----------------------------------------------------------------

en <- dir_ls(here("data", "raw", "english_2afc"), regexp = "\\.csv$") %>% 
  tidy_2afc() %>% 
  mutate(language = "english")

sp <- dir_ls(here("data", "raw", "spanish_2afc"), regexp = "\\.csv$") %>% 
  tidy_2afc() %>% 
  mutate(language = "spanish")

fr <- dir_ls(here("data", "raw", "french_2afc"), regexp = "\\.csv$") %>%
  tidy_2afc() %>% 
  mutate(language = "french")

hu <- dir_ls(here("data", "raw", "hungarian_2afc"), regexp = "\\.csv$") %>% 
  tidy_2afc() %>% 
  mutate(language = "hungarian") 

# -----------------------------------------------------------------------------




# Get completion details ------------------------------------------------------

# Unique IDs from highest to lowest completion rate
language_n <- tibble(
  en = en$participant %>% unique %>% length, 
  sp = sp$participant %>% unique %>% length, 
  hu = hu$participant %>% unique %>% length, 
  fr = fr$participant %>% unique %>% length) %>% 
  pivot_longer(cols = everything(), names_to = "language", values_to = "n")

# Put IDs in a list for filtering
language_ids <- list(
  en = en$participant %>% unique, 
  sp = sp$participant %>% unique, 
  hu = hu$participant %>% unique, 
  fr = fr$participant %>% unique
  ) 

# First we get IDs of participants that completed Spanish and English (RQ0)
en_sp_complete_cases <- en %>% 
  filter(participant %in% language_ids$sp) %>% 
  pull(participant) %>% unique

# For RQs 1 and 2 we need participants that did all 3 tasks (sp & en, fr | hu)
# Get fr finishers
all_3_fr <- en_sp_complete_cases %>% 
  as_tibble() %>% 
  filter(value %in% language_ids$fr) %>% 
  pull

# Get hu finishers
all_3_hu <- en_sp_complete_cases %>% 
  as_tibble() %>% 
  filter(value %in% language_ids$hu) %>% 
  pull

attrition <- tibble(
  combos = c("en_sp", "en_sp_fr", "en_sp_hu"), 
  possible = c(length(language_ids$en), length(language_ids$fr), length(language_ids$hu)),
  actual = c(length(en_sp_complete_cases), length(all_3_fr), length(all_3_hu)), 
  attrition = possible - actual
)

# Make list of all participants needed, and subsets for specific RQs 
# Will use this below to filter data
ids_needed <- list(
  all = c(en_sp_complete_cases, all_3_fr, all_3_hu), 
  en_sp = en_sp_complete_cases, 
  en_sp_fr = all_3_fr, 
  en_sp_hu = all_3_hu
)

saveRDS(ids_needed, here("data", "tidy", "ids_needed.rds"))

# -----------------------------------------------------------------------------




# Get missing info ------------------------------------------------------------

# We need L1/L2 info from the language questionnaire

lhq_temp <- bind_rows(
  read_csv(here("data", "raw", "lhq", "LHQ 3.0 raw result (1).csv")) %>% 
    dplyr::select(l1 = 9, l2 = 15, participant = 1, prolific = 2),
  read_csv(here("data", "raw", "lhq", "LHQ 3.0 raw result (2).csv")) %>% 
    dplyr::select(l1 = 9, l2 = 15, participant = 1, prolific = 2),
  read_csv(here("data", "raw", "lhq", "LHQ 3.0 raw result (3).csv")) %>% 
    dplyr::select(l1 = 9, l2 = 15, participant = 1, prolific = 2),
  read_csv(here("data", "raw", "lhq", "LHQ 3.0 raw result (5).csv")) %>% 
    dplyr::select(l1 = 9, l2 = 15, participant = 1, prolific = 2),
  read_csv(here("data", "raw", "lhq", "LHQ 3.0 raw result (6).csv")) %>% 
    dplyr::select(l1 = 9, l2 = 15, participant = 1, prolific = 2)) 


# to isolate prolific ids 
lhq_main <- lhq_temp %>% 
  filter(nchar(as.character(participant)) == 24)

# Filter to get participants with 5 char IDs (
lhq_sub <- lhq_temp %>% 
  filter(nchar(as.character(participant)) == 5)


# Read csv to match lhq missing ids to their prolific ids 
missing_ids <- read_csv(here("data", "raw", "lhq", "prolifictolhq.csv")) %>% 
  janitor::clean_names() %>% 
  dplyr::select(participant = 2, prolific_id = 1) %>% 
  left_join(., lhq_sub, by = "participant") %>% 
  dplyr::select(l1, l2, participant, prolific = prolific_id) 

lhq <- bind_rows(lhq_main, missing_ids) %>%
   filter(nchar(as.character(prolific)) == 24) %>% 
  dplyr::select(l1, l2, participant = prolific)


# How many of the participants we need are missing from the lhq dataframe?
sum(!(ids_needed$all %in% lhq$participant))
sum(!(ids_needed$en_sp %in% lhq$participant))
sum(!(ids_needed$en_sp_hu %in% lhq$participant))
sum(!(ids_needed$en_sp_fr %in% lhq$participant))

# 3 participants with no language info> 
# "5f00962ef7b0241ae371d912" <- Spanish L1, Hungarian l3 
# "5f3050933b5a04118e4a4f73" <- Spanish L1, Hungarian l3  
# "5f6916f985575a0f22ec8f69" <- Spanish L1, Hungarian l3 

# -----------------------------------------------------------------------------
no_id <- c("5f00962ef7b0241ae371d912","5f3050933b5a04118e4a4f73","5f6916f985575a0f22ec8f69")

no_id_df <- bind_rows(en, sp, fr, hu) %>% 
  dplyr::select(participant, exp, language, step_cont, response) %>% 
  filter(!is.na(response), participant %in% ids_needed$all) %>% 
  filter(participant == "5f00962ef7b0241ae371d912"| participant == "5f3050933b5a04118e4a4f73"| participant == "5f6916f985575a0f22ec8f69") %>% 
  mutate(l1 = "Spanish", l2 = "English")


# Tidy and prep data frames ---------------------------------------------------

# Combine 4 tasks and clean up columns
# Filter out first 2 rows of each participant (response == NA)
# Filter out participants that didn't compete all tasks 
# Add L1, L2
all_data <- bind_rows(en, sp, fr, hu) %>% 
  dplyr::select(participant, exp, language, step_cont, response) %>% 
  filter(!is.na(response), participant %in% ids_needed$all) %>% 
  left_join(., lhq, by = "participant") %>% 
  filter(!(participant %in% no_id)) %>% 
  rbind(., no_id_df) %>% 
  write_csv(here("data", "tidy", "all_2afc.csv"))

# -----------------------------------------------------------------------------
