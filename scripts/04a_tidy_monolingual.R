# 04a_tidy_monolingual  


# Source libs -----------------------------------------------------------------

source(here::here("scripts", "00_libs.R"))
source(here("scripts", "01_helpers.R"))

# -----------------------------------------------------------------------------
# get eligible participants 

mono_list <- read.csv(here("data", "tidy", "completed_mono.csv"))


# find all Textgrid files under "participant_uploads"
list_of_files <- list.files(path = here("data", "all_uploads_mono"), recursive = TRUE,
                            pattern = "\\.TextGrid$", 
                            full.names = TRUE) %>% 
  as.data.frame()



df2 <- character()

for (iteration in 1:nrow(list_of_files)) {
  df <- read_textgrid(list_of_files$.[iteration]) %>% 
    add_words() %>% 
    mutate(file = list_of_files$.[iteration])
  df2 <- rbind(df, df2)
}


spanish_list <- c("tiro","tema","talla","quiso","queja", "cama", "piso","pena","pato")
french_list <- c("tir","terre","tasse","quitte","quelle","pile","pere","patte")


tidy_df <- df2 %>%
  mutate(file = str_remove(file, 
                           "/Users/kyleparrish/Documents/GitHub/l3_production_study/data/all_uploads_mono/")) %>% 
  separate(file, into = c("participant", "other", sep = "/")) %>% 
  mutate(vot = xmax - xmin) %>%
  mutate(relative_vot = vot/duration) %>% 
  dplyr::select(participant, text, tier_name, word, duration, vot, relative_vot) %>% 
  filter(text == "t" | text == "p" | text == "k") %>% 
  mutate(language = case_when(word %in% spanish_list ~ "spanish", 
                              word %in% french_list ~ "french")) %>% 
  filter(!(text == "t" & word == "quitte")) %>% 
  filter(!(text == "t" & word == "patte")) %>% 
  filter(!(text == "t" & word == "pato")) %>% 
  mutate(vot_ms = vot*1000) %>% 
  filter(participant %in% mono_list$participant)


