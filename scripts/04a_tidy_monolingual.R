# Source libs -----------------------------------------------------------------

source(here::here("scripts", "00_libs.R"))
source(here("scripts", "01_helpers.R"))


## load all monolingual participants 

list_of_files_m <- list.files(path = here("data", "mono_uploads"), recursive = TRUE,
                              pattern = "\\.TextGrid$", 
                              full.names = TRUE) %>% 
  as.data.frame()

df3 <- character()

for (iteration in 1:nrow(list_of_files_m)) {
  df <- read_textgrid(list_of_files_m$.[iteration]) %>% 
    add_words() %>% 
    mutate(file = list_of_files_m$.[iteration])
  df3 <- rbind(df, df3)
}

spanish_list <- c("tiro","tema","talla","quiso","queja", "cama", "piso","pena","pato")
french_list <- c("tir","terre","tasse","quitte","quelle","pile","pere","patte")


tidy_mono <- df3 %>%
  mutate(file = str_remove(file, 
                           "/Users/kyleparrish/Documents/GitHub/l3_production_study/data/mono_uploads/")) %>% 
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
  mutate(vot_ms = vot*1000)

# a subset of usable data (clear quality, no or little backgroud noise)
mono_participants <- c(324130, 324140, 324149, 324151, 324154,
                       324155, 324157, 324164, 324165, 324166,
                       324171, 324178, 324183, 324185, 324192,
                       324219, 324225, 324249)


mono_df <- tidy_mono %>% 
  filter(participant %in% mono_participants) %>% 
  filter(!is.na(language))

mono_df %>% 
  write.csv(here("data", "tidy", "mono_df.csv"))





